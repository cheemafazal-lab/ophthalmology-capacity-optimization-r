getwd()
list.files()

install.packages(c("readxl", "openxlsx", "lpSolve"))

library(readxl)
library(openxlsx)
library(lpSolve)

input_file <- "Primazicht--D3M-2026--A2.xlsm"
output_file <- "Primazicht_R_results.xlsx"

raw <- read_excel(input_file, sheet = "Case Data", col_names = TRUE)

dat <- raw[1:52, 1:6]
names(dat) <- c("week", "O", "P1", "P2", "P3", "capacity")

dat$week <- 1:52
dat$O <- as.numeric(dat$O)
dat$P1 <- as.numeric(dat$P1)
dat$P2 <- as.numeric(dat$P2)
dat$P3 <- as.numeric(dat$P3)
dat$capacity <- as.integer(dat$capacity)

dat$P2[is.na(dat$P2)] <- 0
dat$P3[is.na(dat$P3)] <- 0

weeks <- 52
states <- c("O", "P1", "P2", "P3")

late_cost <- c(O = 20, P1 = 3, P2 = 2, P3 = 1)

booking_cost <- c(O = 100, P = 50)
cancel_refund <- c(O = -50, P = -30)
change_cost <- c(O_to_P = -10, P_to_O = 75)

patients_per_session <- c(O = 3, P = 16)

transitions <- list(
  P1 = data.frame(
    lead = 1:8,
    O = c(0.05, 0.10, 0.00, 0.10, 0.00, 0.05, 0.00, 0.00),
    P2 = c(0.00, 0.10, 0.00, 0.10, 0.00, 0.05, 0.00, 0.20)
  ),
  O = data.frame(
    lead = 1:8,
    P2 = c(0.10, 0.20, 0.00, 0.40, 0.00, 0.20, 0.00, 0.10)
  ),
  P2 = data.frame(
    lead = 1:8,
    O = c(0.00, 0.10, 0.00, 0.05, 0.00, 0.00, 0.00, 0.00),
    P3 = c(0.00, 0.00, 0.00, 0.25, 0.00, 0.20, 0.00, 0.10)
  ),
  P3 = data.frame(
    lead = 1:8,
    P3 = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.10, 0.00, 0.20)
  )
)

make_empty_queues <- function() {
  q <- list()
  for (s in states) {
    q[[s]] <- data.frame(due_week = integer(), patients = numeric())
  }
  q
}

add_to_queue <- function(q, state, due_week, patients) {
  if (is.na(patients) || patients <= 1e-9 || due_week > weeks + 8) return(q)
  new_row <- data.frame(due_week = as.integer(due_week), patients = as.numeric(patients))
  q[[state]] <- rbind(q[[state]], new_row)
  q[[state]] <- aggregate(patients ~ due_week, data = q[[state]], sum)
  q
}

remove_from_queue <- function(q, state, due_week, patients) {
  idx <- which(q[[state]]$due_week == due_week)
  if (length(idx) == 0) return(q)
  q[[state]]$patients[idx] <- q[[state]]$patients[idx] - patients
  q[[state]] <- q[[state]][q[[state]]$patients > 1e-9, , drop = FALSE]
  q
}

available_patients <- function(q, state, week) {
  x <- q[[state]]
  if (nrow(x) == 0) return(data.frame(due_week = integer(), patients = numeric()))
  x <- x[x$due_week <= week + 1, , drop = FALSE]
  x <- x[order(x$due_week), , drop = FALSE]
  x
}

serve_state <- function(q, state, week, capacity_patients) {
  served <- data.frame(
    state = character(),
    due_week = integer(),
    patients = numeric(),
    lateness = numeric(),
    stringsAsFactors = FALSE
  )
  
  remaining <- capacity_patients
  candidates <- available_patients(q, state, week)
  
  if (nrow(candidates) == 0 || remaining <= 1e-9) {
    return(list(q = q, served = served))
  }
  
  for (i in seq_len(nrow(candidates))) {
    if (remaining <= 1e-9) break
    
    due <- candidates$due_week[i]
    amount <- min(candidates$patients[i], remaining)
    
    q <- remove_from_queue(q, state, due, amount)
    remaining <- remaining - amount
    
    served <- rbind(
      served,
      data.frame(
        state = state,
        due_week = due,
        patients = amount,
        lateness = max(0, week - due),
        stringsAsFactors = FALSE
      )
    )
  }
  
  list(q = q, served = served)
}

apply_transitions <- function(q, served, week) {
  if (nrow(served) == 0) return(q)
  
  for (i in seq_len(nrow(served))) {
    s <- served$state[i]
    n <- served$patients[i]
    tr <- transitions[[s]]
    
    if (is.null(tr)) next
    
    next_states <- setdiff(names(tr), "lead")
    
    for (ns in next_states) {
      for (j in seq_len(nrow(tr))) {
        prob <- tr[[ns]][j]
        if (!is.na(prob) && prob > 0) {
          q <- add_to_queue(q, ns, week + tr$lead[j], n * prob)
        }
      }
    }
  }
  
  q
}

calculate_backlog <- function(q, state, week) {
  x <- q[[state]]
  if (nrow(x) == 0) return(0)
  sum(x$patients[x$due_week < week])
}

get_demand_pressure <- function(q, week) {
  o_demand <- sum(available_patients(q, "O", week)$patients)
  p1_demand <- sum(available_patients(q, "P1", week)$patients)
  p2_demand <- sum(available_patients(q, "P2", week)$patients)
  p3_demand <- sum(available_patients(q, "P3", week)$patients)
  
  c(O = o_demand, P1 = p1_demand, P2 = p2_demand, P3 = p3_demand, P = p1_demand + p2_demand + p3_demand)
}

adjust_sessions <- function(booked_P, booked_O, q, week, total_capacity) {
  pressure <- get_demand_pressure(q, week)
  
  best <- NULL
  best_score <- Inf
  
  for (actual_P in 0:total_capacity) {
    actual_O <- total_capacity - actual_P
    
    cancelled_P <- max(0, booked_P - actual_P)
    cancelled_O <- max(0, booked_O - actual_O)
    changed_O_to_P <- max(0, actual_P - booked_P)
    changed_P_to_O <- max(0, actual_O - booked_O)
    
    p_short <- max(0, pressure["P"] - actual_P * patients_per_session["P"])
    o_short <- max(0, pressure["O"] - actual_O * patients_per_session["O"])
    p_unused <- max(0, actual_P * patients_per_session["P"] - pressure["P"])
    o_unused <- max(0, actual_O * patients_per_session["O"] - pressure["O"])
    
    score <- 20 * o_short +
      3 * p_short +
      0.25 * p_unused +
      0.5 * o_unused +
      cancelled_P * cancel_refund["P"] +
      cancelled_O * cancel_refund["O"] +
      changed_O_to_P * change_cost["O_to_P"] +
      changed_P_to_O * change_cost["P_to_O"]
    
    if (score < best_score) {
      best_score <- score
      best <- list(
        actual_P = actual_P,
        actual_O = actual_O,
        cancelled_P = cancelled_P,
        cancelled_O = cancelled_O,
        changed_O_to_P = changed_O_to_P,
        changed_P_to_O = changed_P_to_O
      )
    }
  }
  
  best
}

simulate_policy <- function(booked_P, booked_O, method_name) {
  q <- make_empty_queues()
  results <- data.frame()
  total_cost <- 0
  
  for (t in 1:weeks) {
    q <- add_to_queue(q, "O", t, dat$O[t])
    q <- add_to_queue(q, "P1", t, dat$P1[t])
    
    if (dat$P2[t] > 0) q <- add_to_queue(q, "P2", t, dat$P2[t])
    if (dat$P3[t] > 0) q <- add_to_queue(q, "P3", t, dat$P3[t])
    
    adj <- adjust_sessions(booked_P[t], booked_O[t], q, t, dat$capacity[t])
    
    session_cost <- booked_O[t] * booking_cost["O"] +
      booked_P[t] * booking_cost["P"] +
      adj$cancelled_O * cancel_refund["O"] +
      adj$cancelled_P * cancel_refund["P"] +
      adj$changed_O_to_P * change_cost["O_to_P"] +
      adj$changed_P_to_O * change_cost["P_to_O"]
    
    served_week <- data.frame()
    
    out <- serve_state(q, "O", t, adj$actual_O * patients_per_session["O"])
    q <- out$q
    served_week <- rbind(served_week, out$served)
    
    p_capacity <- adj$actual_P * patients_per_session["P"]
    
    for (ps in c("P1", "P2", "P3")) {
      out <- serve_state(q, ps, t, p_capacity)
      q <- out$q
      served_week <- rbind(served_week, out$served)
      p_capacity <- p_capacity - sum(out$served$patients)
    }
    
    lateness_cost <- 0
    
    if (nrow(served_week) > 0) {
      lateness_cost <- sum(
        late_cost[served_week$state] *
          served_week$lateness^2 *
          served_week$patients
      )
    }
    
    q <- apply_transitions(q, served_week, t)
    
    week_cost <- session_cost + lateness_cost
    total_cost <- total_cost + week_cost
    
    results <- rbind(
      results,
      data.frame(
        method = method_name,
        week = t,
        capacity_sessions = dat$capacity[t],
        booked_P = booked_P[t],
        booked_O = booked_O[t],
        actual_P = adj$actual_P,
        actual_O = adj$actual_O,
        cancelled_P = adj$cancelled_P,
        cancelled_O = adj$cancelled_O,
        changed_O_to_P = adj$changed_O_to_P,
        changed_P_to_O = adj$changed_P_to_O,
        treated_O = ifelse(nrow(served_week) == 0, 0, sum(served_week$patients[served_week$state == "O"])),
        treated_P1 = ifelse(nrow(served_week) == 0, 0, sum(served_week$patients[served_week$state == "P1"])),
        treated_P2 = ifelse(nrow(served_week) == 0, 0, sum(served_week$patients[served_week$state == "P2"])),
        treated_P3 = ifelse(nrow(served_week) == 0, 0, sum(served_week$patients[served_week$state == "P3"])),
        delayed_O = calculate_backlog(q, "O", t),
        delayed_P1 = calculate_backlog(q, "P1", t),
        delayed_P2 = calculate_backlog(q, "P2", t),
        delayed_P3 = calculate_backlog(q, "P3", t),
        session_cost = session_cost,
        lateness_cost = lateness_cost,
        total_week_cost = week_cost,
        cumulative_cost = total_cost
      )
    )
  }
  
  list(results = results, total_cost = total_cost)
}

solve_lp_mcdm_week <- function(expected_O, expected_P, capacity_sessions) {
  objective <- c(
    booking_cost["P"],
    booking_cost["O"],
    3,
    20,
    0.25,
    0.50
  )
  
  constraint_matrix <- rbind(
    c(1, 1, 0, 0, 0, 0),
    c(patients_per_session["P"], 0, 1, 0, -1, 0),
    c(0, patients_per_session["O"], 0, 1, 0, -1)
  )
  
  directions <- c("<=", "=", "=")
  rhs <- c(capacity_sessions, expected_P, expected_O)
  
  solution <- lp(
    direction = "min",
    objective.in = objective,
    const.mat = constraint_matrix,
    const.dir = directions,
    const.rhs = rhs,
    int.vec = c(1, 2),
    all.int = FALSE
  )
  
  if (solution$status != 0) {
    p_sessions <- round(capacity_sessions * expected_P / max(1, expected_P + expected_O))
    p_sessions <- max(0, min(capacity_sessions, p_sessions))
    return(p_sessions)
  }
  
  p_sessions <- round(solution$solution[1])
  p_sessions <- max(0, min(capacity_sessions, p_sessions))
  p_sessions
}

make_lp_mcdm_booking <- function() {
  q <- make_empty_queues()
  booked_P <- rep(0, weeks)
  booked_O <- rep(0, weeks)
  
  for (t in 1:weeks) {
    q <- add_to_queue(q, "O", t, dat$O[t])
    q <- add_to_queue(q, "P1", t, dat$P1[t])
    
    if (dat$P2[t] > 0) q <- add_to_queue(q, "P2", t, dat$P2[t])
    if (dat$P3[t] > 0) q <- add_to_queue(q, "P3", t, dat$P3[t])
    
    target_week <- t + 2
    
    if (target_week <= weeks) {
      pressure <- get_demand_pressure(q, t)
      
      expected_O <- pressure["O"] + dat$O[target_week]
      expected_P <- pressure["P"] + dat$P1[target_week] + dat$P2[target_week] + dat$P3[target_week]
      
      booked_P[target_week] <- solve_lp_mcdm_week(expected_O, expected_P, dat$capacity[target_week])
      booked_O[target_week] <- dat$capacity[target_week] - booked_P[target_week]
    }
    
    if (t <= 2 && booked_P[t] + booked_O[t] == 0) {
      pressure <- get_demand_pressure(q, t)
      booked_P[t] <- solve_lp_mcdm_week(pressure["O"], pressure["P"], dat$capacity[t])
      booked_O[t] <- dat$capacity[t] - booked_P[t]
    }
    
    adj <- adjust_sessions(booked_P[t], booked_O[t], q, t, dat$capacity[t])
    
    served_week <- data.frame()
    
    out <- serve_state(q, "O", t, adj$actual_O * patients_per_session["O"])
    q <- out$q
    served_week <- rbind(served_week, out$served)
    
    p_capacity <- adj$actual_P * patients_per_session["P"]
    
    for (ps in c("P1", "P2", "P3")) {
      out <- serve_state(q, ps, t, p_capacity)
      q <- out$q
      served_week <- rbind(served_week, out$served)
      p_capacity <- p_capacity - sum(out$served$patients)
    }
    
    q <- apply_transitions(q, served_week, t)
  }
  
  list(booked_P = booked_P, booked_O = booked_O)
}

booking_m1 <- make_lp_mcdm_booking()

method1 <- simulate_policy(
  booked_P = booking_m1$booked_P,
  booked_O = booking_m1$booked_O,
  method_name = "Method 1 - LP/MCDM"
)

summary_m1 <- data.frame(
  method = "Method 1 - LP/MCDM",
  total_cost = method1$total_cost,
  total_session_cost = sum(method1$results$session_cost),
  total_lateness_cost = sum(method1$results$lateness_cost),
  total_treated_O = sum(method1$results$treated_O),
  total_treated_P1 = sum(method1$results$treated_P1),
  total_treated_P2 = sum(method1$results$treated_P2),
  total_treated_P3 = sum(method1$results$treated_P3)
)

print(summary_m1)

wb <- createWorkbook()

addWorksheet(wb, "Method_1_LP_MCDM")
addWorksheet(wb, "Summary_Method_1")

writeData(wb, "Method_1_LP_MCDM", method1$results)
writeData(wb, "Summary_Method_1", summary_m1)

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("\nMethod 1 complete. Excel file created:", output_file, "\n")