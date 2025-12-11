#!/usr/bin/env Rscript

# Pure derivative-based solution with boundary checks

load_scenario <- function(scenario_name) {
  file_path <- sprintf("results/%s/detailed_results.csv", scenario_name)
  data <- read.csv(file_path)
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  names(avg_data)[2] <- "R2_mean"
  avg_data <- avg_data[order(avg_data$p), ]
  return(avg_data)
}

compute_with_proper_boundaries <- function(M_p) {
  n <- length(M_p)
  
  delta1 <- numeric(n)
  delta1[1] <- M_p[2] - M_p[1]
  delta1[n] <- M_p[n] - M_p[n-1]
  for (i in 2:(n-1)) {
    delta1[i] <- (M_p[i+1] - M_p[i-1]) / 2
  }
  
  delta2 <- numeric(n)
  delta2[1] <- delta1[2] - delta1[1]
  delta2[n] <- delta1[n] - delta1[n-1]
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
  }
  
  delta3 <- numeric(n)
  delta3[1] <- delta2[2] - delta2[1]
  delta3[n] <- delta2[n] - delta2[n-1]
  for (i in 2:(n-1)) {
    delta3[i] <- delta2[i+1] - delta2[i]
  }
  
  return(list(delta1 = delta1, delta2 = delta2, delta3 = delta3))
}

select_p_pure_derivatives <- function(M_p, p_vals) {
  n <- length(M_p)
  derivs <- compute_with_proper_boundaries(M_p)
  
  # Find argmin(Δ₃)
  argmin_idx <- which.min(derivs$delta3)
  
  # If argmin(Δ₃) is at p=1, check if p=1 is the optimum
  if (argmin_idx == 1) {
    # p=1 is optimum if M_p[1] significantly larger than M_p[2]
    # Use relative drop: (M_p[1] - M_p[2]) / M_p[1]
    rel_drop <- (M_p[1] - M_p[2]) / M_p[1]
    if (rel_drop > 0.4) {  # 40% drop indicates strong maximum at p=1
      return(list(p_star = p_vals[1], method = "left_boundary"))
    }
  }
  
  # If argmin(Δ₃) is at p=n or p=n-1, check if p=n is the optimum
  if (argmin_idx >= n-1) {
    # p=n is optimum if Δ₁[n] is very small (slow decline)
    # AND Δ₂[n] is near zero (linear behavior)
    if (abs(derivs$delta1[n]) < 0.005 && abs(derivs$delta2[n]) < 0.001) {
      return(list(p_star = p_vals[n], method = "right_boundary"))
    }
  }
  
  # Interior optimum: argmin(Δ₃) + 1
  if (argmin_idx < n) {
    return(list(p_star = p_vals[argmin_idx + 1], method = "interior_inflection"))
  } else {
    return(list(p_star = p_vals[n], method = "fallback"))
  }
}

true_p_star <- list(
  "A1_Baseline_Uncorrelated" = 3,
  "A2_Single_Predictor" = 1,
  "A3_Full_Support" = 20,
  "B1_AR1_Weak" = 3,
  "B1_AR1_Strong" = 3,
  "B2_Compound_Symmetry" = 3,
  "B3_Block_Structure" = 3,
  "C1_Weak_Signals" = 5,
  "C2_Many_Weak_Signals" = 10,
  "C3_Mixed_Signals" = 8
)

cat("================================================================================\n")
cat("PURE DERIVATIVE SOLUTION WITH BOUNDARY CHECKS\n")
cat("================================================================================\n\n")

cat("Rules:\n")
cat("1. If argmin(Δ₃)=1 AND (M_p[1]-M_p[2])/M_p[1] > 0.4  → p*=1\n")
cat("2. If argmin(Δ₃)≥n-1 AND |Δ₁[n]|<0.005 AND |Δ₂[n]|<0.001  → p*=n\n")
cat("3. Else  → p* = argmin(Δ₃)+1\n\n")

results <- data.frame(
  Scenario = character(),
  True_p = integer(),
  Selected_p = integer(),
  Method = character(),
  Correct = character(),
  stringsAsFactors = FALSE
)

for (scenario in names(true_p_star)) {
  data <- load_scenario(scenario)
  M_p <- data$R2_mean / data$p
  
  result <- select_p_pure_derivatives(M_p, data$p)
  correct <- result$p_star == true_p_star[[scenario]]
  
  results <- rbind(results, data.frame(
    Scenario = scenario,
    True_p = true_p_star[[scenario]],
    Selected_p = result$p_star,
    Method = result$method,
    Correct = ifelse(correct, "✓", "✗"),
    stringsAsFactors = FALSE
  ))
}

print(results)

cat("\n")
success_rate <- sum(results$Correct == "✓") / nrow(results)
cat(sprintf("Success rate: %d/%d (%.1f%%)\n", 
            sum(results$Correct == "✓"), 
            nrow(results), 
            100 * success_rate))
