#!/usr/bin/env Rscript

# Test using PROPER constrained optimization conditions

load_scenario <- function(scenario_name) {
  file_path <- sprintf("results/%s/detailed_results.csv", scenario_name)
  data <- read.csv(file_path)
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  names(avg_data)[2] <- "R2_mean"
  avg_data <- avg_data[order(avg_data$p), ]
  return(avg_data)
}

compute_derivatives_proper <- function(M_p) {
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

select_p_star <- function(M_p, R2_vals, p_vals) {
  n <- length(M_p)
  derivs <- compute_derivatives_proper(M_p)
  
  # Check for RIGHT BOUNDARY optimum (p=n) FIRST
  # Condition: R² near 1 (nearly perfect fit)
  if (R2_vals[n] > 0.99) {
    return(list(p_star = p_vals[n], method = "right_boundary_R2"))
  }
  
  # Check for LEFT BOUNDARY optimum (p=1)
  # Condition: argmax(Δ₂) = 1 (maximum concavity at left boundary)
  if (which.max(derivs$delta2) == 1) {
    return(list(p_star = p_vals[1], method = "left_boundary_delta2"))
  }
  
  # INTERIOR optimum: use third derivative
  # Find inflection point where Δ₃ crosses from negative to positive
  for (i in 1:(n-1)) {
    if (derivs$delta3[i] < 0 && derivs$delta3[i+1] > 0) {
      return(list(p_star = p_vals[i+1], method = "delta3_zero_crossing"))
    }
  }
  
  # Fallback: argmin(Δ₃) + 1
  idx <- which.min(derivs$delta3)
  if (idx < n) {
    return(list(p_star = p_vals[idx + 1], method = "argmin_delta3_plus1"))
  } else {
    return(list(p_star = p_vals[n], method = "fallback_to_n"))
  }
}

# Test all scenarios
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
cat("TESTING CONSTRAINED OPTIMIZATION FORMULATION\n")
cat("================================================================================\n\n")

cat("Mathematical conditions:\n")
cat("1. RIGHT boundary (p=n): R²[n] > 0.99\n")
cat("2. LEFT boundary (p=1):  argmax(Δ₂) = 1\n")
cat("3. INTERIOR:             Δ₃ zero crossing or argmin(Δ₃) + 1\n\n")

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
  
  result <- select_p_star(M_p, data$R2_mean, data$p)
  
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

# Show which method was used
cat("\nMethods used:\n")
print(table(results$Method))
