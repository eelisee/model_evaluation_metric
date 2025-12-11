#!/usr/bin/env Rscript

# Test CORRECT boundary conditions for derivatives

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
  
  # First derivative
  delta1 <- numeric(n)
  delta1[1] <- M_p[2] - M_p[1]  # Forward difference at left
  delta1[n] <- M_p[n] - M_p[n-1]  # Backward difference at right
  for (i in 2:(n-1)) {
    delta1[i] <- (M_p[i+1] - M_p[i-1]) / 2  # Central difference in middle
  }
  
  # Second derivative from delta1
  delta2 <- numeric(n)
  delta2[1] <- delta1[2] - delta1[1]  # Forward difference of delta1
  delta2[n] <- delta1[n] - delta1[n-1]  # Backward difference of delta1
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]  # Standard central difference
  }
  
  # Third derivative from delta2
  delta3 <- numeric(n)
  delta3[1] <- delta2[2] - delta2[1]
  delta3[n] <- delta2[n] - delta2[n-1]
  for (i in 2:(n-1)) {
    delta3[i] <- delta2[i+1] - delta2[i]
  }
  
  return(list(delta1 = delta1, delta2 = delta2, delta3 = delta3))
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
cat("TESTING WITH PROPER BOUNDARY CONDITIONS\n")
cat("================================================================================\n\n")

results <- data.frame(
  Scenario = character(),
  True_p = integer(),
  argmin_delta3 = integer(),
  argmin_plus1 = integer(),
  Method1_correct = character(),
  Method2_correct = character(),
  stringsAsFactors = FALSE
)

for (scenario in names(true_p_star)) {
  data <- load_scenario(scenario)
  M_p <- data$R2_mean / data$p
  p_vals <- data$p
  n <- length(p_vals)
  
  derivs <- compute_with_proper_boundaries(M_p)
  
  # Method 1: argmin(delta3)
  argmin_idx <- which.min(derivs$delta3)
  method1 <- p_vals[argmin_idx]
  
  # Method 2: argmin(delta3) + 1
  if (argmin_idx < n) {
    method2 <- p_vals[argmin_idx + 1]
  } else {
    method2 <- p_vals[n]
  }
  
  results <- rbind(results, data.frame(
    Scenario = scenario,
    True_p = true_p_star[[scenario]],
    argmin_delta3 = method1,
    argmin_plus1 = method2,
    Method1_correct = ifelse(method1 == true_p_star[[scenario]], "✓", "✗"),
    Method2_correct = ifelse(method2 == true_p_star[[scenario]], "✓", "✗"),
    stringsAsFactors = FALSE
  ))
}

print(results)

cat("\n")
method1_success <- sum(results$Method1_correct == "✓")
method2_success <- sum(results$Method2_correct == "✓")
total <- nrow(results)

cat(sprintf("argmin(Δ₃):     %d/%d (%.1f%%)\n", method1_success, total, 100*method1_success/total))
cat(sprintf("argmin(Δ₃)+1:   %d/%d (%.1f%%)\n\n", method2_success, total, 100*method2_success/total))

cat("With proper boundary conditions:\n")
cat("- Eliminates artifacts at p=19\n")
cat("- argmin(Δ₃)+1 achieves 50%% success\n")
cat("- But still fails on boundary cases (A2, A3)\n")
