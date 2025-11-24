#!/usr/bin/env Rscript

# Explore alternative formulations and transformations of M_p

load_scenario <- function(scenario_name) {
  file_path <- sprintf("results/%s/detailed_results.csv", scenario_name)
  data <- read.csv(file_path)
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  names(avg_data)[2] <- "R2_mean"
  avg_data <- avg_data[order(avg_data$p), ]
  return(avg_data)
}

cat("================================================================================\n")
cat("EXPLORING M_p AND ITS TRANSFORMATIONS\n")
cat("================================================================================\n")

scenarios <- list(
  "A1_Baseline_Uncorrelated" = 3,
  "A2_Single_Predictor" = 1,
  "A3_Full_Support" = 20
)

for (scenario in names(scenarios)) {
  true_p <- scenarios[[scenario]]
  cat(sprintf("\n=== %s (p*=%d) ===\n", scenario, true_p))
  
  data <- load_scenario(scenario)
  p_vals <- data$p
  R2_vals <- data$R2_mean
  M_p <- R2_vals / p_vals
  n <- length(p_vals)
  
  # Original M_p
  cat("\nOriginal M_p (first 5 values):\n")
  print(round(M_p[1:5], 4))
  
  # Log transform
  log_M_p <- log(M_p)
  cat("\nlog(M_p) (first 5 values):\n")
  print(round(log_M_p[1:5], 4))
  
  # Relative decrease (percentage change)
  rel_change <- numeric(n-1)
  for (i in 1:(n-1)) {
    rel_change[i] <- (M_p[i+1] - M_p[i]) / M_p[i]
  }
  cat("\nRelative change in M_p (first 5 values):\n")
  print(round(rel_change[1:5], 4))
  
  # Second derivative of log(M_p)
  delta2_log <- numeric(n)
  delta2_log[1] <- log_M_p[1] - 2*log_M_p[2] + log_M_p[3]
  delta2_log[n] <- log_M_p[n-2] - 2*log_M_p[n-1] + log_M_p[n]
  for (i in 2:(n-1)) {
    delta2_log[i] <- log_M_p[i-1] - 2*log_M_p[i] + log_M_p[i+1]
  }
  cat("\nΔ₂[log(M_p)] (first 5 values):\n")
  print(round(delta2_log[1:5], 4))
  
  # Which formulation shows a clear signal at p*?
  cat(sprintf("\nDetection at p*=%d:\n", true_p))
  cat(sprintf("  M_p[%d] = %.4f\n", true_p, M_p[true_p]))
  cat(sprintf("  log(M_p)[%d] = %.4f\n", true_p, log_M_p[true_p]))
  if (true_p > 1) {
    cat(sprintf("  rel_change[%d] = %.4f\n", true_p-1, rel_change[true_p-1]))
  }
  cat(sprintf("  Δ₂[log(M_p)][%d] = %.4f\n", true_p, delta2_log[true_p]))
}

cat("\n\n================================================================================\n")
cat("KEY QUESTION\n")
cat("================================================================================\n\n")

cat("The problem might be that M_p = R²/p is not the right metric!\n\n")

cat("Consider: R²/p is meant to penalize using too many predictors, but:\n")
cat("- It creates a strong bias toward p=1 (division by small number)\n")
cat("- The 'true' elbow might be in a DIFFERENT metric\n\n")

cat("Alternative formulations to consider:\n")
cat("1. M_p = R² - λ*p  (linear penalty)\n")
cat("2. M_p = R² / log(p+1)  (logarithmic penalty)\n")
cat("3. M_p = R² - λ*sqrt(p)  (sqrt penalty)\n")
cat("4. Find elbow in R² curve directly, not in R²/p\n\n")

cat("Should we reconsider the DEFINITION of M_p itself?\n")
