#!/usr/bin/env Rscript
# Test sigmoid fits for A2 scenario

source("R/01_data_generation.R")
source("R/02_metrics.R")

# Load A2 results
results <- read.csv('results/A2_Single_Predictor/detailed_results.csv')

# Average M_p values
p_vals <- 1:20
avg_mp <- numeric(20)
for (p in p_vals) {
  avg_mp[p] <- mean(results[results$p == p, 'M_p'])
}

cat("=== A2 SCENARIO: Single Predictor (p*=1) ===\n\n")
cat("Average M_p curve:\n")
for (i in 1:5) {
  cat(sprintf("p=%d: %.4f\n", i, avg_mp[i]))
}

cat("\n=== Declines ===\n")
for (i in 1:4) {
  decline <- avg_mp[i] - avg_mp[i+1]
  cat(sprintf("p=%d to p=%d: %.6f\n", i, i+1, decline))
}

# Find steepest decline
delta1 <- diff(avg_mp)
steepest_idx <- which.min(delta1)
cat(sprintf("\nSteepest decline: p=%d to p=%d (%.6f)\n", 
            steepest_idx, steepest_idx+1, delta1[steepest_idx]))

# Test sigmoid fit on averaged data
cat("\n=== Testing sigmoid fit on averaged data ===\n")
r2_avg <- avg_mp * p_vals
avg_r2_curve <- data.frame(
  p = p_vals,
  R2 = r2_avg,
  subset_Mp = NA
)

sigmoid_result <- metric_sigmoid_mp(avg_r2_curve)
cat(sprintf("Fitted delta: %.2f\n", sigmoid_result$delta))
cat(sprintf("Selected p*: %d\n", sigmoid_result$p_star))

# Check individual iterations
cat("\n=== Individual iteration fits ===\n")
for (iter in 1:3) {
  iter_data <- results[results$iteration == iter, ]
  r2_curve <- data.frame(
    p = iter_data$p,
    R2 = iter_data$R2,
    subset_Mp = NA
  )
  
  result <- metric_sigmoid_mp(r2_curve)
  cat(sprintf("Iter %d: delta=%.2f, p*=%d\n", iter, result$delta, result$p_star))
}

# The key question: Why is steepest decline at p=1→2 but p*=3 selected?
cat("\n=== Analysis ===\n")
cat("The HUGE drop from p=1 to p=2 (0.474) is because:\n")
cat("  - p=1: Only 1 predictor included (R²=0.949)\n")
cat("  - p=2: 2 predictors (R²/2 = 0.475)\n")
cat("\nThis massive drop should put the inflection at p=1!\n")
cat("But sigmoid might be fitting poorly due to:\n")
cat("  1. The drop is TOO steep (not smooth S-curve)\n")
cat("  2. After p=2, it looks like gradual decay\n")
cat("  3. Sigmoid optimizer might be 'averaging' the behavior\n")
