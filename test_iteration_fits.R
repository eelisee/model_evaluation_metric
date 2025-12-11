#!/usr/bin/env Rscript
# Test sigmoid fits for each iteration

source("R/01_data_generation.R")
source("R/02_metrics.R")

# Load results
results <- read.csv('results/A1_Baseline_Uncorrelated/detailed_results.csv')

for (iter in 1:3) {
  cat(sprintf("\n========== ITERATION %d ==========\n", iter))
  
  # Extract M_p for this iteration
  iter_data <- results[results$iteration == iter, ]
  p_vals <- iter_data$p
  M_vals <- iter_data$M_p
  
  # Calculate declines
  delta1 <- diff(M_vals)
  steepest_idx <- which.min(delta1)
  
  cat(sprintf("Steepest decline: p=%d→%d (%.4f)\n", 
              steepest_idx, steepest_idx+1, delta1[steepest_idx]))
  cat(sprintf("delta_start = %d\n", p_vals[steepest_idx + 1]))
  
  # Create r2_curve object
  r2_curve <- data.frame(
    p = p_vals,
    R2 = iter_data$R2,
    subset_Mp = NA
  )
  
  # Fit sigmoid
  result <- metric_sigmoid_mp(r2_curve)
  
  cat(sprintf("Fitted delta: %.2f\n", result$delta))
  cat(sprintf("Selected p*: %d\n", result$p_star))
  
  # Check fit quality
  fitted <- result$fitted_curve
  rmse <- sqrt(mean((M_vals - fitted)^2))
  cat(sprintf("RMSE: %.6f\n", rmse))
}
