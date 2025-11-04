# Main Analysis Script for M_p Model Evaluation
# ==============================================
# This script runs the complete M_p analysis pipeline from data generation
# through model evaluation to visualization.

# Clear workspace
rm(list = ls())

# Set seed for reproducibility
set.seed(123)

# Load required libraries
if (!require("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Source functions
source("R/mp_functions.R")
source("R/visualization.R")

# ============================================================================
# CONFIGURATION
# ============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("M_p Model Evaluation Metric - Main Analysis\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Data generation parameters
config <- list(
  n = 100,                              # Number of observations
  p_max = 10,                           # Maximum number of predictors
  true_beta = c(1, 0.8, 0.5, rep(0, 7)), # True coefficients
  sigma = 0.2,                          # Noise standard deviation
  output_dir = "example_output",         # Output directory
  save_plots = TRUE,                    # Save plots to file
  save_results = TRUE                   # Save results to CSV
)

# Number of true predictors
p_true <- sum(config$true_beta != 0)

cat("Configuration:\n")
cat(sprintf("  - Sample size: n = %d\n", config$n))
cat(sprintf("  - Max predictors: p_max = %d\n", config$p_max))
cat(sprintf("  - True predictors: p_true = %d\n", p_true))
cat(sprintf("  - Noise level: sigma = %.2f\n", config$sigma))
cat(sprintf("  - Total models to evaluate: %d\n", 2^config$p_max - 1))
cat("\n")

# ============================================================================
# STEP 1: DATA GENERATION
# ============================================================================

cat("Step 1: Generating synthetic data...\n")

data <- generate_data(
  n = config$n,
  p_max = config$p_max,
  true_beta = config$true_beta,
  sigma = config$sigma
)

cat(sprintf("  Generated %d observations with %d predictors\n", 
            nrow(data$X), ncol(data$X)))
cat(sprintf("  True model: y = %.2f*X1 + %.2f*X2 + %.2f*X3 + noise\n",
            config$true_beta[1], config$true_beta[2], config$true_beta[3]))
cat("\n")

# ============================================================================
# STEP 2: MODEL EVALUATION
# ============================================================================

cat("Step 2: Evaluating all possible models...\n")

# Evaluate all models
results <- evaluate_all_models(data$X, data$y, verbose = TRUE)

cat(sprintf("  Total models evaluated: %d\n", nrow(results)))
cat(sprintf("  R² range: [%.4f, %.4f]\n", min(results$R2), max(results$R2)))
cat(sprintf("  M_p range: [%.4f, %.4f]\n", min(results$Mp), max(results$Mp)))
cat("\n")

# ============================================================================
# STEP 3: FIND OPTIMAL MODEL
# ============================================================================

cat("Step 3: Identifying optimal model...\n")

optimal_result <- find_optimal_model(results)

# Print detailed summary
print_summary(optimal_result, true_p = p_true)

# ============================================================================
# STEP 4: CALCULATE INFORMATION CRITERIA
# ============================================================================

cat("\nStep 4: Calculating AIC and BIC for comparison...\n")

results <- calculate_information_criteria(data$X, data$y, results)

# Find optimal models according to AIC and BIC
best_models_full <- find_best_models_by_p(results)
p_star_aic <- best_models_full$p[which.min(best_models_full$AIC)]
p_star_bic <- best_models_full$p[which.min(best_models_full$BIC)]

cat(sprintf("  Optimal p according to M_p: %d\n", optimal_result$p_star))
cat(sprintf("  Optimal p according to AIC: %d\n", p_star_aic))
cat(sprintf("  Optimal p according to BIC: %d\n", p_star_bic))
cat(sprintf("  True number of predictors: %d\n", p_true))
cat("\n")

# ============================================================================
# STEP 5: SAVE RESULTS
# ============================================================================

if (config$save_results) {
  cat("Step 5: Saving results...\n")
  
  # Create output directory
  if (!dir.exists(config$output_dir)) {
    dir.create(config$output_dir, recursive = TRUE)
  }
  
  # Save full results
  results_file <- file.path(config$output_dir, "all_models_results.csv")
  write.csv(results, results_file, row.names = FALSE)
  #cat(sprintf("  Full results saved to: %s\n", results_file))
  
  # Save best models by p
  best_models_file <- file.path(config$output_dir, "best_models_by_p.csv")
  write.csv(best_models_full, best_models_file, row.names = FALSE)
  #cat(sprintf("  Best models saved to: %s\n", best_models_file))
  
  # Save summary
  summary_file <- file.path(config$output_dir, "analysis_summary.txt")
  sink(summary_file)
  cat("M_p Model Selection Analysis Summary\n")
  cat("====================================\n\n")
  cat("Data Configuration:\n")
  cat(sprintf("  - Sample size: n = %d\n", config$n))
  cat(sprintf("  - Max predictors: p_max = %d\n", config$p_max))
  cat(sprintf("  - True predictors: p_true = %d\n", p_true))
  cat(sprintf("  - Noise level: sigma = %.2f\n", config$sigma))
  cat("\n")
  cat("Results:\n")
  cat(sprintf("  - Optimal p (M_p): %d\n", optimal_result$p_star))
  cat(sprintf("  - Optimal p (AIC): %d\n", p_star_aic))
  cat(sprintf("  - Optimal p (BIC): %d\n", p_star_bic))
  cat(sprintf("  - True p: %d\n", p_true))
  cat("\n")
  cat("Optimal Model Details:\n")
  cat(sprintf("  - Selected predictors: %s\n", optimal_result$optimal_model$subset_str))
  cat(sprintf("  - R²: %.4f\n", optimal_result$optimal_model$R2))
  cat(sprintf("  - M_p: %.4f\n", optimal_result$optimal_model$Mp))
  cat(sprintf("  - AIC: %.4f\n", optimal_result$optimal_model$AIC))
  cat(sprintf("  - BIC: %.4f\n", optimal_result$optimal_model$BIC))
  cat("\n")
  cat("Best Models by Cardinality:\n")
  print(best_models_full[, c("p", "R2", "Mp", "AIC", "BIC", "subset_str")])
  sink()
  cat(sprintf("  Summary and models saved. %s\n", summary_file))
  cat("\n")
}

# ============================================================================
# STEP 6: VISUALIZATION
# ============================================================================

if (config$save_plots) {
  cat("Step 6: Generating visualizations...\n")
  
  # Create plots directory
  plots_dir <- file.path(config$output_dir, "plots")
  if (!dir.exists(plots_dir)) {
    dir.create(plots_dir, recursive = TRUE)
  }
  
  # Generate all visualizations
  create_visualization_dashboard(results, optimal_result, plots_dir)
  
  # Generate comparison plot
  plot_criterion_comparison(
    results, 
    p_star_mp = optimal_result$p_star,
    save_path = file.path(plots_dir, "criterion_comparison.png")
  )
  
}

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("Analysis Complete!\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("Key Findings:\n")
cat(sprintf("  1. M_p selected p* = %d (True: %d, Difference: %+d)\n", 
            optimal_result$p_star, p_true, optimal_result$p_star - p_true))
cat(sprintf("  2. AIC selected p* = %d (True: %d, Difference: %+d)\n",
            p_star_aic, p_true, p_star_aic - p_true))
cat(sprintf("  3. BIC selected p* = %d (True: %d, Difference: %+d)\n",
            p_star_bic, p_true, p_star_bic - p_true))
cat("\n")

if (config$save_results && config$save_plots) {
  cat("Output files are in the corresponding subdirectory of the output directory\n")
  cat("\n")
}

cat("Interpretation:\n")
if (optimal_result$p_star == p_true) {
  cat("  ✓ M_p correctly identified the true model complexity!\n")
} else if (optimal_result$p_star < p_true) {
  cat("  ⚠ M_p selected a simpler model (underfitting risk)\n")
} else {
  cat("  ⚠ M_p selected a more complex model (overfitting risk)\n")
}
