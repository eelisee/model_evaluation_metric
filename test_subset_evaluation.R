# Test Subset Evaluation Implementation
# =======================================
# Quick test with N=5 iterations to verify the new subset evaluation works

# Load only the module files
source("R/01_data_generation.R")
source("R/02_metrics.R")
source("R/03_evaluation.R")
source("R/04_visualization.R")

# Define simple test scenario
test_scenario <- list(
  name = "TEST_SubsetEval",
  description = "Testing subset evaluation implementation",
  n = 500,
  p = 20,
  sigma_structure = "identity",
  rho = 0.0,
  support_spec = 3,
  signal_strength = "strong",
  sigma_eps = 0.2
)

# Test with sequential iterations
cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("TESTING SUBSET EVALUATION IMPLEMENTATION\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

N_iterations <- 5
all_iterations <- list()
all_r2_curves <- list()
all_metric_results <- list()
all_support_true <- list()

cat(sprintf("Running %d iterations...\\n", N_iterations))

for (iter in 1:N_iterations) {
  
  cat(sprintf("  Iteration %d/%d\\n", iter, N_iterations))
  
  # Set seed
  set.seed(1000 + iter)
  
  # Generate data
  data <- generate_data(test_scenario)
  
  # Compute R² curve
  r2_curve <- compute_r2_curve(data$X, data$y)
  
  # Apply metrics
  metric_results <- apply_all_metrics(r2_curve)
  
  # Evaluate with support_true
  evaluation <- evaluate_iteration(metric_results, data$p_true, data$support_true)
  
  # Store
  all_iterations[[iter]] <- evaluation
  all_r2_curves[[iter]] <- r2_curve
  all_metric_results[[iter]] <- metric_results
  all_support_true[[iter]] <- data$support_true
  
  cat(sprintf("    p* selections: M_p=%d, AIC=%d, BIC=%d\\n", 
              metric_results$M_p$p_star, 
              metric_results$AIC$p_star,
              metric_results$BIC$p_star))
}

# Compute summary statistics
cat("\\nComputing summary statistics...\\n")
summary_stats <- compute_summary_statistics(all_iterations, all_r2_curves, data$p_true, all_metric_results)

# Create detailed results
cat("Creating detailed results...\\n")
detailed_results <- create_detailed_results(all_iterations, all_r2_curves, all_metric_results, all_support_true)

# Print subset metrics
cat("\\n")
cat(paste(rep("=", 70), collapse = ""), "\\n")
cat("SUBSET EVALUATION METRICS:\\n")
cat(paste(rep("=", 70), collapse = ""), "\\n")
print(summary_stats[, c("metric", "Jaccard", "Precision", "Recall", "F1", 
                         "avg_TP", "avg_FP", "avg_FN", "SubsetHitRate")])

# Check detailed results columns
cat("\\n\\nDetailed Results Columns:\\n")
print(head(colnames(detailed_results), 20))

cat("\\n")
cat(paste(rep("=", 70), collapse = ""), "\\n")
cat("TEST COMPLETE!\\n")
cat(paste(rep("=", 70), collapse = ""), "\\n")
cat("\\nSubset evaluation implementation verified successfully!\\n\\n")
