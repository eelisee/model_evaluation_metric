# Test Case: A1 Baseline Scenario
# =================================
# Quick manual test with small settings

# Source modules directly to avoid auto-execution
source("R/01_data_generation.R")
source("R/02_metrics.R")
source("R/03_evaluation.R")
source("R/04_visualization.R")

# Source write_summary function from run_experiment.R
source("run_experiment.R")

# Manually define run_scenario function inline
run_scenario_test <- function(scenario, N_iterations, output_dir) {
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("SCENARIO:", scenario$name, "\n")
  cat(scenario$description, "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  scenario_dir <- file.path(output_dir, scenario$name)
  dir.create(scenario_dir, showWarnings = FALSE, recursive = TRUE)
  
  all_iterations <- list()
  all_r2_curves <- list()
  all_metric_results <- list()
  
  for (iter in 1:N_iterations) {
    cat(sprintf("  [%d/%d] Iteration %d...\n", iter, N_iterations, iter))
    
    scenario$seed <- 1000 + iter
    data <- generate_data(scenario)
    cat(sprintf("      Generated: X %dx%d, true p=%d\n", 
                nrow(data$X), ncol(data$X), data$p_true))
    
    r2_curve <- compute_r2_curve(data$X, data$y)
    metric_results <- apply_all_metrics(r2_curve)
    
    cat("      M_p:", metric_results$M_p$p_star, 
        "| AIC:", metric_results$AIC$p_star,
        "| BIC:", metric_results$BIC$p_star, "\n")
    
    eval <- evaluate_iteration(metric_results, data$p_true)
    eval$iteration <- iter
    
    all_iterations[[iter]] <- eval
    all_r2_curves[[iter]] <- r2_curve
    all_metric_results[[iter]] <- metric_results
  }
  
  summary_stats <- compute_summary_statistics(all_iterations, all_r2_curves, data$p_true)
  
  create_all_plots(all_iterations, all_r2_curves, all_metric_results,
                   summary_stats, data$p_true,
                   file.path(scenario_dir, "plots"))
  
  write.csv(summary_stats, file.path(scenario_dir, "summary_stats.csv"), row.names = FALSE)
  
  # Save summary.txt
  write_summary(scenario, summary_stats, all_iterations,
                file.path(scenario_dir, "summary.txt"))
  
  # Save detailed results
  detailed_results <- create_detailed_results(all_iterations, all_r2_curves, all_metric_results)
  write.csv(detailed_results, file.path(scenario_dir, "detailed_results.csv"), row.names = FALSE)
  
  return(list(summary_stats = summary_stats, all_iterations = all_iterations, 
              detailed_results = detailed_results))
}

cat("\n")
cat("======================================================================\n")
cat("TEST CASE: A1 Baseline with p=8, N=3\n")
cat("======================================================================\n\n")

test_scenario <- list(
  name = "TEST_A1_Small",
  description = "Test: Uncorrelated predictors, p=8, N=3",
  n = 100,
  p = 8,
  sigma_structure = "identity",
  rho = 0,
  support_spec = 3,
  signal_strength = "strong",
  sigma_eps = 0.2
)

result <- run_scenario_test(test_scenario, N_iterations = 3, output_dir = "results_test")

cat("\n")
cat("======================================================================\n")
cat("Summary Statistics:\n")
cat("======================================================================\n")
print(result$summary_stats)

cat("\n\nResults saved to: results_test/TEST_A1_Small/\n")
