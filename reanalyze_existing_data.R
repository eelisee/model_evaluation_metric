# Reanalyze Existing Data with New M_p Definition
# =================================================
# Reads detailed_results.csv from completed scenarios and recomputes
# metrics with the updated finite difference method

# Load modules
source("R/02_metrics.R")
source("R/03_evaluation.R")
source("R/04_visualization.R")

# Load write_summary function manually (to avoid running main script)
write_summary <- function(scenario, summary_stats, all_iterations, filename) {
  
  sink(filename)
  
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("SCENARIO:", scenario$name, "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  cat("Description:\n")
  cat("  ", scenario$description, "\n\n")
  
  cat("Configuration:\n")
  cat(sprintf("  Sample size (n):           %d\n", scenario$n))
  cat(sprintf("  Predictors (p):            %d\n", scenario$p))
  cat(sprintf("  Correlation structure:     %s\n", scenario$sigma_structure))
  cat(sprintf("  Correlation parameter:     %.2f\n", scenario$rho))
  cat(sprintf("  Support specification:     %s\n", as.character(scenario$support_spec)))
  cat(sprintf("  Signal strength:           %s\n", scenario$signal_strength))
  cat(sprintf("  Noise std dev (σ_eps):     %.2f\n", scenario$sigma_eps))
  cat("\n")
  
  cat("Summary Statistics:\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  print(summary_stats)
  cat("\n")
  
  cat("Interpretation:\n")
  for (i in 1:nrow(summary_stats)) {
    m <- summary_stats$metric[i]
    cat(sprintf("\n%s:\n", m))
    cat(sprintf("  MAE:         %.3f\n", summary_stats$MAE[i]))
    cat(sprintf("  Bias:        %.3f\n", summary_stats$Bias[i]))
    cat(sprintf("  Variance:    %.3f\n", summary_stats$Variance[i]))
    cat(sprintf("  Hit Rate:    %.1f%%\n", summary_stats$HitRate[i] * 100))
    cat(sprintf("  Correct:     %d\n", summary_stats$n_correct[i]))
    cat(sprintf("  Underfit:    %d\n", summary_stats$n_underfit[i]))
    cat(sprintf("  Overfit:     %d\n", summary_stats$n_overfit[i]))
    
    # Add delta2 for M_p metric
    if (m == "M_p") {
      delta2_cols <- grep("_delta2$", names(summary_stats), value = TRUE)
      if (length(delta2_cols) > 0) {
        cat("\n  Second Derivative (delta2) of M_p:\n")
        for (col in delta2_cols) {
          p_val <- gsub("p|_delta2", "", col)
          delta2_val <- summary_stats[i, col]
          cat(sprintf("    p=%s: %+.6e\n", p_val, delta2_val))
        }
      }
    }
  }
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("Timestamp:", format(Sys.time()), "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  
  sink()
}

#' Reanalyze Single Scenario
#'
#' @param scenario_name String. Name of scenario folder
#' @param input_dir String. Base input directory
#' @param output_dir String. Base output directory
reanalyze_scenario <- function(scenario_name, input_dir = "results", output_dir = "reanalysis_test") {
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("REANALYZING:", scenario_name, "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  # Read detailed results
  scenario_in <- file.path(input_dir, scenario_name)
  detailed_file <- file.path(scenario_in, "detailed_results.csv")
  
  if (!file.exists(detailed_file)) {
    cat("ERROR: File not found:", detailed_file, "\n")
    return(NULL)
  }
  
  detailed <- read.csv(detailed_file)
  cat("  Loaded", nrow(detailed), "rows from detailed_results.csv\n")
  
  # Extract parameters
  N_iterations <- max(detailed$iteration)
  p_max <- max(detailed$p)
  
  cat("  N =", N_iterations, "iterations\n")
  cat("  p_max =", p_max, "\n\n")
  
  # Reconstruct all_iterations, all_r2_curves, all_metric_results
  all_iterations <- list()
  all_r2_curves <- list()
  all_metric_results <- list()
  
  for (iter in 1:N_iterations) {
    
    iter_data <- detailed[detailed$iteration == iter, ]
    
    # Reconstruct r2_curve
    r2_curve <- data.frame(
      p = iter_data$p,
      R2 = iter_data$R2,
      AIC = iter_data$AIC,
      BIC = iter_data$BIC,
      subset = I(vector("list", nrow(iter_data)))  # Dummy subsets
    )
    
    # Recompute metrics with NEW M_p definition
    metric_results <- apply_all_metrics(r2_curve)
    
    # Extract true p from first iteration (assuming all iterations have same p_true)
    if (iter == 1) {
      # Find p_true from selected_by_* columns (where all three agree or majority)
      # Use the scenario name to infer p_true
      if (grepl("Single", scenario_name)) {
        p_true <- 1
      } else if (grepl("Full", scenario_name)) {
        p_true <- p_max
      } else {
        # For baseline scenarios, assume p_true = 3 (most common)
        p_true <- 3
      }
    }
    
    # Evaluate
    eval <- evaluate_iteration(metric_results, p_true)
    eval$iteration <- iter
    
    # Store
    all_iterations[[iter]] <- eval
    all_r2_curves[[iter]] <- r2_curve
    all_metric_results[[iter]] <- metric_results
  }
  
  cat("  Recomputed metrics for all iterations\n\n")
  
  # Compute summary statistics
  summary_stats <- compute_summary_statistics(all_iterations, all_r2_curves, p_true, all_metric_results)
  
  # Create output directory
  scenario_out <- file.path(output_dir, scenario_name)
  dir.create(scenario_out, showWarnings = FALSE, recursive = TRUE)
  
  # Create plots
  cat("  Creating plots...\n")
  create_all_plots(
    all_iterations = all_iterations,
    all_r2_curves = all_r2_curves,
    all_metric_results = all_metric_results,
    summary_stats = summary_stats,
    p_true = p_true,
    output_dir = file.path(scenario_out, "plots")
  )
  
  # Write summary.txt
  cat("  Writing summary.txt...\n")
  
  # Create minimal scenario object for summary
  scenario <- list(
    name = scenario_name,
    description = paste("Reanalyzed with new M_p definition"),
    n = 500,
    p = p_max,
    sigma_structure = "unknown",
    rho = 0,
    support_spec = p_true,
    signal_strength = "unknown",
    sigma_eps = 0.2
  )
  
  write_summary(scenario, summary_stats, all_iterations, 
                file.path(scenario_out, "summary_new_Mp.txt"))
  
  # Save new summary stats
  write.csv(summary_stats, 
            file.path(scenario_out, "summary_stats_new_Mp.csv"),
            row.names = FALSE)
  
  # Save updated detailed results
  cat("  Saving updated detailed_results...\n")
  detailed_new <- create_detailed_results(all_iterations, all_r2_curves, all_metric_results)
  write.csv(detailed_new,
            file.path(scenario_out, "detailed_results_new_Mp.csv"),
            row.names = FALSE)
  
  cat("\n✓ Reanalysis complete! Results saved to:", scenario_out, "\n")
  
  return(list(
    scenario = scenario,
    summary_stats = summary_stats
  ))
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("REANALYSIS WITH NEW M_p DEFINITION\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("\nRecomputing metrics for existing data with updated finite difference method\n")
cat("Output directory: reanalysis_test/\n\n")

# Reanalyze A1 and A2
result_A1 <- reanalyze_scenario("A1_Baseline_Uncorrelated")
result_A2 <- reanalyze_scenario("A2_Single_Predictor")

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("REANALYSIS COMPLETE!\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("\nCompare old vs new results:\n")
cat("  Old: results/A1_Baseline_Uncorrelated/\n")
cat("  New: reanalysis_test/A1_Baseline_Uncorrelated/\n\n")
cat("  Old: results/A2_Single_Predictor/\n")
cat("  New: reanalysis_test/A2_Single_Predictor/\n\n")
cat("To delete test results: rm -rf reanalysis_test/\n\n")
