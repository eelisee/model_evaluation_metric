# Main Experiment Runner
# =======================
# Executes the full experiment loop as specified

# Load modules
source("R/01_data_generation.R")
source("R/02_metrics.R")
source("R/03_evaluation.R")
source("R/04_visualization.R")

#' Define All Scenarios
#'
#' @return List of scenario configurations
#' @export
define_scenarios <- function() {
  
  list(
    
    # === A) Baseline Scenarios ===
    
    A1 = list(
      name = "A1_Baseline_Uncorrelated",
      description = "Uncorrelated predictors, sparse β, Gaussian noise",
      n = 500,
      p = 20,
      sigma_structure = "identity",
      rho = 0,
      support_spec = 3,
      signal_strength = "strong",
      sigma_eps = 0.2
    ),
    
    A2 = list(
      name = "A2_Single_Predictor",
      description = "Only one relevant variable",
      n = 500,
      p = 20,
      sigma_structure = "identity",
      rho = 0,
      support_spec = "single",
      signal_strength = "strong",
      sigma_eps = 0.2
    ),
    
    A3 = list(
      name = "A3_Full_Support",
      description = "All variables relevant",
      n = 500,
      p = 20,
      sigma_structure = "identity",
      rho = 0,
      support_spec = "full",
      signal_strength = "strong",
      sigma_eps = 0.2
    ),
    
    # === B) Structural Scenarios ===
    
    B1_weak = list(
      name = "B1_AR1_Weak",
      description = "AR(1) correlation, ρ = 0.5",
      n = 500,
      p = 20,
      sigma_structure = "ar1",
      rho = 0.5,
      support_spec = 3,
      signal_strength = "strong",
      sigma_eps = 0.2
    ),
    
    B1_strong = list(
      name = "B1_AR1_Strong",
      description = "AR(1) correlation, ρ = 0.8",
      n = 500,
      p = 20,
      sigma_structure = "ar1",
      rho = 0.8,
      support_spec = 3,
      signal_strength = "strong",
      sigma_eps = 0.2
    ),
    
    B2 = list(
      name = "B2_Compound_Symmetry",
      description = "Exchangeable correlation",
      n = 500,
      p = 20,
      sigma_structure = "compound",
      rho = 0.5,
      support_spec = 3,
      signal_strength = "strong",
      sigma_eps = 0.2
    ),
    
    B3 = list(
      name = "B3_Block_Structure",
      description = "Block correlation (groups of 5)",
      n = 500,
      p = 20,
      sigma_structure = "block",
      rho = 0.7,
      block_size = 5,
      support_spec = 3,
      signal_strength = "strong",
      sigma_eps = 0.2
    ),
    
    # === C) Support Scenarios ===
    
    C1 = list(
      name = "C1_Weak_Signals",
      description = "All signals weak",
      n = 500,
      p = 20,
      sigma_structure = "identity",
      rho = 0,
      support_spec = 5,
      signal_strength = "weak",
      sigma_eps = 0.2
    ),
    
    C2 = list(
      name = "C2_Many_Weak_Signals",
      description = "Many weak signals",
      n = 500,
      p = 20,
      sigma_structure = "identity",
      rho = 0,
      support_spec = 10,
      signal_strength = "weak",
      sigma_eps = 0.2
    ),
    
    C3 = list(
      name = "C3_Mixed_Signals",
      description = "Mixed strong and weak signals",
      n = 500,
      p = 20,
      sigma_structure = "identity",
      rho = 0,
      support_spec = 8,
      signal_strength = "mixed",
      sigma_eps = 0.2
    )
  )
}


#' Run Single Scenario
#'
#' @param scenario List. Scenario configuration
#' @param N_iterations Integer. Number of Monte Carlo iterations
#' @param output_dir String. Output directory
#' @param parallel_iterations Logical. If TRUE, parallelize across iterations (default: TRUE)
#' @param n_cores Integer. Number of cores to use (default: detectCores() - 1)
#' @export
run_scenario <- function(scenario, N_iterations = 50, output_dir = "results", parallel_iterations = TRUE, n_cores = NULL) {
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("SCENARIO:", scenario$name, "\n")
  cat(scenario$description, "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  # Create output directory
  scenario_dir <- file.path(output_dir, scenario$name)
  dir.create(scenario_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Storage for all iterations
  all_iterations <- list()
  all_r2_curves <- list()
  all_metric_results <- list()
  
  # Monte Carlo loop - can be parallelized
  if (parallel_iterations && N_iterations > 1) {
    
    cat(sprintf("  Running %d iterations in parallel...\n", N_iterations))
    
    # Parallel execution
    library(parallel)
    if (is.null(n_cores)) {
      n_cores <- min(N_iterations, detectCores() - 1)
    } else {
      n_cores <- min(N_iterations, n_cores)
    }
    cat(sprintf("  Using %d cores for parallel execution\n", n_cores))
    
    results <- mclapply(1:N_iterations, function(iter) {
      
      # Set seed for reproducibility
      scenario$seed <- 1000 + iter
      
      # Generate data
      data <- generate_data(scenario)
      
      # Compute R² curve (exhaustive search over all subsets)
      # Use only 1 core per iteration to avoid memory overhead when iterations are parallel
      r2_curve <- compute_r2_curve(data$X, data$y, n_cores = 1)
      
      # Apply all metrics
      metric_results <- apply_all_metrics(r2_curve)
      
      # Evaluate (with support_true for subset evaluation)
      eval <- evaluate_iteration(metric_results, data$p_true, data$support_true)
      eval$iteration <- iter
      
      list(
        eval = eval,
        r2_curve = r2_curve,
        metric_results = metric_results,
        p_true = data$p_true,
        support_true = data$support_true,
        beta_true = data$beta_true
      )
      
    }, mc.cores = n_cores)
    
    # Extract results
    p_true <- results[[1]]$p_true
    all_support_true <- list()
    all_beta_true <- list()
    for (iter in 1:N_iterations) {
      all_iterations[[iter]] <- results[[iter]]$eval
      all_r2_curves[[iter]] <- results[[iter]]$r2_curve
      all_metric_results[[iter]] <- results[[iter]]$metric_results
      all_support_true[[iter]] <- results[[iter]]$support_true
      all_beta_true[[iter]] <- results[[iter]]$beta_true
    }
    
    # Force garbage collection to free memory
    gc()
    
    cat(sprintf("  ✓ Completed %d iterations (parallel)\n\n", N_iterations))
    
  } else {
    
    # Sequential execution (original code)
    all_support_true <- list()
    all_beta_true <- list()
    
    for (iter in 1:N_iterations) {
      
      # Only print every 10th iteration or first/last
      if (iter == 1 || iter == N_iterations || iter %% 10 == 0) {
        cat(sprintf("  [%d/%d]\n", iter, N_iterations))
      }
      
      # Set seed for reproducibility
      scenario$seed <- 1000 + iter
      
      # Generate data
      data <- generate_data(scenario)
      
      # Compute R² curve (exhaustive search over all subsets)
      r2_curve <- compute_r2_curve(data$X, data$y, n_cores = n_cores)
      
      # Apply all metrics
      metric_results <- apply_all_metrics(r2_curve)
      
      # Evaluate (with support_true for subset evaluation)
      eval <- evaluate_iteration(metric_results, data$p_true, data$support_true)
      eval$iteration <- iter
      
      # Store
      all_iterations[[iter]] <- eval
      all_r2_curves[[iter]] <- r2_curve
      all_metric_results[[iter]] <- metric_results
      all_support_true[[iter]] <- data$support_true
      all_beta_true[[iter]] <- data$beta_true
      
      # Force garbage collection to free memory
      if (iter %% 10 == 0) gc()
    }
  }
  
  cat("\n  Computing summary statistics...\n")
  
  # Get p_true (either from parallel or sequential branch)
  if (parallel_iterations && N_iterations > 1) {
    # p_true already set in parallel branch
  } else {
    p_true <- data$p_true
  }
  
  # Compute summary statistics
  summary_stats <- compute_summary_statistics(all_iterations, all_r2_curves, p_true, all_metric_results)
  
  # Create plots (using all iterations for averaging)
  cat("  Creating plots...\n")
  create_all_plots(
    all_iterations = all_iterations,
    all_r2_curves = all_r2_curves,
    all_metric_results = all_metric_results,
    summary_stats = summary_stats,
    p_true = p_true,
    output_dir = file.path(scenario_dir, "plots")
  )
  
  # Save summary
  cat("  Saving summary...\n")
  write_summary(scenario, summary_stats, all_iterations, 
                file.path(scenario_dir, "summary.txt"), all_metric_results)
  
  # Save summary CSV
  write.csv(summary_stats, 
            file.path(scenario_dir, "summary_stats.csv"),
            row.names = FALSE)
  
  # Save detailed results CSV (all iterations x all p x all metrics)
  cat("  Saving detailed results...\n")
  detailed_results <- create_detailed_results(all_iterations, all_r2_curves, all_metric_results, all_support_true, all_beta_true)
  write.csv(detailed_results,
            file.path(scenario_dir, "detailed_results.csv"),
            row.names = FALSE)
  
  cat("\n✓ Scenario complete! Results saved to:", scenario_dir, "\n")
  
  return(list(
    scenario = scenario,
    summary_stats = summary_stats,
    all_iterations = all_iterations
  ))
}


#' Write Summary Report
#'
#' @param scenario List
#' @param summary_stats Data frame
#' @param all_iterations List
#' @param all_metric_results List (optional, for delta2 values)
#' @param filename String
write_summary <- function(scenario, summary_stats, all_iterations, filename, all_metric_results = NULL) {
  
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
    cat(sprintf("  Cardinality Metrics:\n"))
    cat(sprintf("    MAE:         %.3f\n", summary_stats$MAE[i]))
    cat(sprintf("    Bias:        %.3f\n", summary_stats$Bias[i]))
    cat(sprintf("    Variance:    %.3f\n", summary_stats$Variance[i]))
    cat(sprintf("    Hit Rate:    %.1f%%\n", summary_stats$HitRate[i] * 100))
    cat(sprintf("    Correct:     %d\n", summary_stats$n_correct[i]))
    cat(sprintf("    Underfit:    %d\n", summary_stats$n_underfit[i]))
    cat(sprintf("    Overfit:     %d\n", summary_stats$n_overfit[i]))
    
    cat(sprintf("\n  Subset Selection Metrics:\n"))
    cat(sprintf("    Jaccard Index:      %.3f\n", summary_stats$Jaccard[i]))
    cat(sprintf("    Precision:          %.3f\n", summary_stats$Precision[i]))
    cat(sprintf("    Recall:             %.3f\n", summary_stats$Recall[i]))
    cat(sprintf("    F1 Score:           %.3f\n", summary_stats$F1[i]))
    cat(sprintf("    Avg True Pos:       %.2f\n", summary_stats$avg_TP[i]))
    cat(sprintf("    Avg False Pos:      %.2f\n", summary_stats$avg_FP[i]))
    cat(sprintf("    Avg False Neg:      %.2f\n", summary_stats$avg_FN[i]))
    cat(sprintf("    Subset Hit Rate:    %.1f%%\n", summary_stats$SubsetHitRate[i] * 100))
    
    # Add delta2 for M_p metric (from all_metric_results if available)
    if (m == "M_p" && !is.null(all_metric_results)) {
      # Get delta2 from first iteration
      delta2 <- all_metric_results[[1]]$M_p$delta2
      
      if (!is.null(delta2)) {
        cat("\n  Second Derivative (delta2) of M_p:\n")
        p_vals <- 1:length(delta2)
        for (p in p_vals) {
          cat(sprintf("    p=%d: %+.6e\n", p, delta2[p]))
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


#' Run All Scenarios
#'
#' @param N_iterations Integer
#' @param output_dir String
#' @param n_cores Integer. Number of cores to use (default: detectCores() - 1)
#' @export
run_all_scenarios <- function(N_iterations = 50, output_dir = "results", n_cores = NULL) {
  
  scenarios <- define_scenarios()
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("RUNNING ALL SCENARIOS\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat(sprintf("Total scenarios: %d\n", length(scenarios)))
  cat(sprintf("Iterations per scenario: %d\n", N_iterations))
  cat(sprintf("Output directory: %s\n", output_dir))
  cat("\n")
  
  results <- list()
  
  for (i in seq_along(scenarios)) {
    cat(sprintf("\n▶ Starting scenario %d/%d with N=%d iterations\n", 
                i, length(scenarios), N_iterations))
    results[[i]] <- run_scenario(
      scenario = scenarios[[i]], 
      N_iterations = N_iterations,  # EXPLICITLY NAMED
      output_dir = output_dir,
      n_cores = n_cores
    )
  }
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("ALL SCENARIOS COMPLETE!\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  return(results)
}


# ============================================================================
# MAIN EXECUTION
# ============================================================================

# Check if we're being sourced for testing (skip auto-execution)
if (exists(".skip_auto_run") && .skip_auto_run) {
  # Sourced for testing - don't auto-run
  invisible(NULL)
} else if (interactive()) {
  cat("\nInteractive mode. To run experiments:\n\n")
  cat("  # Run single scenario\n")
  cat("  result <- run_scenario(define_scenarios()$A1, N_iterations = 10)\n\n")
  cat("  # Run all scenarios\n")
  cat("  results <- run_all_scenarios(N_iterations = 100)\n\n")
} else {
  # Command-line execution
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    # Default: run all with 50 iterations
    run_all_scenarios(N_iterations = 50)
  } else {
    N <- as.integer(args[1])
    run_all_scenarios(N_iterations = N)
  }
}
