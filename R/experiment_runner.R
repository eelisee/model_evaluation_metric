# Experiment Runner
# ==================
# Orchestrates complete experiment pipeline from data generation to reporting

# Ensure required packages
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("Installing jsonlite package...\n")
  install.packages("jsonlite", repos = "https://cloud.r-project.org", quiet = TRUE)
}

#' Run Complete Experiment
#'
#' Orchestrates full pipeline: data generation, model enumeration,
#' evaluation, selection, and reporting
#'
#' @param config List. Experiment configuration (from scenarios.R)
#' @param output_dir Character. Base output directory (default "results/")
#' @param save_data Logical. Save data objects
#' @param save_plots Logical. Generate and save plots
#' @param verbose Logical. Print progress messages
#' @return List with all results
#' @export
run_experiment <- function(config, 
                          output_dir = "results",
                          save_data = TRUE,
                          save_plots = TRUE,
                          verbose = TRUE) {
  
  # Start timer
  start_time <- Sys.time()
  
  if (verbose) {
    cat("\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat("RUNNING EXPERIMENT:", config$scenario_name, "\n")
    cat(paste(rep("=", 70), collapse = ""), "\n\n")
  }
  
  # ==========================================================================
  # STEP 1: Create output directory
  # ==========================================================================
  
  if (verbose) cat("Step 1: Creating output directory...\n")
  
  exp_dir <- create_experiment_dir(
    config$scenario_name, 
    base_dir = output_dir,
    timestamp = TRUE
  )
  
  if (verbose) cat(sprintf("  Output directory: %s\n\n", exp_dir))
  
  # ==========================================================================
  # STEP 2: Generate data
  # ==========================================================================
  
  if (verbose) cat("Step 2: Generating data...\n")
  
  data <- generate_data_advanced(config)
  
  if (verbose) {
    cat(sprintf("  Generated %d observations with %d predictors\n", 
                nrow(data$X), ncol(data$X)))
    cat(sprintf("  True active predictors: %d\n", data$meta$p_true))
    cat(sprintf("  True indices: %s\n\n", 
                paste(data$meta$true_indices, collapse = ", ")))
  }
  
  # Save data if requested
  if (save_data) {
    save_data_objects(data, exp_dir)
  }
  
  # ==========================================================================
  # STEP 3: Enumerate models
  # ==========================================================================
  
  if (verbose) cat("Step 3: Enumerating models...\n")
  
  # Get enumeration strategy from config (or use default)
  enum_strategy <- if (!is.null(config$enumeration_strategy)) {
    config$enumeration_strategy
  } else {
    "full"
  }
  
  max_p_enum <- config$max_p
  n_samples <- config$n_samples
  
  subsets <- enumerate_models(
    p_max = ncol(data$X),
    strategy = enum_strategy,
    max_p = max_p_enum,
    n_samples = n_samples
  )
  
  if (verbose) {
    cat(sprintf("  Strategy: %s\n", enum_strategy))
    cat(sprintf("  Models to evaluate: %d\n\n", length(subsets)))
  }
  
  # ==========================================================================
  # STEP 4: Evaluate all models
  # ==========================================================================
  
  if (verbose) cat("Step 4: Evaluating models...\n")
  
  results <- evaluate_all_models_batch(
    X = data$X,
    y = data$y,
    subsets = subsets,
    verbose = verbose
  )
  
  if (verbose) cat("\n")
  
  # ==========================================================================
  # STEP 5: Apply selection rule
  # ==========================================================================
  
  if (verbose) cat("Step 5: Applying selection rule...\n")
  
  selection_results <- apply_selection_rule(results)
  
  if (verbose) {
    cat(sprintf("  M_p: p* = %d\n", 
                selection_results$mp$p_star))
    cat(sprintf("  AIC: p* = %d\n", selection_results$p_star_aic))
    cat(sprintf("  BIC: p* = %d\n", selection_results$p_star_bic))
    cat(sprintf("  True: p_true = %d\n\n", config$p_true))
  }
  
  # ==========================================================================
  # STEP 6: Compute recovery metrics
  # ==========================================================================
  
  if (verbose) cat("Step 6: Computing recovery metrics...\n")
  
  recovery_df <- evaluate_selection_rules(selection_results, data$true_beta)
  
  if (verbose) {
    cat(sprintf("  M_p F1 score: %.3f\n\n", 
                recovery_df[recovery_df$rule == "mp", "F1"]))
  }
  
  # ==========================================================================
  # STEP 7: Compute additional diagnostics
  # ==========================================================================
  
  if (verbose) cat("Step 7: Computing diagnostics...\n")
  
  # Aggregate by p
  aggregate_df <- aggregate_by_p(results)
  
  # Ranking correlations
  ranking_corr <- compute_ranking_correlations(results, data$true_beta)
  
  if (verbose) {
    cat(sprintf("  M_p vs R² correlation: %.3f\n", ranking_corr$Mp_vs_R2))
    cat(sprintf("  M_p vs truth closeness: %.3f\n\n", 
                ranking_corr$Mp_vs_truth_closeness))
  }
  
  # ==========================================================================
  # STEP 8: Save results
  # ==========================================================================
  
  if (verbose) cat("Step 8: Saving results...\n")
  
  # Save metadata (silent)
  save_meta_json(config, exp_dir)
  
  # Save model results (silent)
  save_model_results(results, exp_dir)
  
  # Save recovery stats (silent)
  save_recovery_stats(recovery_df, exp_dir)
  
  # Save selection comparison (silent)
  save_selection_comparison(selection_results, exp_dir)
  
  # Save ranking correlations (silent)
  save_ranking_correlations(ranking_corr, exp_dir)
  
  # Save aggregate statistics (silent)
  save_aggregate_by_p(aggregate_df, exp_dir)
  
  if (verbose) cat("\n")
  
  # ==========================================================================
  # STEP 9: Generate plots
  # ==========================================================================
  
  if (save_plots) {
    if (verbose) cat("Step 9: Generating plots...\n")
    
    plots_dir <- file.path(exp_dir, "plots")
    
    # M_p and R² curves (silent)
    plot_mp_curves(
      results, 
      p_star = selection_results$mp$p_star,
      save_path = file.path(plots_dir, "mp_and_r2_curves.png")
    )
    
    # M_p efficiency curve (silent)
    plot_best_mp_curve(
      results,
      p_star = selection_results$mp$p_star,
      save_path = file.path(plots_dir, "mp_efficiency_curve.png")
    )
    
    # R² curve (silent)
    plot_r2_curve(
      results,
      p_star = selection_results$mp$p_star,
      save_path = file.path(plots_dir, "r2_curve.png")
    )
    
    # All models scatter (silent)
    plot_all_models_scatter(
      results,
      p_star = selection_results$mp$p_star,
      save_path = file.path(plots_dir, "all_models_scatter.png")
    )
    
    # Criterion comparison (silent)
    plot_criterion_comparison(
      results,
      p_star_mp = selection_results$mp$p_star,
      save_path = file.path(plots_dir, "criterion_comparison.png")
    )
    
    if (verbose) cat("\n")
  }
  
  # ==========================================================================
  # STEP 10: Generate summary report
  # ==========================================================================
  
  if (verbose) cat("Step 10: Generating summary report...\n")
  
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  generate_summary_report(
    config = config,
    results = results,
    selection_results = selection_results,
    recovery_df = recovery_df,
    exp_dir = exp_dir,
    elapsed_time = elapsed_time
  )
  
  if (verbose) cat("\n")
  
  # ==========================================================================
  # COMPLETE
  # ==========================================================================
  
  if (verbose) {
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat("EXPERIMENT COMPLETE\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat(sprintf("  Total time: %.2f seconds\n", elapsed_time))
    cat(sprintf("  Output directory: %s\n", exp_dir))
    cat("\n")
  }
  
  # Return all results
  return(invisible(list(
    exp_dir = exp_dir,
    config = config,
    data = data,
    results = results,
    selection_results = selection_results,
    recovery = recovery_df,
    aggregate = aggregate_df,
    ranking_corr = ranking_corr,
    elapsed_time = elapsed_time
  )))
}


#' Run Multiple Experiments (Batch Mode)
#'
#' Run multiple scenarios or repetitions
#'
#' @param scenarios List. List of scenario configurations
#' @param n_reps Integer. Number of repetitions per scenario (with different seeds)
#' @param output_dir Character. Base output directory
#' @param parallel Logical. Use parallel execution (not yet implemented)
#' @param verbose Logical. Print progress
#' @return List of all experiment results
#' @export
run_batch_experiments <- function(scenarios, 
                                  n_reps = 1,
                                  output_dir = "results",
                                  parallel = FALSE,
                                  verbose = TRUE) {
  
  if (verbose) {
    cat("\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat("BATCH EXPERIMENT RUNNER\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat(sprintf("  Scenarios: %d\n", length(scenarios)))
    cat(sprintf("  Repetitions per scenario: %d\n", n_reps))
    cat(sprintf("  Total experiments: %d\n", length(scenarios) * n_reps))
    cat(paste(rep("=", 70), collapse = ""), "\n\n")
  }
  
  all_results <- list()
  experiment_count <- 0
  total_experiments <- length(scenarios) * n_reps
  
  for (scenario_name in names(scenarios)) {
    config <- scenarios[[scenario_name]]
    
    if (verbose) {
      cat(sprintf("\n[%s] Starting scenario: %s\n", 
                  format(Sys.time(), "%H:%M:%S"), scenario_name))
    }
    
    scenario_results <- list()
    
    for (rep in 1:n_reps) {
      experiment_count <- experiment_count + 1
      
      if (verbose && n_reps > 1) {
        cat(sprintf("  Repetition %d/%d (Experiment %d/%d)\n", 
                    rep, n_reps, experiment_count, total_experiments))
      }
      
      # Set unique seed for this repetition
      config$seed <- if (!is.null(config$seed)) {
        config$seed + rep - 1
      } else {
        1000 * which(names(scenarios) == scenario_name) + rep
      }
      
      # Run experiment
      result <- run_experiment(
        config = config,
        output_dir = output_dir,
        save_data = (rep == 1),  # Only save data for first rep
        save_plots = TRUE,
        verbose = FALSE  # Suppress individual experiment messages
      )
      
      scenario_results[[rep]] <- result
    }
    
    all_results[[scenario_name]] <- scenario_results
  }
  
  if (verbose) {
    cat("\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat("BATCH EXPERIMENTS COMPLETE\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat(sprintf("  Total experiments run: %d\n", experiment_count))
    cat(sprintf("  Results saved to: %s/\n", output_dir))
    cat("\n")
  }
  
  return(invisible(all_results))
}


#' Quick Test Run
#'
#' Run a single quick experiment for testing
#'
#' @param scenario_name Character. Which scenario to run
#' @export
quick_test <- function(scenario_name = "s2") {
  
  # Get scenario configuration
  if (scenario_name == "s1") {
    config <- scenario_s1_constant(seed = 123)
  } else if (scenario_name == "s2") {
    config <- scenario_s2_baseline(seed = 123)
  } else if (scenario_name == "s3") {
    config <- scenario_s3_random_order(seed = 123)
  } else {
    stop("Unknown scenario. Use 's1', 's2', or 's3'")
  }
  
  # Run experiment
  result <- run_experiment(
    config = config,
    output_dir = "results_test",
    save_data = TRUE,
    save_plots = TRUE,
    verbose = TRUE
  )
  
  return(invisible(result))
}
