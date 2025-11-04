# I/O and Folder Management Utilities
# =====================================
# Handles file structure, saving/loading results, and metadata

#' Create Experiment Output Directory Structure
#'
#' @param scenario_name Character. Scenario identifier
#' @param base_dir Character. Base results directory (default "results/")
#' @param timestamp Logical. Add timestamp to folder name
#' @return Character. Path to created directory
#' @export
create_experiment_dir <- function(scenario_name, base_dir = "results", timestamp = TRUE) {
  
  # Create base directory if needed
  if (!dir.exists(base_dir)) {
    dir.create(base_dir, recursive = TRUE)
  }
  
  # Create scenario-specific folder
  if (timestamp) {
    time_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
    folder_name <- sprintf("%s__%s", scenario_name, time_str)
  } else {
    folder_name <- scenario_name
  }
  
  exp_dir <- file.path(base_dir, folder_name)
  
  # Create directory structure
  dir.create(exp_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(exp_dir, "plots"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(exp_dir, "diagnostics"), recursive = TRUE, showWarnings = FALSE)
  
  return(exp_dir)
}


#' Save Experiment Metadata as JSON
#'
#' @param config List. Experiment configuration
#' @param exp_dir Character. Experiment directory
#' @export
save_meta_json <- function(config, exp_dir) {
  
  meta_file <- file.path(exp_dir, "meta.json")
  
  # Convert to JSON-friendly format
  meta <- config
  meta$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  meta$R_version <- R.version.string
  
  # Convert vectors to lists for JSON
  for (name in names(meta)) {
    if (is.vector(meta[[name]]) && length(meta[[name]]) > 1) {
      meta[[name]] <- as.list(meta[[name]])
    }
  }
  
  # Write as JSON (simple format without external package)
  json_str <- jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE)
  writeLines(json_str, meta_file)
  
  cat(sprintf("  Metadata saved to: %s\n", meta_file))
}


#' Save Data Objects
#'
#' @param data List. Data object from generate_data_advanced
#' @param exp_dir Character. Experiment directory
#' @export
save_data_objects <- function(data, exp_dir) {
  
  data_file <- file.path(exp_dir, "data.RData")
  
  X <- data$X
  y <- data$y
  true_beta <- data$true_beta
  
  save(X, y, true_beta, file = data_file)
  
  cat(sprintf("  Data objects saved to: %s\n", data_file))
}


#' Save Model Results
#'
#' @param results Data frame. Full model evaluation results
#' @param exp_dir Character. Experiment directory
#' @export
save_model_results <- function(results, exp_dir) {
  
  # Full results
  full_file <- file.path(exp_dir, "models_full.csv")
  write.csv(results, full_file, row.names = FALSE)
  
  # Best models by p
  best_models <- find_best_models_by_p(results)
  best_file <- file.path(exp_dir, "best_models_by_p.csv")
  write.csv(best_models, best_file, row.names = FALSE)
  
  cat(sprintf("  Model results saved to: %s\n", dirname(full_file)))
}


#' Save Recovery Statistics
#'
#' @param recovery_df Data frame. Recovery metrics from evaluate_selection_rules
#' @param exp_dir Character. Experiment directory
#' @export
save_recovery_stats <- function(recovery_df, exp_dir) {
  
  recovery_file <- file.path(exp_dir, "recovery_stats.csv")
  write.csv(recovery_df, recovery_file, row.names = FALSE)
  
  cat(sprintf("  Recovery stats saved to: %s\n", recovery_file))
}


##' Save Selection Comparison
#'
#' @param selection_results List. Output from apply_selection_rule
#' @param exp_dir Character. Experiment directory
#' @export
save_selection_comparison <- function(selection_results, exp_dir) {
  
  # Extract key information from M_p rule and comparison methods
  comparison_df <- data.frame(
    rule = c("mp", "AIC", "BIC"),
    p_star = c(
      selection_results$mp$p_star,
      selection_results$p_star_aic,
      selection_results$p_star_bic
    ),
    is_degenerate = c(
      selection_results$mp$is_degenerate,
      FALSE, FALSE
    ),
    stringsAsFactors = FALSE
  )
  
  comp_file <- file.path(exp_dir, "selection_comparison.csv")
  write.csv(comparison_df, comp_file, row.names = FALSE)
}


#' Save Ranking Correlations
#'
#' @param correlations Data frame. Output from compute_ranking_correlations
#' @param exp_dir Character. Experiment directory
#' @export
save_ranking_correlations <- function(correlations, exp_dir) {
  
  corr_file <- file.path(exp_dir, "ranking_correlations.csv")
  write.csv(correlations, corr_file, row.names = FALSE)
  
  cat(sprintf("  Ranking correlations saved to: %s\n", corr_file))
}


#' Save Aggregated Statistics by Cardinality
#'
#' @param aggregate_df Data frame. Output from aggregate_by_p
#' @param exp_dir Character. Experiment directory
#' @export
save_aggregate_by_p <- function(aggregate_df, exp_dir) {
  
  agg_file <- file.path(exp_dir, "diagnostics", "aggregate_by_p.csv")
  write.csv(aggregate_df, agg_file, row.names = FALSE)
  
  cat(sprintf("  Aggregate statistics saved to: %s\n", agg_file))
}


#' Generate Summary Text Report
#'
#' @param config List. Experiment configuration
#' @param results Data frame. Model results
#' @param selection_results List. Selection results
#' @param recovery_df Data frame. Recovery metrics
#' @param exp_dir Character. Experiment directory
#' @param elapsed_time Numeric. Execution time in seconds
#' @export
generate_summary_report <- function(config, results, selection_results, 
                                   recovery_df, exp_dir, elapsed_time) {
  
  summary_file <- file.path(exp_dir, "summary.txt")
  
  sink(summary_file)
  
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("M_p MODEL SELECTION EXPERIMENT SUMMARY\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  # Scenario information
  cat("SCENARIO:", config$scenario_name, "\n")
  cat("Description:", config$scenario_description, "\n\n")
  
  # Configuration
  cat("CONFIGURATION\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  cat(sprintf("  Sample size (n): %d\n", config$n))
  cat(sprintf("  Predictors (p_max): %d\n", config$p_max))
  cat(sprintf("  True active predictors (p_true): %d\n", config$p_true))
  cat(sprintf("  Noise level (sigma_eps): %.3f\n", config$sigma_eps))
  cat(sprintf("  Predictor distribution: %s\n", config$X_dist))
  cat(sprintf("  Correlation structure: %s\n", config$correlation_structure))
  if (!is.null(config$rho) && config$rho > 0) {
    cat(sprintf("  Correlation parameter (rho): %.3f\n", config$rho))
  }
  cat(sprintf("  Random seed: %s\n", ifelse(is.null(config$seed), "NULL", config$seed)))
  cat("\n")
  
  # Results overview
  cat("RESULTS OVERVIEW\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  cat(sprintf("  Models evaluated: %d\n", nrow(results)))
  cat(sprintf("  R² range: [%.4f, %.4f]\n", 
              min(results$R2, na.rm = TRUE), max(results$R2, na.rm = TRUE)))
  cat(sprintf("  M_p range: [%.4f, %.4f]\n", 
              min(results$Mp, na.rm = TRUE), max(results$Mp, na.rm = TRUE)))
  cat(sprintf("  Execution time: %.2f seconds\n", elapsed_time))
  cat("\n")
  
  # Selection results
  cat("MODEL SELECTION RESULTS\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  cat(sprintf("  M_p:                  p* = %d\n", 
              selection_results$mp$p_star))
  cat(sprintf("  AIC:                  p* = %d\n", 
              selection_results$p_star_aic))
  cat(sprintf("  BIC:                  p* = %d\n", 
              selection_results$p_star_bic))
  cat(sprintf("  True p:               p_true = %d\n", config$p_true))
  cat("\n")
  
  # Best models by cardinality table
  cat("BEST MODELS BY CARDINALITY\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  best_models <- selection_results$best_models
  # Format for nice printing
  table_output <- capture.output(print(best_models[, c("p", "R2", "Mp", "AIC", "BIC", "subset_str")], 
                                       row.names = TRUE, digits = 4))
  cat(paste(table_output, collapse = "\n"), "\n\n")
  
  # Recovery metrics (if available)
  if (!is.null(recovery_df) && nrow(recovery_df) > 0) {
    cat("VARIABLE SELECTION PERFORMANCE\n")
    cat(paste(rep("-", 70), collapse = ""), "\n")
    
    for (i in 1:nrow(recovery_df)) {
      row <- recovery_df[i, ]
      rule_label <- ifelse(row$rule == "mp", "M_p", toupper(row$rule))
      cat(sprintf("  %s:\n", rule_label))
      cat(sprintf("    p* = %d (true: %d, diff: %+d)\n", 
                  row$p_star, row$p_true, row$p_diff))
      cat(sprintf("    TP=%d, FP=%d, FN=%d\n", row$TP, row$FP, row$FN))
      cat(sprintf("    Precision: %.3f, Recall: %.3f, F1: %.3f\n", 
                  row$precision, row$recall, row$F1))
      cat(sprintf("    Hamming distance: %d, Exact match: %s\n", 
                  row$hamming_distance, row$exact_match))
      cat(sprintf("    Selected: %s\n\n", row$selected_subset))
    }
  }
  
  # Expected behavior
  cat("EXPECTED BEHAVIOR\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  cat("  ", config$expected_behavior, "\n\n")
  
  # Interpretation
  cat("INTERPRETATION\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  
  # M_p assessment
  p_star_sd <- selection_results$mp$p_star
  if (p_star_sd == config$p_true) {
    cat("  ✓ M_p correctly identified the true complexity\n")
  } else if (abs(p_star_sd - config$p_true) <= 1) {
    cat("  ~ M_p close to true complexity (off by", 
        abs(p_star_sd - config$p_true), ")\n")
  } else {
    cat("  ✗ M_p missed the true complexity by", 
        abs(p_star_sd - config$p_true), "\n")
  }
  
  # Degeneracy check
  if (selection_results$mp$is_degenerate) {
    cat("  ⚠ M_p curve is degenerate (constant or near-constant)\n")
  }
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("End of summary\n")
  
  sink()
  
  cat(sprintf("  Summary report saved to: %s\n", summary_file))
}


#' Load Experiment Results
#'
#' @param exp_dir Character. Experiment directory
#' @return List with all loaded objects
#' @export
load_experiment_results <- function(exp_dir) {
  
  results <- list()
  
  # Load metadata
  meta_file <- file.path(exp_dir, "meta.json")
  if (file.exists(meta_file)) {
    results$meta <- jsonlite::fromJSON(meta_file)
  }
  
  # Load data
  data_file <- file.path(exp_dir, "data.RData")
  if (file.exists(data_file)) {
    env <- new.env()
    load(data_file, envir = env)
    results$X <- env$X
    results$y <- env$y
    results$true_beta <- env$true_beta
  }
  
  # Load model results
  models_file <- file.path(exp_dir, "models_full.csv")
  if (file.exists(models_file)) {
    results$models <- read.csv(models_file, stringsAsFactors = FALSE)
  }
  
  # Load best models
  best_file <- file.path(exp_dir, "best_models_by_p.csv")
  if (file.exists(best_file)) {
    results$best_models <- read.csv(best_file, stringsAsFactors = FALSE)
  }
  
  # Load recovery stats
  recovery_file <- file.path(exp_dir, "recovery_stats.csv")
  if (file.exists(recovery_file)) {
    results$recovery <- read.csv(recovery_file, stringsAsFactors = FALSE)
  }
  
  return(results)
}


#' Save Ranking Correlations
#'
#' Save ranking correlations to CSV
#'
#' @param ranking_corr List. Output from compute_ranking_correlations()
#' @param exp_dir Character. Experiment directory
#' @export
save_ranking_correlations <- function(ranking_corr, exp_dir) {
  
  df <- data.frame(
    metric = names(ranking_corr),
    correlation = unlist(ranking_corr),
    row.names = NULL
  )
  
  out_file <- file.path(exp_dir, "ranking_correlations.csv")
  write.csv(df, out_file, row.names = FALSE)
  
  invisible(out_file)
}


#' Save Aggregate Statistics by p
#'
#' Save aggregate statistics by p to CSV
#'
#' @param aggregate_df Data frame. Output from aggregate_by_p()
#' @param exp_dir Character. Experiment directory
#' @export
save_aggregate_by_p <- function(aggregate_df, exp_dir) {
  
  out_file <- file.path(exp_dir, "aggregate_by_p.csv")
  write.csv(aggregate_df, out_file, row.names = FALSE)
  
  invisible(out_file)
}


#' Check if jsonlite is available, install if needed
#' @keywords internal
ensure_jsonlite <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    message("Installing jsonlite package for JSON support...")
    install.packages("jsonlite", repos = "https://cloud.r-project.org", quiet = TRUE)
  }
}
