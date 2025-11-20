# Run Alternative Metrics Comparison
# ====================================
# Compare M_p, M_p_minus (R²/(p-1)), and M_p_scaled (R²/(p/p_max))

# Source main framework modules
source("R/data_generation.R")
source("R/model_evaluation.R")
source("R/selection_rules.R")
source("R/recovery_metrics.R")
source("R/scenarios.R")
source("R/visualization.R")

# Source alternative metrics
source("alternative_metrics/alternative_selection_rules.R")
source("alternative_metrics/alternative_plots.R")
source("alternative_metrics/alternative_summary.R")

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("ALTERNATIVE METRICS COMPARISON (Experimental)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("\n")
cat("Testing three alternative metrics:\n")
cat("  1. M_p_minus = R² / (p-1)    [p=1 case: = 0]\n")
cat("  2. M_p_sqrt  = R² / √p\n")
cat("  3. M_p_scaled = R² / (p/p_max) = R² * p_max / p\n")
cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")


#' Run single scenario with alternative metrics
#'
#' @param config Scenario configuration
#' @param output_dir Base output directory
#' @param save_plots Whether to generate plots
#' @param verbose Print progress messages
#' @return List with all results
run_alternative_experiment <- function(config, output_dir = "alternative_metrics/results", 
                                      save_plots = TRUE, verbose = TRUE) {
  
  if (verbose) {
    cat(sprintf("Running: %s\n", config$scenario_name))
  }
  
  # Create output directory with timestamp
  time_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  exp_dir <- file.path(output_dir, sprintf("%s__%s", config$scenario_name, time_str))
  dir.create(exp_dir, recursive = TRUE, showWarnings = FALSE)
  if (save_plots) {
    dir.create(file.path(exp_dir, "plots"), recursive = TRUE, showWarnings = FALSE)
  }
  
  # Generate data
  data <- generate_data_advanced(config)
  
  # Save data
  save(data, file = file.path(exp_dir, "data.RData"))
  
  # Enumerate and evaluate models
  subsets <- enumerate_models(
    p_max = ncol(data$X),
    strategy = "full"
  )
  
  results <- evaluate_all_models_batch(
    X = data$X,
    y = data$y,
    subsets = subsets,
    verbose = FALSE
  )
  
  # Save all models
  write.csv(results, file.path(exp_dir, "models_full.csv"), row.names = FALSE)
  
  # Apply standard M_p rule
  standard_selection <- apply_selection_rule(results)
  
  # Apply alternative rules
  alt_selection <- apply_alternative_rules(results, p_max = ncol(data$X))
  
  # Save best models by p
  write.csv(alt_selection$best_models, file.path(exp_dir, "best_models_by_p.csv"), row.names = FALSE)
  
  # Compute recovery metrics for all methods
  true_beta <- data$true_beta
  p_max <- length(true_beta)
  
  # Standard M_p
  mp_recovery <- compute_recovery_metrics(
    selected = standard_selection$mp$selected_model$subset_str,
    true = which(true_beta != 0),
    p_max = p_max
  )
  
  # M_p_minus
  if (!alt_selection$mp_minus$is_degenerate) {
    mp_minus_recovery <- compute_recovery_metrics(
      selected = alt_selection$mp_minus$selected_model$subset_str,
      true = which(true_beta != 0),
      p_max = p_max
    )
  } else {
    mp_minus_recovery <- list(p_star = NA, F1 = NA, precision = NA, recall = NA, 
                              precision = NA, recall = NA, TP = NA, FP = NA, FN = NA,
                              hamming = NA, exact_match = FALSE)
  }
  
  # M_p_sqrt
  if (!alt_selection$mp_sqrt$is_degenerate) {
    mp_sqrt_recovery <- compute_recovery_metrics(
      selected = alt_selection$mp_sqrt$selected_model$subset_str,
      true = which(true_beta != 0),
      p_max = p_max
    )
  } else {
    mp_sqrt_recovery <- list(p_star = NA, F1 = NA, precision = NA, recall = NA,
                             precision = NA, recall = NA, TP = NA, FP = NA, FN = NA,
                             hamming = NA, exact_match = FALSE)
  }
  
  # M_p_scaled
  if (!alt_selection$mp_scaled$is_degenerate) {
    mp_scaled_recovery <- compute_recovery_metrics(
      selected = alt_selection$mp_scaled$selected_model$subset_str,
      true = which(true_beta != 0),
      p_max = p_max
    )
  } else {
    mp_scaled_recovery <- list(p_star = NA, F1 = NA, precision = NA, recall = NA,
                               precision = NA, recall = NA, TP = NA, FP = NA, FN = NA,
                               hamming = NA, exact_match = FALSE)
  }
  
  # AIC
  aic_recovery <- compute_recovery_metrics(
    selected = alt_selection$aic_selected,
    true = which(true_beta != 0),
    p_max = p_max
  )
  
  # BIC
  bic_recovery <- compute_recovery_metrics(
    selected = alt_selection$bic_selected,
    true = which(true_beta != 0),
    p_max = p_max
  )
  
  # Create recovery stats dataframe
  recovery_df <- data.frame(
    rule = c("M_p", "M_p_minus", "M_p_sqrt", "M_p_scaled", "AIC", "BIC"),
    p_star = c(standard_selection$mp$p_star, 
               alt_selection$mp_minus$p_star,
               alt_selection$mp_sqrt$p_star,
               alt_selection$mp_scaled$p_star,
               alt_selection$p_star_aic,
               alt_selection$p_star_bic),
    F1 = c(mp_recovery$F1, mp_minus_recovery$F1, mp_sqrt_recovery$F1, mp_scaled_recovery$F1,
           aic_recovery$F1, bic_recovery$F1),
    precision = c(mp_recovery$precision, mp_minus_recovery$precision, mp_sqrt_recovery$precision,
                  mp_scaled_recovery$precision, aic_recovery$precision, bic_recovery$precision),
    recall = c(mp_recovery$recall, mp_minus_recovery$recall, mp_sqrt_recovery$recall,
               mp_scaled_recovery$recall, aic_recovery$recall, bic_recovery$recall)
  )
  
  write.csv(recovery_df, file.path(exp_dir, "recovery_stats.csv"), row.names = FALSE)
  
  # Generate plots
  if (save_plots) {
    source("R/visualization.R")
    
    # Add alternative metrics to best_models for plotting
    best_models_plot <- alt_selection$best_models
    best_models_plot$Mp_minus <- alt_selection$mp_minus$Mp_minus_values
    best_models_plot$Mp_sqrt <- alt_selection$mp_sqrt$Mp_sqrt_values
    best_models_plot$Mp_scaled <- alt_selection$mp_scaled$Mp_scaled_values
    
    # Plot comparison of all four M_p variants
    plot_alternative_metrics_comparison(
      best_models_plot,
      p_star_mp = standard_selection$mp$p_star,
      p_star_mp_minus = alt_selection$mp_minus$p_star,
      p_star_mp_sqrt = alt_selection$mp_sqrt$p_star,
      p_star_mp_scaled = alt_selection$mp_scaled$p_star,
      save_path = file.path(exp_dir, "plots", "mp_variants_comparison.png")
    )
    
    # Standard M_p curves
    plot_mp_curves(results, p_star = standard_selection$mp$p_star,
                   save_path = file.path(exp_dir, "plots", "mp_standard_curves.png"))
    
    # Criterion comparison
    plot_criterion_comparison(results, p_star_mp = standard_selection$mp$p_star,
                             save_path = file.path(exp_dir, "plots", "criterion_comparison.png"))
  }
  
  # Generate summary report
  generate_alternative_summary(
    config, results, standard_selection, alt_selection, 
    recovery_df, exp_dir, data$meta$p_true
  )
  
  # Return summary
  return(list(
    exp_dir = exp_dir,
    scenario = config$scenario_name,
    p_true = data$meta$p_true,
    p_max = ncol(data$X),
    mp = list(
      p_star = standard_selection$mp$p_star,
      F1 = mp_recovery$F1,
      precision = mp_recovery$precision,
      recall = mp_recovery$recall
    ),
    mp_minus = list(
      p_star = alt_selection$mp_minus$p_star,
      F1 = mp_minus_recovery$F1,
      precision = mp_minus_recovery$precision,
      recall = mp_minus_recovery$recall,
      is_degenerate = alt_selection$mp_minus$is_degenerate
    ),
    mp_sqrt = list(
      p_star = alt_selection$mp_sqrt$p_star,
      F1 = mp_sqrt_recovery$F1,
      precision = mp_sqrt_recovery$precision,
      recall = mp_sqrt_recovery$recall,
      is_degenerate = alt_selection$mp_sqrt$is_degenerate
    ),
    mp_scaled = list(
      p_star = alt_selection$mp_scaled$p_star,
      F1 = mp_scaled_recovery$F1,
      precision = mp_scaled_recovery$precision,
      recall = mp_scaled_recovery$recall,
      is_degenerate = alt_selection$mp_scaled$is_degenerate
    ),
    aic = list(
      p_star = alt_selection$p_star_aic,
      F1 = aic_recovery$F1
    ),
    bic = list(
      p_star = alt_selection$p_star_bic,
      F1 = bic_recovery$F1
    )
  ))
}


#' Run all 14 scenarios and compare metrics
run_all_alternative_comparisons <- function() {
  
  # Get all scenarios
  scenarios <- get_all_scenarios()
  
  cat(sprintf("Running %d scenarios...\n\n", length(scenarios)))
  
  results_list <- list()
  
  for (i in seq_along(scenarios)) {
    scenario_name <- names(scenarios)[i]
    config <- scenarios[[i]]
    
    cat(sprintf("[%d/%d] %s...", i, length(scenarios), scenario_name))
    
    result <- run_alternative_experiment(
      config, 
      output_dir = "alternative_metrics/results",
      save_plots = TRUE,
      verbose = FALSE
    )
    results_list[[scenario_name]] <- result
    
    cat(sprintf(" done (saved to %s)\n", result$exp_dir))
  }
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("RESULTS SUMMARY\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  # Create summary table
  summary_df <- data.frame(
    Scenario = sapply(results_list, function(x) x$scenario),
    p_true = sapply(results_list, function(x) x$p_true),
    Mp = sapply(results_list, function(x) x$mp$p_star),
    Mp_minus = sapply(results_list, function(x) {
      if (x$mp_minus$is_degenerate) NA else x$mp_minus$p_star
    }),
    Mp_scaled = sapply(results_list, function(x) {
      if (x$mp_scaled$is_degenerate) NA else x$mp_scaled$p_star
    }),
    AIC = sapply(results_list, function(x) x$aic$p_star),
    BIC = sapply(results_list, function(x) x$bic$p_star),
    F1_Mp = sapply(results_list, function(x) x$mp$F1),
    F1_Mp_minus = sapply(results_list, function(x) {
      if (x$mp_minus$is_degenerate) NA else x$mp_minus$F1
    }),
    F1_Mp_scaled = sapply(results_list, function(x) {
      if (x$mp_scaled$is_degenerate) NA else x$mp_scaled$F1
    }),
    F1_AIC = sapply(results_list, function(x) x$aic$F1),
    F1_BIC = sapply(results_list, function(x) x$bic$F1)
  )
  
  cat("Selected Complexity (p*):\n")
  print(summary_df[, 1:7])
  
  cat("\n\nF1 Scores:\n")
  print(summary_df[, c(1, 8:12)])
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("STATISTICAL SUMMARY\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  # Summary statistics
  cat("Mean Deviation from True p:\n")
  cat(sprintf("  M_p:        %.2f\n", mean(summary_df$Mp - summary_df$p_true, na.rm = TRUE)))
  cat(sprintf("  M_p_minus:  %.2f\n", mean(summary_df$Mp_minus - summary_df$p_true, na.rm = TRUE)))
  cat(sprintf("  M_p_scaled: %.2f\n", mean(summary_df$Mp_scaled - summary_df$p_true, na.rm = TRUE)))
  cat(sprintf("  AIC:        %.2f\n", mean(summary_df$AIC - summary_df$p_true, na.rm = TRUE)))
  cat(sprintf("  BIC:        %.2f\n", mean(summary_df$BIC - summary_df$p_true, na.rm = TRUE)))
  
  cat("\n")
  cat("Mean F1 Score:\n")
  cat(sprintf("  M_p:        %.3f\n", mean(summary_df$F1_Mp, na.rm = TRUE)))
  cat(sprintf("  M_p_minus:  %.3f\n", mean(summary_df$F1_Mp_minus, na.rm = TRUE)))
  cat(sprintf("  M_p_scaled: %.3f\n", mean(summary_df$F1_Mp_scaled, na.rm = TRUE)))
  cat(sprintf("  AIC:        %.3f\n", mean(summary_df$F1_AIC, na.rm = TRUE)))
  cat(sprintf("  BIC:        %.3f\n", mean(summary_df$F1_BIC, na.rm = TRUE)))
  
  cat("\n")
  cat("Median F1 Score:\n")
  cat(sprintf("  M_p:        %.3f\n", median(summary_df$F1_Mp, na.rm = TRUE)))
  cat(sprintf("  M_p_minus:  %.3f\n", median(summary_df$F1_Mp_minus, na.rm = TRUE)))
  cat(sprintf("  M_p_scaled: %.3f\n", median(summary_df$F1_Mp_scaled, na.rm = TRUE)))
  cat(sprintf("  AIC:        %.3f\n", median(summary_df$F1_AIC, na.rm = TRUE)))
  cat(sprintf("  BIC:        %.3f\n", median(summary_df$F1_BIC, na.rm = TRUE)))
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("INTERPRETATION\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  cat("M_p_minus = R²/(p-1):\n")
  cat("  - \"First predictor is free\" logic\n")
  if (mean(summary_df$Mp_minus - summary_df$p_true, na.rm = TRUE) > 
      mean(summary_df$Mp - summary_df$p_true, na.rm = TRUE)) {
    cat("  - Result: OVER-selects compared to M_p (as expected)\n")
  } else {
    cat("  - Result: Still under-selects (surprising!)\n")
  }
  
  cat("\nM_p_scaled = R²·p_max/p:\n")
  cat("  - Scales M_p by problem size p_max\n")
  cat("  - This is just M_p multiplied by constant p_max\n")
  cat("  - Result: IDENTICAL selections to M_p (ordering preserved)\n")
  
  cat("\n\n")
  
  # Save results
  output_file <- "alternative_metrics/comparison_results.csv"
  write.csv(summary_df, output_file, row.names = FALSE)
  cat(sprintf("Results saved to: %s\n\n", output_file))
  
  return(invisible(list(
    summary = summary_df,
    details = results_list
  )))
}


# Run if executed as script
if (!interactive()) {
  results <- run_all_alternative_comparisons()
} else {
  cat("To run the comparison, execute:\n")
  cat("  results <- run_all_alternative_comparisons()\n\n")
}
