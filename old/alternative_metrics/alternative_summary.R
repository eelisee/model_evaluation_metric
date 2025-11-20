# Summary Report Generation for Alternative Metrics
# ==================================================

#' Generate summary report for alternative metrics experiment
#'
#' @param config Scenario configuration
#' @param results Full model results
#' @param standard_selection Standard M_p selection results
#' @param alt_selection Alternative metrics selection results
#' @param recovery_df Recovery metrics dataframe
#' @param exp_dir Experiment directory
#' @param p_true True number of active predictors
generate_alternative_summary <- function(config, results, standard_selection, 
                                        alt_selection, recovery_df, exp_dir, p_true) {
  
  summary_file <- file.path(exp_dir, "summary.txt")
  
  sink(summary_file)
  
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("ALTERNATIVE METRICS EXPERIMENT SUMMARY\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  cat(sprintf("SCENARIO: %s\n", config$scenario_name))
  cat(sprintf("Description: %s\n\n", config$scenario_description))
  
  cat("CONFIGURATION\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  cat(sprintf("  Sample size (n): %d\n", config$n))
  cat(sprintf("  Predictors (p_max): %d\n", config$p_max))
  cat(sprintf("  True active predictors (p_true): %d\n", p_true))
  cat(sprintf("  Noise level (sigma_eps): %.3f\n", config$sigma_eps))
  cat(sprintf("  Random seed: %d\n\n", config$seed))
  
  cat("MODELS EVALUATED\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  cat(sprintf("  Total models: %d\n", nrow(results)))
  cat(sprintf("  R² range: [%.4f, %.4f]\n", min(results$R2), max(results$R2)))
  cat(sprintf("  M_p range: [%.4f, %.4f]\n\n", min(results$Mp), max(results$Mp)))
  
  cat("MODEL SELECTION RESULTS\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  cat("Standard M_p (R²/p):\n")
  cat(sprintf("  p* = %d\n", standard_selection$mp$p_star))
  cat(sprintf("  Selected: %s\n\n", standard_selection$mp$selected_model$subset_str))
  
  cat("M_p_minus (R²/(p-1)):\n")
  if (alt_selection$mp_minus$is_degenerate) {
    cat("  DEGENERATE - constant values\n\n")
  } else {
    cat(sprintf("  p* = %d\n", alt_selection$mp_minus$p_star))
    cat(sprintf("  Selected: %s\n", alt_selection$mp_minus$selected_model$subset_str))
    cat(sprintf("  Diagnostics: %s\n\n", alt_selection$mp_minus$diagnostics))
  }
  
  cat("M_p_sqrt (R²/√p):\n")
  if (alt_selection$mp_sqrt$is_degenerate) {
    cat("  DEGENERATE - constant values\n\n")
  } else {
    cat(sprintf("  p* = %d\n", alt_selection$mp_sqrt$p_star))
    cat(sprintf("  Selected: %s\n", alt_selection$mp_sqrt$selected_model$subset_str))
    cat(sprintf("  Diagnostics: %s\n\n", alt_selection$mp_sqrt$diagnostics))
  }
  
  cat("M_p_scaled (R²·p_max/p):\n")
  if (alt_selection$mp_scaled$is_degenerate) {
    cat("  DEGENERATE - constant values\n\n")
  } else {
    cat(sprintf("  p* = %d\n", alt_selection$mp_scaled$p_star))
    cat(sprintf("  Selected: %s\n", alt_selection$mp_scaled$selected_model$subset_str))
    cat(sprintf("  Diagnostics: %s\n\n", alt_selection$mp_scaled$diagnostics))
  }
  
  cat("Comparison Methods:\n")
  cat(sprintf("  AIC: p* = %d, Selected: %s\n", 
              alt_selection$p_star_aic, alt_selection$aic_selected))
  cat(sprintf("  BIC: p* = %d, Selected: %s\n\n", 
              alt_selection$p_star_bic, alt_selection$bic_selected))
  
  cat("True Model:\n")
  cat(sprintf("  p_true = %d\n\n", p_true))
  
  cat("BEST MODELS BY CARDINALITY\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  best_display <- alt_selection$best_models[, c("p", "R2", "Mp", "AIC", "BIC", "subset_str")]
  print(best_display, row.names = FALSE)
  cat("\n")
  
  cat("VARIABLE SELECTION PERFORMANCE\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  for (i in 1:nrow(recovery_df)) {
    rule_name <- recovery_df$rule[i]
    cat(sprintf("%s:\n", rule_name))
    cat(sprintf("  p* = %s (true: %d, diff: %+d)\n", 
                ifelse(is.na(recovery_df$p_star[i]), "NA", as.character(recovery_df$p_star[i])),
                p_true,
                ifelse(is.na(recovery_df$p_star[i]), NA, recovery_df$p_star[i] - p_true)))
    
    if (!is.na(recovery_df$F1[i])) {
      cat(sprintf("  Precision: %.3f, Recall: %.3f, F1: %.3f\n\n",
                  recovery_df$precision[i], recovery_df$recall[i], recovery_df$F1[i]))
    } else {
      cat("  Metrics: NA (degenerate case)\n\n")
    }
  }
  
  cat("INTERPRETATION\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  # Compare M_p_minus to M_p
  if (!alt_selection$mp_minus$is_degenerate) {
    diff_minus <- alt_selection$mp_minus$p_star - standard_selection$mp$p_star
    if (diff_minus > 0) {
      cat(sprintf("M_p_minus OVER-selects by %d compared to M_p\n", diff_minus))
    } else if (diff_minus < 0) {
      cat(sprintf("M_p_minus UNDER-selects by %d compared to M_p\n", abs(diff_minus)))
    } else {
      cat("M_p_minus selects SAME complexity as M_p\n")
    }
  }
  
  # Compare M_p_sqrt to M_p
  if (!alt_selection$mp_sqrt$is_degenerate) {
    diff_sqrt <- alt_selection$mp_sqrt$p_star - standard_selection$mp$p_star
    if (diff_sqrt > 0) {
      cat(sprintf("M_p_sqrt OVER-selects by %d compared to M_p (sublinear penalty)\n", diff_sqrt))
    } else if (diff_sqrt < 0) {
      cat(sprintf("M_p_sqrt UNDER-selects by %d compared to M_p\n", abs(diff_sqrt)))
    } else {
      cat("M_p_sqrt selects SAME complexity as M_p\n")
    }
  }
  
  # Compare M_p_scaled to M_p
  if (alt_selection$mp_scaled$p_star == standard_selection$mp$p_star) {
    cat("M_p_scaled selects IDENTICAL complexity to M_p (as expected - scale invariant)\n")
  } else {
    cat("WARNING: M_p_scaled differs from M_p (unexpected!)\n")
  }
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("End of summary\n")
  
  sink()
}
