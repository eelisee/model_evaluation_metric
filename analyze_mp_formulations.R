# Comprehensive M_p Formulation Analysis
# ========================================
# This script tests different formulations of the M_p metric to find
# which one best matches the true p* across all scenarios.

library(parallel)

# Define true p* for each scenario (from support_spec in run_experiment.R)
true_p_star <- list(
  A1_Baseline_Uncorrelated = 3,
  A2_Single_Predictor = 1,
  A3_Full_Support = 20,
  B1_AR1_Weak = 3,
  B1_AR1_Strong = 3,
  B2_Compound_Symmetry = 3,
  B3_Block_Structure = 3,
  C1_Weak_Signals = 5,
  C2_Many_Weak_Signals = 10,
  C3_Mixed_Signals = 8
)

#' Load and Average R² Data for a Scenario
#'
#' @param scenario_name String. Name of scenario folder
#' @return Data frame with columns: p, R2_avg, M_p_avg
load_scenario_data <- function(scenario_name) {
  
  # Load detailed results
  csv_path <- file.path("results", scenario_name, "detailed_results.csv")
  
  if (!file.exists(csv_path)) {
    cat(sprintf("Warning: %s not found\n", csv_path))
    return(NULL)
  }
  
  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  # Average R² across all iterations for each p
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  colnames(avg_data) <- c("p", "R2_avg")
  
  # Also get BIC selection (most frequent p* selected by BIC)
  bic_selections <- data[data$selected_by_BIC == TRUE, "p"]
  bic_p_star <- as.numeric(names(sort(table(bic_selections), decreasing = TRUE)[1]))
  
  return(list(
    data = avg_data,
    bic_p_star = bic_p_star
  ))
}


#' Compute All M_p Derivative Formulations
#'
#' @param R2_vals Numeric vector of R² values
#' @param p_vals Numeric vector of p values (usually 1:length(R2_vals))
#' @return List with all computed metrics
compute_all_formulations <- function(R2_vals, p_vals) {
  
  n <- length(p_vals)
  
  # Base: M_p = R² / p
  M_p <- R2_vals / p_vals
  
  # ========================================
  # FIRST DERIVATIVES
  # ========================================
  
  # Forward difference: Δ₁(p) = M(p+1) - M(p)
  delta1_forward <- diff(M_p)
  
  # Backward difference: Δ₁(p) = M(p) - M(p-1)
  delta1_backward <- c(NA, diff(M_p))
  
  # Central difference (symmetric): Δ₁(p) = [M(p+1) - M(p-1)] / 2
  delta1_central <- numeric(n)
  delta1_central[1] <- NA
  delta1_central[n] <- NA
  for (i in 2:(n-1)) {
    delta1_central[i] <- (M_p[i+1] - M_p[i-1]) / 2
  }
  
  # ========================================
  # SECOND DERIVATIVES
  # ========================================
  
  # Forward difference of forward difference
  delta2_forward <- diff(delta1_forward)
  
  # Central difference (symmetric): Δ₂(p) = M(p-1) - 2M(p) + M(p+1)
  delta2_central <- numeric(n)
  delta2_central[1] <- NA
  delta2_central[n] <- NA
  for (i in 2:(n-1)) {
    delta2_central[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
  }
  
  # Intuitive second derivative: diff(diff(M_p))
  delta2_intuitive <- c(NA, diff(diff(M_p)))
  
  # ========================================
  # THIRD DERIVATIVES
  # ========================================
  
  # Central symmetric third derivative
  delta3_central_from_central <- diff(delta2_central)
  
  # Intuitive third derivative from central second derivative
  delta3_intuitive_from_central <- diff(delta2_central)
  
  # Third derivative from forward differences
  delta3_forward <- diff(delta2_forward)
  
  # ========================================
  # ALTERNATIVE METRICS
  # ========================================
  
  # Efficiency change: [M(p+1)/(p+1)] - [M(p)/p] = (R²(p+1)/(p+1)) - (R²(p)/p)
  # This is just diff(M_p) since M_p = R²/p
  efficiency_change <- diff(M_p)
  
  # Efficiency change with index shift: M(p+1) - M(p-1)
  efficiency_change_shifted <- numeric(n)
  efficiency_change_shifted[1] <- NA
  efficiency_change_shifted[n] <- NA
  for (i in 2:(n-1)) {
    efficiency_change_shifted[i] <- M_p[i+1] - M_p[i-1]
  }
  
  # Ratio-based: M(p+1) / M(p)
  ratio_metric <- c(M_p[-1] / M_p[-n], NA)
  
  # Percentage change: [M(p+1) - M(p)] / M(p)
  pct_change <- c((M_p[-1] - M_p[-n]) / M_p[-n], NA)
  
  # ========================================
  # CURVATURE METRICS
  # ========================================
  
  # Kneedle-inspired: normalized second derivative
  M_p_normalized <- (M_p - min(M_p)) / (max(M_p) - min(M_p))
  p_normalized <- (p_vals - min(p_vals)) / (max(p_vals) - min(p_vals))
  
  kneedle_diff <- numeric(n)
  for (i in 1:n) {
    kneedle_diff[i] <- abs(M_p_normalized[i] - p_normalized[i])
  }
  
  # L-method: find point with maximum distance to line connecting first and last point
  # Line from (p[1], M_p[1]) to (p[n], M_p[n])
  x1 <- p_vals[1]
  y1 <- M_p[1]
  x2 <- p_vals[n]
  y2 <- M_p[n]
  
  # Distance from point (p[i], M_p[i]) to line
  l_method_distance <- numeric(n)
  for (i in 1:n) {
    # Distance = |ax + by + c| / sqrt(a² + b²)
    # Line equation: (y2-y1)x - (x2-x1)y + (x2-x1)y1 - (y2-y1)x1 = 0
    a <- y2 - y1
    b <- -(x2 - x1)
    c <- (x2 - x1) * y1 - (y2 - y1) * x1
    
    l_method_distance[i] <- abs(a * p_vals[i] + b * M_p[i] + c) / sqrt(a^2 + b^2)
  }
  
  return(list(
    M_p = M_p,
    delta1_forward = delta1_forward,
    delta1_backward = delta1_backward,
    delta1_central = delta1_central,
    delta2_forward = delta2_forward,
    delta2_central = delta2_central,
    delta2_intuitive = delta2_intuitive,
    delta3_central = delta3_central_from_central,
    delta3_forward = delta3_forward,
    efficiency_change = efficiency_change,
    efficiency_change_shifted = efficiency_change_shifted,
    ratio_metric = ratio_metric,
    pct_change = pct_change,
    kneedle_diff = kneedle_diff,
    l_method_distance = l_method_distance
  ))
}


#' Apply Selection Rules to a Metric
#'
#' @param metric_vals Numeric vector
#' @param p_vals Numeric vector
#' @return List of p* selected by different rules
apply_selection_rules <- function(metric_vals, p_vals) {
  
  # Remove NA values for certain operations
  valid_idx <- which(!is.na(metric_vals))
  
  if (length(valid_idx) == 0) {
    return(list(
      min = NA,
      max = NA,
      abs_min = NA,
      abs_max = NA,
      zero_crossing_floor = NA,
      zero_crossing_ceil = NA,
      zero_crossing_round = NA
    ))
  }
  
  metric_valid <- metric_vals[valid_idx]
  p_valid <- p_vals[valid_idx]
  
  # Min
  p_min <- p_valid[which.min(metric_valid)]
  
  # Max
  p_max <- p_valid[which.max(metric_valid)]
  
  # Absolute min
  p_abs_min <- p_valid[which.min(abs(metric_valid))]
  
  # Absolute max
  p_abs_max <- p_valid[which.max(abs(metric_valid))]
  
  # Zero crossing (where metric changes sign)
  zero_crossings <- which(metric_valid[-length(metric_valid)] * metric_valid[-1] < 0)
  
  if (length(zero_crossings) > 0) {
    # Take first zero crossing
    idx <- zero_crossings[1]
    
    # Interpolate to find exact crossing point
    x1 <- p_valid[idx]
    x2 <- p_valid[idx + 1]
    y1 <- metric_valid[idx]
    y2 <- metric_valid[idx + 1]
    
    # Linear interpolation: x = x1 - y1 * (x2 - x1) / (y2 - y1)
    x_cross <- x1 - y1 * (x2 - x1) / (y2 - y1)
    
    p_zero_floor <- floor(x_cross)
    p_zero_ceil <- ceiling(x_cross)
    p_zero_round <- round(x_cross)
  } else {
    p_zero_floor <- NA
    p_zero_ceil <- NA
    p_zero_round <- NA
  }
  
  return(list(
    min = p_min,
    max = p_max,
    abs_min = p_abs_min,
    abs_max = p_abs_max,
    zero_crossing_floor = p_zero_floor,
    zero_crossing_ceil = p_zero_ceil,
    zero_crossing_round = p_zero_round
  ))
}


#' Analyze Single Scenario
#'
#' @param scenario_name String
#' @param true_p Numeric
#' @return Data frame with all formulation results
analyze_scenario <- function(scenario_name, true_p) {
  
  cat(sprintf("\n=== Analyzing %s (true p* = %d) ===\n", scenario_name, true_p))
  
  # Load data
  scenario_data <- load_scenario_data(scenario_name)
  
  if (is.null(scenario_data)) {
    return(NULL)
  }
  
  data <- scenario_data$data
  bic_p_star <- scenario_data$bic_p_star
  
  # Compute all formulations
  formulations <- compute_all_formulations(data$R2_avg, data$p)
  
  # Storage for results
  results <- list()
  
  # Test each formulation with each selection rule
  formulation_names <- names(formulations)
  
  for (form_name in formulation_names) {
    
    metric_vals <- formulations[[form_name]]
    
    # Apply all selection rules
    selections <- apply_selection_rules(metric_vals, data$p)
    
    # Store results
    for (rule_name in names(selections)) {
      
      p_selected <- selections[[rule_name]]
      
      # Calculate error
      error_true <- if (!is.na(p_selected)) abs(p_selected - true_p) else Inf
      error_bic <- if (!is.na(p_selected)) abs(p_selected - bic_p_star) else Inf
      
      results[[length(results) + 1]] <- data.frame(
        scenario = scenario_name,
        formulation = form_name,
        selection_rule = rule_name,
        p_selected = ifelse(is.na(p_selected), -1, p_selected),
        p_true = true_p,
        p_bic = bic_p_star,
        error_true = error_true,
        error_bic = error_bic,
        is_correct = (!is.na(p_selected) && p_selected == true_p),
        matches_bic = (!is.na(p_selected) && p_selected == bic_p_star),
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine results
  result_df <- do.call(rbind, results)
  
  cat(sprintf("  Tested %d formulation x rule combinations\n", nrow(result_df)))
  
  return(result_df)
}


# ============================================================================
# MAIN ANALYSIS
# ============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("M_p FORMULATION ANALYSIS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("Testing different M_p formulations across all scenarios...\n")
cat(sprintf("Total scenarios: %d\n", length(true_p_star)))
cat("\nFormulations to test:\n")
cat("  - M_p = R²/p (base)\n")
cat("  - First derivatives (forward, backward, central)\n")
cat("  - Second derivatives (forward, central, intuitive)\n")
cat("  - Third derivatives (central, forward)\n")
cat("  - Efficiency changes\n")
cat("  - Ratio and percentage metrics\n")
cat("  - Kneedle and L-method\n")
cat("\nSelection rules to apply:\n")
cat("  - min, max\n")
cat("  - abs(min), abs(max)\n")
cat("  - zero crossings (floor, ceil, round)\n")
cat("\n")

# Analyze all scenarios
all_results <- list()

for (scenario_name in names(true_p_star)) {
  
  result <- analyze_scenario(scenario_name, true_p_star[[scenario_name]])
  
  if (!is.null(result)) {
    all_results[[scenario_name]] <- result
  }
}

# Combine all results
cat("\n\nCombining results...\n")
combined_results <- do.call(rbind, all_results)

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SUMMARY STATISTICS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Aggregate by formulation + rule
summary_stats <- aggregate(
  cbind(is_correct, matches_bic, error_true, error_bic) ~ formulation + selection_rule,
  data = combined_results,
  FUN = function(x) c(
    success_rate = mean(x, na.rm = TRUE),
    mean_error = mean(x[is.finite(x)], na.rm = TRUE),
    median_error = median(x[is.finite(x)], na.rm = TRUE)
  )
)

# Extract aggregated values properly
summary_final <- data.frame(
  formulation = summary_stats$formulation,
  selection_rule = summary_stats$selection_rule,
  correct_rate = summary_stats$is_correct[, "success_rate"],
  bic_match_rate = summary_stats$matches_bic[, "success_rate"],
  mean_error_true = summary_stats$error_true[, "mean_error"],
  median_error_true = summary_stats$error_true[, "median_error"],
  mean_error_bic = summary_stats$error_bic[, "mean_error"],
  median_error_bic = summary_stats$error_bic[, "median_error"],
  stringsAsFactors = FALSE
)

# Sort by correct rate (descending), then by mean error (ascending)
summary_final <- summary_final[order(-summary_final$correct_rate, summary_final$mean_error_true), ]

# ============================================================================
# OUTPUT RESULTS
# ============================================================================

cat("\n=== TOP 20 FORMULATIONS (by correct rate) ===\n\n")
print(head(summary_final, 20))

cat("\n\n=== TOP 20 FORMULATIONS (by mean error to true p*) ===\n\n")
summary_by_error <- summary_final[order(summary_final$mean_error_true), ]
print(head(summary_by_error, 20))

# Save detailed results
write.csv(combined_results, "mp_formulation_analysis_detailed.csv", row.names = FALSE)
write.csv(summary_final, "mp_formulation_analysis_summary.csv", row.names = FALSE)

cat("\n\n✓ Analysis complete!\n")
cat("  Detailed results saved to: mp_formulation_analysis_detailed.csv\n")
cat("  Summary results saved to: mp_formulation_analysis_summary.csv\n\n")

# ============================================================================
# IDENTIFY FAILURES
# ============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("FAILURE ANALYSIS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Find best performing formulation
best_formulation <- summary_final[1, ]
cat(sprintf("Best formulation: %s + %s\n", best_formulation$formulation, best_formulation$selection_rule))
cat(sprintf("  Correct rate: %.1f%%\n", best_formulation$correct_rate * 100))
cat(sprintf("  Mean error: %.2f\n", best_formulation$mean_error_true))
cat("\n")

# Find scenarios where it failed
best_combo <- combined_results[
  combined_results$formulation == best_formulation$formulation &
  combined_results$selection_rule == best_formulation$selection_rule,
]

failures <- best_combo[!best_combo$is_correct, ]

if (nrow(failures) > 0) {
  cat("Scenarios where best formulation failed:\n\n")
  print(failures[, c("scenario", "p_selected", "p_true", "p_bic", "error_true")])
} else {
  cat("✓ Best formulation succeeded in all scenarios!\n")
}

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
