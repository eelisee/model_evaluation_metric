#!/usr/bin/env Rscript

# Test alternative efficiency metrics beyond M_p = R²/p
# Goal: Find formulations with clearer elbows/inflection points

load_scenario <- function(scenario_name) {
  file_path <- sprintf("results/%s/detailed_results.csv", scenario_name)
  data <- read.csv(file_path)
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  names(avg_data)[2] <- "R2_mean"
  avg_data <- avg_data[order(avg_data$p), ]
  return(avg_data)
}

# Compute derivatives with proper boundaries
compute_derivatives <- function(values) {
  n <- length(values)
  
  delta1 <- numeric(n)
  delta1[1] <- values[2] - values[1]
  delta1[n] <- values[n] - values[n-1]
  for (i in 2:(n-1)) {
    delta1[i] <- (values[i+1] - values[i-1]) / 2
  }
  
  delta2 <- numeric(n)
  delta2[1] <- delta1[2] - delta1[1]
  delta2[n] <- delta1[n] - delta1[n-1]
  for (i in 2:(n-1)) {
    delta2[i] <- values[i-1] - 2*values[i] + values[i+1]
  }
  
  delta3 <- numeric(n)
  delta3[1] <- delta2[2] - delta2[1]
  delta3[n] <- delta2[n] - delta2[n-1]
  for (i in 2:(n-1)) {
    delta3[i] <- delta2[i+1] - delta2[i]
  }
  
  return(list(delta1 = delta1, delta2 = delta2, delta3 = delta3))
}

# Alternative formulations
compute_alternatives <- function(R2_vals, p_vals) {
  n <- length(p_vals)
  
  # Original
  M_p_original <- R2_vals / p_vals
  
  # 1. Log penalty: R² / log(p + 1)
  # Idea: Logarithmic penalty grows slower, might emphasize small p less
  M_p_log <- R2_vals / log(p_vals + 1)
  
  # 2. Square root penalty: R² / sqrt(p)
  # Idea: Between linear and log, sublinear growth
  M_p_sqrt <- R2_vals / sqrt(p_vals)
  
  # 3. Incremental gain: (R²[p] - R²[p-1])
  # Idea: Marginal improvement, should drop sharply after true p*
  incremental_gain <- c(R2_vals[1], diff(R2_vals))
  
  # 4. Normalized incremental: (R²[p] - R²[p-1]) / p
  # Idea: Incremental gain per predictor
  norm_incremental <- incremental_gain / p_vals
  
  # 5. Efficiency ratio: R²[p] / (p * (1 - R²[p]))
  # Idea: Penalize both by p and by unexplained variance
  efficiency_ratio <- R2_vals / (p_vals * (1 - R2_vals + 1e-10))
  
  # 6. Information gain: log(1/(1-R²)) / p
  # Idea: Information theoretic approach
  info_gain <- log(1 / (1 - R2_vals + 1e-10)) / p_vals
  
  # 7. Adjusted R²-like: (R² - p/n) / p where n = sample size (assume 500)
  # Idea: Penalize model complexity like adjusted R²
  n_samples <- 500
  adj_efficiency <- (R2_vals - p_vals/n_samples) / p_vals
  
  # 8. Diminishing returns: R²^2 / p
  # Idea: Non-linear transformation emphasizes high R²
  diminishing <- (R2_vals^2) / p_vals
  
  # 9. Exponential penalty: R² * exp(-p/10)
  # Idea: Exponential decay for complexity
  exp_penalty <- R2_vals * exp(-p_vals/10)
  
  # 10. Curvature-based: -second derivative of R² curve
  # Idea: Find where R² curve flattens (maximum curvature)
  R2_deriv2 <- numeric(n)
  for (i in 2:(n-1)) {
    R2_deriv2[i] <- R2_vals[i-1] - 2*R2_vals[i] + R2_vals[i+1]
  }
  R2_deriv2[1] <- R2_vals[1] - 2*R2_vals[2] + R2_vals[3]
  R2_deriv2[n] <- R2_vals[n-2] - 2*R2_vals[n-1] + R2_vals[n]
  
  return(list(
    original = M_p_original,
    log_penalty = M_p_log,
    sqrt_penalty = M_p_sqrt,
    incremental = incremental_gain,
    norm_incremental = norm_incremental,
    efficiency_ratio = efficiency_ratio,
    info_gain = info_gain,
    adj_efficiency = adj_efficiency,
    diminishing = diminishing,
    exp_penalty = exp_penalty,
    R2_curvature = -R2_deriv2  # Negative so we look for maximum
  ))
}

# Test selection using argmin(delta3) + 1
test_formulation <- function(values, p_vals, true_p, name) {
  derivs <- compute_derivatives(values)
  
  # Try multiple selection methods
  argmin_idx <- which.min(derivs$delta3)
  argmax_idx <- which.max(derivs$delta3)
  
  # Method 1: argmin(delta3) + 1
  selected_min <- ifelse(argmin_idx < length(p_vals), 
                         p_vals[argmin_idx + 1], 
                         p_vals[length(p_vals)])
  
  # Method 2: argmax(delta3) (for formulations where peak matters)
  selected_max <- p_vals[argmax_idx]
  
  # Method 3: Just argmax of the metric itself
  selected_argmax_metric <- p_vals[which.max(values)]
  
  correct_min <- selected_min == true_p
  correct_max <- selected_max == true_p
  correct_argmax <- selected_argmax_metric == true_p
  
  return(list(
    name = name,
    selected_min = selected_min,
    selected_max = selected_max,
    selected_argmax = selected_argmax_metric,
    correct_min = correct_min,
    correct_max = correct_max,
    correct_argmax = correct_argmax,
    max_value = max(values, na.rm = TRUE),
    min_value = min(values, na.rm = TRUE)
  ))
}

# Main analysis
cat("================================================================================\n")
cat("TESTING ALTERNATIVE EFFICIENCY METRICS\n")
cat("================================================================================\n\n")

true_p_star <- list(
  "A1_Baseline_Uncorrelated" = 3,
  "A2_Single_Predictor" = 1,
  "A3_Full_Support" = 20,
  "B1_AR1_Weak" = 3,
  "B1_AR1_Strong" = 3,
  "B2_Compound_Symmetry" = 3,
  "B3_Block_Structure" = 3,
  "C1_Weak_Signals" = 5,
  "C2_Many_Weak_Signals" = 10,
  "C3_Mixed_Signals" = 8
)

# Store results
results_summary <- data.frame(
  Formulation = character(),
  ArgminPlus1_Success = integer(),
  Argmax_Success = integer(),
  ArgmaxMetric_Success = integer(),
  stringsAsFactors = FALSE
)

formulation_names <- c("original", "log_penalty", "sqrt_penalty", "incremental", 
                       "norm_incremental", "efficiency_ratio", "info_gain", 
                       "adj_efficiency", "diminishing", "exp_penalty", "R2_curvature")

# Initialize counters
success_counts <- matrix(0, nrow = length(formulation_names), ncol = 3)
rownames(success_counts) <- formulation_names
colnames(success_counts) <- c("argmin+1", "argmax_delta3", "argmax_metric")

# Test each scenario
for (scenario in names(true_p_star)) {
  cat(sprintf("\n%s (p*=%d):\n", scenario, true_p_star[[scenario]]))
  
  data <- load_scenario(scenario)
  p_vals <- data$p
  R2_vals <- data$R2_mean
  true_p <- true_p_star[[scenario]]
  
  # Compute all alternatives
  alts <- compute_alternatives(R2_vals, p_vals)
  
  # Test each formulation
  for (i in 1:length(formulation_names)) {
    form_name <- formulation_names[i]
    values <- alts[[form_name]]
    
    # Handle potential NA/Inf values
    if (any(is.na(values)) || any(is.infinite(values))) {
      cat(sprintf("  %s: SKIPPED (NA/Inf values)\n", form_name))
      next
    }
    
    result <- test_formulation(values, p_vals, true_p, form_name)
    
    if (result$correct_min) success_counts[form_name, 1] <- success_counts[form_name, 1] + 1
    if (result$correct_max) success_counts[form_name, 2] <- success_counts[form_name, 2] + 1
    if (result$correct_argmax) success_counts[form_name, 3] <- success_counts[form_name, 3] + 1
    
    cat(sprintf("  %-20s: argmin+1=%2d %s | argmax_Δ3=%2d %s | argmax=%2d %s\n",
                form_name,
                result$selected_min, ifelse(result$correct_min, "✓", "✗"),
                result$selected_max, ifelse(result$correct_max, "✓", "✗"),
                result$selected_argmax, ifelse(result$correct_argmax, "✓", "✗")))
  }
}

# Summary
cat("\n\n")
cat("================================================================================\n")
cat("SUMMARY (Success Rate across 10 scenarios)\n")
cat("================================================================================\n\n")

summary_df <- data.frame(
  Formulation = formulation_names,
  ArgminPlus1 = sprintf("%d/10 (%.0f%%)", success_counts[,1], success_counts[,1]*10),
  ArgmaxDelta3 = sprintf("%d/10 (%.0f%%)", success_counts[,2], success_counts[,2]*10),
  ArgmaxMetric = sprintf("%d/10 (%.0f%%)", success_counts[,3], success_counts[,3]*10),
  stringsAsFactors = FALSE
)

print(summary_df)

cat("\n\nFormulation descriptions:\n")
cat("  original:         R² / p\n")
cat("  log_penalty:      R² / log(p + 1)\n")
cat("  sqrt_penalty:     R² / sqrt(p)\n")
cat("  incremental:      R²[p] - R²[p-1] (marginal gain)\n")
cat("  norm_incremental: (R²[p] - R²[p-1]) / p\n")
cat("  efficiency_ratio: R² / (p * (1 - R²))\n")
cat("  info_gain:        log(1/(1-R²)) / p\n")
cat("  adj_efficiency:   (R² - p/n) / p\n")
cat("  diminishing:      R²² / p\n")
cat("  exp_penalty:      R² * exp(-p/10)\n")
cat("  R2_curvature:     -Δ₂[R²] (maximum curvature of R² curve)\n")

cat("\n\nSelection methods:\n")
cat("  argmin+1:      argmin(Δ₃) + 1 (current best approach)\n")
cat("  argmax_delta3: argmax(Δ₃) (for metrics where peak indicates optimum)\n")
cat("  argmax_metric: argmax(metric) directly (no derivatives)\n")
