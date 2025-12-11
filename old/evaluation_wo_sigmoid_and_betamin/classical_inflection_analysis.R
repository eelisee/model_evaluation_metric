#!/usr/bin/env Rscript

# Classical Inflection Point Analysis
# ====================================
# Implements the classical definition:
#   "Find the smallest index p where delta2 descends (delta2[p] < delta2[p-1])
#    and is immediately followed by an ascent (delta2[p+1] > delta2[p])"
#
# Edge cases:
#   - No descent exists: p* = 1 (boundary optimum at start)
#   - No ascent after descent: p* = 20 (boundary optimum at end)

library(ggplot2)
library(gridExtra)

# Load scenario data
load_scenario <- function(scenario_name) {
  file_path <- sprintf("results/%s/detailed_results.csv", scenario_name)
  if (!file.exists(file_path)) {
    stop(sprintf("File not found: %s", file_path))
  }
  data <- read.csv(file_path)
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  names(avg_data)[2] <- "R2_mean"
  avg_data <- avg_data[order(avg_data$p), ]
  return(avg_data)
}

# Get true p* from scenario name
get_true_p <- function(scenario_name) {
  true_p_map <- list(
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
  return(true_p_map[[scenario_name]])
}

# Compute derivatives with proper boundary conditions
compute_derivatives <- function(M_p) {
  n <- length(M_p)
  
  # First derivative (central differences with forward/backward at boundaries)
  delta1 <- numeric(n)
  delta1[1] <- M_p[2] - M_p[1]  # Forward
  delta1[n] <- M_p[n] - M_p[n-1]  # Backward
  for (i in 2:(n-1)) {
    delta1[i] <- (M_p[i+1] - M_p[i-1]) / 2  # Central
  }
  
  # Second derivative from delta1
  delta2 <- numeric(n)
  delta2[1] <- delta1[2] - delta1[1]  # Via first derivative
  delta2[n] <- delta1[n] - delta1[n-1]  # Via first derivative
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]  # Standard central
  }
  
  # Third derivative
  delta3 <- numeric(n)
  delta3[1] <- delta2[2] - delta2[1]
  delta3[n] <- delta2[n] - delta2[n-1]
  for (i in 2:(n-1)) {
    delta3[i] <- (delta2[i+1] - delta2[i-1]) / 2
  }
  
  return(list(delta1 = delta1, delta2 = delta2, delta3 = delta3))
}

# Classical inflection point detection
# Find smallest p where delta2 changes sign (true inflection point)
classical_inflection <- function(delta2) {
  n <- length(delta2)
  
  # Look for TRUE sign change: delta2 changes from positive to negative or vice versa
  # This is the mathematically rigorous definition of an inflection point
  
  for (p in 1:(n-1)) {
    # Check for sign change: different signs (one positive, one negative)
    if (sign(delta2[p]) * sign(delta2[p+1]) < 0) {
      # True inflection point detected
      return(list(
        p_star = p + 1,  # Select the point after sign change
        method = "inflection_detected",
        details = sprintf("Sign change: delta2[%d]=%.8f -> delta2[%d]=%.8f",
                         p, delta2[p], p+1, delta2[p+1])
      ))
    }
  }
  
  # No sign change found - check if delta2 is uniformly positive or negative
  
  if (all(delta2 > 0)) {
    # All positive: M_p is purely convex (concave down)
    # Maximum must be at p=1
    return(list(
      p_star = 1,
      method = "no_inflection_convex",
      details = "delta2 > 0 everywhere (M_p purely convex), maximum at p=1"
    ))
  }
  
  if (all(delta2 < 0)) {
    # All negative: M_p is purely concave (concave up)
    # Maximum could be at p=n
    return(list(
      p_star = n,
      method = "no_inflection_concave",
      details = "delta2 < 0 everywhere (M_p purely concave), maximum at p=n"
    ))
  }
  
  # Mixed signs but no clear inflection (shouldn't happen with proper boundaries)
  return(list(
    p_star = 1,
    method = "no_clear_pattern",
    details = "No clear inflection pattern detected, defaulting to p=1"
  ))
}

# Alternative: argmin of delta3 (for comparison)
argmin_delta3_method <- function(delta3) {
  argmin <- which.min(delta3)
  return(list(
    p_star = argmin,
    method = "argmin_delta3"
  ))
}

# Analyze all scenarios
scenarios <- c(
  "A1_Baseline_Uncorrelated",
  "A2_Single_Predictor", 
  "A3_Full_Support",
  "B1_AR1_Weak",
  "B1_AR1_Strong",
  "B2_Compound_Symmetry",
  "B3_Block_Structure",
  "C1_Weak_Signals",
  "C2_Many_Weak_Signals",
  "C3_Mixed_Signals"
)

results_summary <- data.frame(
  scenario = character(),
  p_true = integer(),
  p_classical = integer(),
  method_classical = character(),
  p_argmin_delta3 = integer(),
  p_argmin_delta3_plus1 = integer(),
  correct_classical = logical(),
  correct_argmin = logical(),
  correct_argmin_plus1 = logical(),
  stringsAsFactors = FALSE
)

cat("\n")
cat("========================================\n")
cat("CLASSICAL INFLECTION POINT ANALYSIS\n")
cat("========================================\n\n")

for (scenario_name in scenarios) {
  cat(sprintf("Analyzing: %s\n", scenario_name))
  
  # Load data
  data <- load_scenario(scenario_name)
  p_true <- get_true_p(scenario_name)
  
  # Compute M_p
  M_p <- data$R2_mean / data$p
  
  # Compute derivatives
  derivs <- compute_derivatives(M_p)
  
  # Apply classical method
  classical <- classical_inflection(derivs$delta2)
  
  # Apply argmin delta3 (for comparison)
  argmin_d3 <- argmin_delta3_method(derivs$delta3)
  argmin_d3_plus1 <- argmin_d3$p_star + 1
  
  # Check correctness
  correct_classical <- (classical$p_star == p_true)
  correct_argmin <- (argmin_d3$p_star == p_true)
  correct_argmin_plus1 <- (argmin_d3_plus1 == p_true)
  
  # Print results
  cat(sprintf("  p_true = %d\n", p_true))
  cat(sprintf("  Classical inflection: p* = %d [%s] %s\n", 
              classical$p_star, 
              classical$method,
              ifelse(correct_classical, "✓", "✗")))
  cat(sprintf("    Details: %s\n", classical$details))
  cat(sprintf("  argmin(delta3): p* = %d %s\n", 
              argmin_d3$p_star,
              ifelse(correct_argmin, "✓", "✗")))
  cat(sprintf("  argmin(delta3)+1: p* = %d %s\n", 
              argmin_d3_plus1,
              ifelse(correct_argmin_plus1, "✓", "✗")))
  cat("\n")
  
  # Store results
  results_summary <- rbind(results_summary, data.frame(
    scenario = scenario_name,
    p_true = p_true,
    p_classical = classical$p_star,
    method_classical = classical$method,
    p_argmin_delta3 = argmin_d3$p_star,
    p_argmin_delta3_plus1 = argmin_d3_plus1,
    correct_classical = correct_classical,
    correct_argmin = correct_argmin,
    correct_argmin_plus1 = correct_argmin_plus1,
    stringsAsFactors = FALSE
  ))
}

# Summary statistics
cat("\n")
cat("========================================\n")
cat("SUMMARY\n")
cat("========================================\n\n")

n_scenarios <- nrow(results_summary)
success_classical <- sum(results_summary$correct_classical)
success_argmin <- sum(results_summary$correct_argmin)
success_argmin_plus1 <- sum(results_summary$correct_argmin_plus1)

cat(sprintf("Total scenarios: %d\n\n", n_scenarios))
cat(sprintf("Classical inflection method:  %d/%d (%.0f%%)\n", 
            success_classical, n_scenarios, 100*success_classical/n_scenarios))
cat(sprintf("argmin(delta3):               %d/%d (%.0f%%)\n", 
            success_argmin, n_scenarios, 100*success_argmin/n_scenarios))
cat(sprintf("argmin(delta3)+1:             %d/%d (%.0f%%)\n", 
            success_argmin_plus1, n_scenarios, 100*success_argmin_plus1/n_scenarios))
cat("\n")

# Print detailed table
cat("Detailed Results:\n")
print(results_summary)

# Save results
write.csv(results_summary, "classical_inflection_results.csv", row.names = FALSE)
cat("\nResults saved to: classical_inflection_results.csv\n")

# Create visualization comparing methods
cat("\nCreating comparison plots...\n")

pdf("classical_inflection_comparison.pdf", width = 14, height = 10)

for (scenario_name in scenarios) {
  # Load data
  data <- load_scenario(scenario_name)
  p_true <- get_true_p(scenario_name)
  M_p <- data$R2_mean / data$p
  derivs <- compute_derivatives(M_p)
  classical <- classical_inflection(derivs$delta2)
  argmin_d3 <- argmin_delta3_method(derivs$delta3)
  
  # Create plots
  p_vals <- data$p
  
  # Plot 1: M_p curve
  p1 <- ggplot(data.frame(p = p_vals, M_p = M_p), aes(x = p, y = M_p)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_vline(xintercept = p_true, color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = classical$p_star, color = "blue", linetype = "dotted", linewidth = 1) +
    labs(title = sprintf("%s: M_p = R²/p", scenario_name),
         subtitle = sprintf("p_true=%d (red), p_classical=%d (blue)", p_true, classical$p_star),
         x = "p", y = "M_p") +
    theme_bw()
  
  # Plot 2: delta2 with descent-ascent pattern
  p2 <- ggplot(data.frame(p = p_vals, delta2 = derivs$delta2), aes(x = p, y = delta2)) +
    geom_line(linewidth = 1, color = "darkgreen") +
    geom_point(size = 2, color = "darkgreen") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
    geom_vline(xintercept = p_true, color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = classical$p_star, color = "blue", linetype = "dotted", linewidth = 1) +
    labs(title = "Second Derivative (Curvature)",
         subtitle = classical$details,
         x = "p", y = "Δ₂[M_p]") +
    theme_bw()
  
  # Plot 3: delta3
  p3 <- ggplot(data.frame(p = p_vals, delta3 = derivs$delta3), aes(x = p, y = delta3)) +
    geom_line(linewidth = 1, color = "purple") +
    geom_point(size = 2, color = "purple") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
    geom_vline(xintercept = p_true, color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = argmin_d3$p_star, color = "orange", linetype = "dotted", linewidth = 1) +
    labs(title = "Third Derivative",
         subtitle = sprintf("argmin(Δ₃)=%d", argmin_d3$p_star),
         x = "p", y = "Δ₃[M_p]") +
    theme_bw()
  
  # Combine plots
  grid.arrange(p1, p2, p3, ncol = 3)
}

dev.off()

cat("Plots saved to: classical_inflection_comparison.pdf\n\n")
