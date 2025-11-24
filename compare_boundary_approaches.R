#!/usr/bin/env Rscript

# Comparison of Two Boundary Approaches
# ======================================
# Approach 1: Via first derivative (Eqs 5-7 in paper)
# Approach 2: Standard forward/backward second differences (Eqs 8-9 in paper)

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

# APPROACH 1: Via first derivative (current implementation)
compute_derivatives_approach1 <- function(M_p) {
  n <- length(M_p)
  
  # First derivative
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

# APPROACH 2: Standard forward/backward second differences
compute_derivatives_approach2 <- function(M_p) {
  n <- length(M_p)
  
  # Second derivative with standard one-sided formulas
  delta2 <- numeric(n)
  delta2[1] <- M_p[1] - 2*M_p[2] + M_p[3]  # Forward second difference
  delta2[n] <- M_p[n-2] - 2*M_p[n-1] + M_p[n]  # Backward second difference
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
  
  return(list(delta2 = delta2, delta3 = delta3))
}

# Classical inflection detection
classical_inflection <- function(delta2) {
  n <- length(delta2)
  
  for (p in 1:(n-1)) {
    if (sign(delta2[p]) * sign(delta2[p+1]) < 0) {
      return(list(
        p_star = p + 1,
        method = "inflection_detected"
      ))
    }
  }
  
  if (all(delta2 > 0)) {
    return(list(p_star = 1, method = "no_inflection_convex"))
  } else if (all(delta2 < 0)) {
    return(list(p_star = 20, method = "no_inflection_concave"))
  } else {
    return(list(p_star = 1, method = "mixed_no_clear_pattern"))
  }
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

results_list <- list()

cat("\n=== COMPARISON: Approach 1 vs Approach 2 ===\n\n")

for (scenario in scenarios) {
  data <- load_scenario(scenario)
  M_p <- data$R2_mean / data$p
  p_true <- get_true_p(scenario)
  
  # Approach 1
  deriv1 <- compute_derivatives_approach1(M_p)
  class1 <- classical_inflection(deriv1$delta2)
  argmin1 <- which.min(deriv1$delta3)
  
  # Approach 2
  deriv2 <- compute_derivatives_approach2(M_p)
  class2 <- classical_inflection(deriv2$delta2)
  argmin2 <- which.min(deriv2$delta3)
  
  # Compare delta2 at boundaries
  delta2_1_boundary <- deriv1$delta2[1]
  delta2_1_boundary_app2 <- deriv2$delta2[1]
  
  cat(sprintf("%-25s (p*=%2d)\n", scenario, p_true))
  cat(sprintf("  Approach 1: Classical=%2d (%s), argmin(Δ₃)=%2d\n", 
              class1$p_star, class1$method, argmin1))
  cat(sprintf("  Approach 2: Classical=%2d (%s), argmin(Δ₃)=%2d\n", 
              class2$p_star, class2$method, argmin2))
  cat(sprintf("  Δ₂(1): Approach1=%.6f, Approach2=%.6f (diff=%.6f)\n",
              delta2_1_boundary, delta2_1_boundary_app2, 
              delta2_1_boundary_app2 - delta2_1_boundary))
  
  # Check if results differ
  if (class1$p_star != class2$p_star || argmin1 != argmin2) {
    cat("  *** DIFFERENT RESULTS! ***\n")
  }
  cat("\n")
  
  results_list[[scenario]] <- data.frame(
    scenario = scenario,
    p_true = p_true,
    p_classical_app1 = class1$p_star,
    p_classical_app2 = class2$p_star,
    p_argmin_app1 = argmin1,
    p_argmin_app2 = argmin2,
    correct_classical_app1 = (class1$p_star == p_true),
    correct_classical_app2 = (class2$p_star == p_true),
    correct_argmin_app1 = (argmin1 == p_true),
    correct_argmin_app2 = (argmin2 == p_true),
    delta2_1_app1 = delta2_1_boundary,
    delta2_1_app2 = delta2_1_boundary_app2
  )
}

# Combine results
results_df <- do.call(rbind, results_list)

# Summary statistics
cat("\n=== SUMMARY ===\n\n")
cat("Approach 1 (via Δ₁):\n")
cat(sprintf("  Classical method: %d/10 correct\n", 
            sum(results_df$correct_classical_app1)))
cat(sprintf("  argmin(Δ₃):       %d/10 correct\n", 
            sum(results_df$correct_argmin_app1)))

cat("\nApproach 2 (standard forward/backward):\n")
cat(sprintf("  Classical method: %d/10 correct\n", 
            sum(results_df$correct_classical_app2)))
cat(sprintf("  argmin(Δ₃):       %d/10 correct\n", 
            sum(results_df$correct_argmin_app2)))

# Save results
write.csv(results_df, "boundary_approach_comparison.csv", row.names = FALSE)
cat("\nResults saved to: boundary_approach_comparison.csv\n")

# Create comparison plot for A2 (most interesting case)
cat("\nGenerating comparison plot for A2 (Single Predictor)...\n")

data_a2 <- load_scenario("A2_Single_Predictor")
M_p_a2 <- data_a2$R2_mean / data_a2$p

deriv1_a2 <- compute_derivatives_approach1(M_p_a2)
deriv2_a2 <- compute_derivatives_approach2(M_p_a2)

plot_df <- data.frame(
  p = rep(data_a2$p, 2),
  delta2 = c(deriv1_a2$delta2, deriv2_a2$delta2),
  approach = rep(c("Approach 1 (via Δ₁)", "Approach 2 (forward/backward)"), 
                 each = length(data_a2$p))
)

p_plot <- ggplot(plot_df, aes(x = p, y = delta2, color = approach, linetype = approach)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 1, linetype = "dotted", color = "red", size = 0.8) +
  labs(
    title = "A2: Δ₂[M_p] Comparison - Boundary Treatment Impact",
    subtitle = sprintf("p* = 1, Δ₂(1) values: App1=%.4f, App2=%.4f", 
                      deriv1_a2$delta2[1], deriv2_a2$delta2[1]),
    x = "Number of Components (p)",
    y = "Second Derivative Δ₂[M_p]",
    color = "Boundary Approach",
    linetype = "Boundary Approach"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("boundary_approach_comparison_A2.pdf", p_plot, width = 10, height = 6)
cat("Plot saved to: boundary_approach_comparison_A2.pdf\n")
