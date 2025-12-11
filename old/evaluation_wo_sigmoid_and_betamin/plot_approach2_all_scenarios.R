#!/usr/bin/env Rscript

# Generate plots for all scenarios using Approach 2
# (standard forward/backward second differences)

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

scenario_labels <- list(
  "A1_Baseline_Uncorrelated" = "A1: Baseline (Uncorrelated)",
  "A2_Single_Predictor" = "A2: Single Predictor",
  "A3_Full_Support" = "A3: Full Support",
  "B1_AR1_Weak" = "B1-Weak: AR(1) rho=0.5",
  "B1_AR1_Strong" = "B1-Strong: AR(1) rho=0.8",
  "B2_Compound_Symmetry" = "B2: Compound Symmetry",
  "B3_Block_Structure" = "B3: Block Structure",
  "C1_Weak_Signals" = "C1: Weak Signals",
  "C2_Many_Weak_Signals" = "C2: Many Weak Signals",
  "C3_Mixed_Signals" = "C3: Mixed Signals"
)

# APPROACH 2: Standard forward/backward second differences
compute_derivatives_approach2 <- function(M_p) {
  n <- length(M_p)
  
  # Second derivative with standard one-sided formulas
  delta2 <- numeric(n)
  delta2[1] <- M_p[1] - 2*M_p[2] + M_p[3]  # Forward
  delta2[n] <- M_p[n-2] - 2*M_p[n-1] + M_p[n]  # Backward
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]  # Central
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
      return(p + 1)
    }
  }
  
  # No inflection found
  if (all(delta2 > 0)) {
    return(1)  # Convex, fallback to p=1
  } else {
    return(1)
  }
}

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

# Generate plots for each scenario
all_plots <- list()

for (scenario in scenarios) {
  cat(sprintf("Processing %s...\n", scenario))
  
  data <- load_scenario(scenario)
  M_p <- data$R2_mean / data$p
  p_true <- get_true_p(scenario)
  label <- scenario_labels[[scenario]]
  
  deriv <- compute_derivatives_approach2(M_p)
  p_classical <- classical_inflection(deriv$delta2)
  p_argmin <- which.min(deriv$delta3)
  
  # Plot 1: M_p
  p1 <- ggplot(data.frame(p = data$p, M_p = M_p), aes(x = p, y = M_p)) +
    geom_line(size = 1, color = "blue") +
    geom_point(size = 2, color = "blue") +
    geom_vline(xintercept = p_true, linetype = "dashed", color = "red", size = 1) +
    geom_vline(xintercept = p_classical, linetype = "dotted", color = "green", size = 0.8, alpha = 0.7) +
    geom_vline(xintercept = p_argmin, linetype = "dotted", color = "purple", size = 0.8, alpha = 0.7) +
    labs(
      title = sprintf("%s (p* = %d)", label, p_true),
      subtitle = sprintf("Approach 2: Classical=%d, argmin(Delta3)=%d", p_classical, p_argmin),
      x = "p",
      y = "M_p = R²/p"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  # Plot 2: Delta2
  has_inflection <- p_classical > 1 && p_classical < 20
  
  p2 <- ggplot(data.frame(p = data$p, delta2 = deriv$delta2), aes(x = p, y = delta2)) +
    geom_line(size = 1, color = "darkgreen") +
    geom_point(size = 2, color = "darkgreen") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = p_true, linetype = "dashed", color = "red", size = 1) +
    labs(
      title = ifelse(has_inflection, 
                    sprintf("Inflection at p = %d", p_classical),
                    "No inflection (all Delta2 > 0)"),
      x = "p",
      y = "Delta2[M_p]"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 9))
  
  # Highlight inflection point if exists
  if (has_inflection && p_classical <= length(data$p)) {
    p2 <- p2 + geom_vline(xintercept = p_classical, linetype = "dotted", 
                          color = "green", size = 0.8, alpha = 0.7)
  }
  
  # Plot 3: Delta3
  p3 <- ggplot(data.frame(p = data$p, delta3 = deriv$delta3), aes(x = p, y = delta3)) +
    geom_line(size = 1, color = "purple") +
    geom_point(size = 2, color = "purple") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = p_true, linetype = "dashed", color = "red", size = 1) +
    geom_vline(xintercept = p_argmin, linetype = "dotted", color = "purple", size = 0.8, alpha = 0.7) +
    labs(
      title = sprintf("argmin(Delta3) = %d", p_argmin),
      x = "p",
      y = "Delta3[M_p]"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 9))
  
  # Combine 3 plots
  combined <- arrangeGrob(p1, p2, p3, ncol = 1)
  all_plots[[scenario]] <- combined
}

# Save all plots to one PDF
cat("\nSaving all plots to approach2_all_scenarios.pdf...\n")
pdf("approach2_all_scenarios.pdf", width = 10, height = 12)
for (scenario in scenarios) {
  grid.arrange(all_plots[[scenario]])
}
dev.off()

cat("Done! Plots saved to: approach2_all_scenarios.pdf\n")
cat("\nSummary:\n")
cat("- Red dashed line: True p*\n")
cat("- Green dotted line: Classical inflection detection\n")
cat("- Purple dotted line: argmin(Delta3)\n")
