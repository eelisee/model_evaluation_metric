#!/usr/bin/env Rscript

# Analyze derivative behavior to detect boundary optima
# WITHOUT using R² as a criterion

load_scenario <- function(scenario_name) {
  file_path <- sprintf("results/%s/detailed_results.csv", scenario_name)
  data <- read.csv(file_path)
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  names(avg_data)[2] <- "R2_mean"
  avg_data <- avg_data[order(avg_data$p), ]
  return(avg_data)
}

compute_derivatives_proper <- function(M_p) {
  n <- length(M_p)
  
  delta1 <- numeric(n)
  delta1[1] <- M_p[2] - M_p[1]
  delta1[n] <- M_p[n] - M_p[n-1]
  for (i in 2:(n-1)) {
    delta1[i] <- (M_p[i+1] - M_p[i-1]) / 2
  }
  
  delta2 <- numeric(n)
  delta2[1] <- delta1[2] - delta1[1]
  delta2[n] <- delta1[n] - delta1[n-1]
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
  }
  
  delta3 <- numeric(n)
  delta3[1] <- delta2[2] - delta2[1]
  delta3[n] <- delta2[n] - delta2[n-1]
  for (i in 2:(n-1)) {
    delta3[i] <- delta2[i+1] - delta2[i]
  }
  
  return(list(delta1 = delta1, delta2 = delta2, delta3 = delta3))
}

# Analyze the three critical scenarios
scenarios <- list(
  "A1_Baseline_Uncorrelated" = 3,
  "A2_Single_Predictor" = 1,
  "A3_Full_Support" = 20
)

cat("================================================================================\n")
cat("PURE DERIVATIVE ANALYSIS - NO EXTERNAL CRITERIA\n")
cat("================================================================================\n")

for (scenario in names(scenarios)) {
  true_p <- scenarios[[scenario]]
  cat(sprintf("\n=== %s (p*=%d) ===\n", scenario, true_p))
  
  data <- load_scenario(scenario)
  p_vals <- data$p
  M_p <- data$R2_mean / data$p
  n <- length(p_vals)
  
  derivs <- compute_derivatives_proper(M_p)
  
  cat("\nAll derivative values:\n")
  df <- data.frame(
    p = p_vals,
    M_p = round(M_p, 4),
    delta1 = round(derivs$delta1, 4),
    delta2 = round(derivs$delta2, 4),
    delta3 = round(derivs$delta3, 4)
  )
  print(df)
  
  # Analyze derivative properties
  cat("\n=== DERIVATIVE PROPERTIES ===\n")
  
  # First derivative: always negative for M_p = R²/p
  cat(sprintf("Δ₁: all negative? %s\n", all(derivs$delta1 < 0)))
  cat(sprintf("Δ₁[1] = %.4f (at left boundary)\n", derivs$delta1[1]))
  cat(sprintf("Δ₁[n] = %.4f (at right boundary)\n", derivs$delta1[n]))
  
  # Second derivative: curvature
  cat(sprintf("\nΔ₂[1] = %.4f\n", derivs$delta2[1]))
  cat(sprintf("argmax(Δ₂) = p=%d\n", p_vals[which.max(derivs$delta2)]))
  cat(sprintf("Δ₂[n] = %.4f\n", derivs$delta2[n]))
  
  # Third derivative: rate of change of curvature
  cat(sprintf("\nΔ₃[1] = %.4f\n", derivs$delta3[1]))
  cat(sprintf("argmin(Δ₃) = p=%d\n", p_vals[which.min(derivs$delta3)]))
  cat(sprintf("Δ₃[n] = %.4f\n", derivs$delta3[n]))
  
  # Key observation: Check if Δ₂ is monotonic
  is_delta2_monotonic_decreasing <- all(diff(derivs$delta2) <= 0)
  is_delta2_monotonic_increasing <- all(diff(derivs$delta2) >= 0)
  
  cat(sprintf("\nΔ₂ monotonic decreasing? %s\n", is_delta2_monotonic_decreasing))
  cat(sprintf("Δ₂ monotonic increasing? %s\n", is_delta2_monotonic_increasing))
  
  if (is_delta2_monotonic_decreasing) {
    cat("  → Curvature is strongest at p=1, weakens afterward\n")
    cat("  → This suggests p*=1 (left boundary optimum)\n")
  } else if (is_delta2_monotonic_increasing) {
    cat("  → Curvature is strongest at p=n, strengthens throughout\n")
    cat("  → This suggests p*=n (right boundary optimum)\n")
  } else {
    cat("  → Curvature has an interior maximum\n")
    cat("  → This suggests an interior optimum\n")
  }
  
  # Check zero crossings
  delta3_crossings <- which(derivs$delta3[-1] * derivs$delta3[-n] < 0)
  if (length(delta3_crossings) > 0) {
    cat(sprintf("\nΔ₃ zero crossings at: p=%s\n", 
                paste(p_vals[delta3_crossings], collapse=", ")))
  }
}

cat("\n\n")
cat("================================================================================\n")
cat("KEY INSIGHT\n")
cat("================================================================================\n\n")

cat("For constrained optimization on [1, n]:\n\n")

cat("1. If Δ₂ is MONOTONICALLY DECREASING:\n")
cat("   → Maximum curvature at p=1 (left boundary)\n")
cat("   → Optimum is at p*=1\n\n")

cat("2. If Δ₂ is MONOTONICALLY INCREASING:\n")
cat("   → Maximum curvature at p=n (right boundary)\n")
cat("   → Optimum is at p*=n\n\n")

cat("3. If Δ₂ has an INTERIOR MAXIMUM:\n")
cat("   → Use Δ₃ zero crossing or argmin(Δ₃)+1\n")
cat("   → Optimum is in the interior\n\n")

cat("This is purely based on derivatives, no external criteria!\n")
