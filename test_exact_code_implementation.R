#!/usr/bin/env Rscript

# Test with EXACT implementation from 02_metrics.R

load_scenario <- function(scenario_name) {
  file_path <- sprintf("results/%s/detailed_results.csv", scenario_name)
  data <- read.csv(file_path)
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  names(avg_data)[2] <- "R2_mean"
  avg_data <- avg_data[order(avg_data$p), ]
  return(avg_data)
}

compute_derivatives_exact_as_code <- function(M_p) {
  n <- length(M_p)
  
  # Second derivative - EXACT implementation from 02_metrics.R
  delta2 <- numeric(n)
  
  # Interior points: central difference
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
  }
  
  # Boundary points from code:
  # First point: assuming M_p[0] = 0
  delta2[1] <- -2 * M_p[1] + M_p[2]
  
  # Last point: assuming M_p[n+1] = M_p[n]
  delta2[n] <- M_p[n-1] - 2 * M_p[n]
  
  # Third derivative - EXACT implementation from 02_metrics.R
  delta3 <- numeric(n-1)
  for (i in 1:(n-1)) {
    delta3[i] <- delta2[i+1] - delta2[i]
  }
  
  return(list(delta2 = delta2, delta3 = delta3))
}

scenarios <- list(
  "A1_Baseline_Uncorrelated" = 3,
  "A2_Single_Predictor" = 1,
  "A3_Full_Support" = 20
)

cat("================================================================================\n")
cat("EXACT IMPLEMENTATION FROM 02_metrics.R\n")
cat("================================================================================\n")

for (scenario in names(scenarios)) {
  true_p <- scenarios[[scenario]]
  cat(sprintf("\n=== %s (p*=%d) ===\n", scenario, true_p))
  
  data <- load_scenario(scenario)
  p_vals <- data$p
  M_p <- data$R2_mean / data$p
  n <- length(p_vals)
  
  derivs <- compute_derivatives_exact_as_code(M_p)
  
  cat("\nM_p and derivatives:\n")
  df <- data.frame(
    p = p_vals,
    M_p = round(M_p, 4),
    delta2 = round(derivs$delta2, 4),
    delta3 = c(round(derivs$delta3, 4), NA)
  )
  print(df)
  
  # Current method: argmin(delta3)
  argmin_delta3 <- which.min(derivs$delta3)
  selected_current <- p_vals[argmin_delta3]
  
  cat(sprintf("\nCurrent method: argmin(Δ₃) = p=%d %s\n", 
              selected_current, 
              ifelse(selected_current == true_p, "✓", "✗")))
  
  # Check delta2 properties with this formulation
  cat(sprintf("\nΔ₂[1] = %.4f (with M_p[0]=0 assumption)\n", derivs$delta2[1]))
  cat(sprintf("argmax(Δ₂) = p=%d\n", p_vals[which.max(derivs$delta2)]))
  
  # Check delta3[1]
  cat(sprintf("\nΔ₃[1] = %.4f\n", derivs$delta3[1]))
}

cat("\n\n================================================================================\n")
cat("OBSERVATION\n")
cat("================================================================================\n\n")

cat("With the M_p[0]=0 boundary condition:\n")
cat("- Δ₂[1] = -2*M_p[1] + M_p[2]\n")
cat("- This is NEGATIVE when M_p[1] > M_p[2]/2\n")
cat("- Which is almost always true since M_p decreases!\n\n")

cat("This boundary condition might not be appropriate for detecting p*=1.\n")
cat("The assumption M_p[0]=0 makes sense mathematically (can't have 0 predictors),\n")
cat("but it creates artifacts that prevent detection of left boundary optimum.\n")
