#!/usr/bin/env Rscript

# Analyze derivative boundary conditions for M_p
# Focus: Why do derivatives fail at p=1 and p=20?

# Load a scenario
load_scenario <- function(scenario_name) {
  file_path <- sprintf("results/%s/detailed_results.csv", scenario_name)
  data <- read.csv(file_path)
  
  # Average R² across iterations
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  names(avg_data)[2] <- "R2_mean"
  avg_data <- avg_data[order(avg_data$p), ]
  
  return(avg_data)
}

analyze_derivatives_at_boundaries <- function(scenario_name, true_p) {
  cat(sprintf("\n=== %s (p*=%d) ===\n", scenario_name, true_p))
  
  data <- load_scenario(scenario_name)
  p_vals <- data$p
  R2_vals <- data$R2_mean
  n <- length(p_vals)
  
  # Compute M_p
  M_p <- R2_vals / p_vals
  
  cat("\nM_p values:\n")
  print(data.frame(p = p_vals, R2 = round(R2_vals, 4), M_p = round(M_p, 4)))
  
  # Central difference for delta1 (currently used)
  delta1_central <- numeric(n)
  delta1_central[1] <- NA  # No left neighbor
  delta1_central[n] <- NA  # No right neighbor
  for (i in 2:(n-1)) {
    delta1_central[i] <- (M_p[i+1] - M_p[i-1]) / 2
  }
  
  # Forward difference for delta1 (alternative)
  delta1_forward <- numeric(n)
  delta1_forward[n] <- NA  # No right neighbor
  for (i in 1:(n-1)) {
    delta1_forward[i] <- M_p[i+1] - M_p[i]
  }
  
  # Backward difference for delta1 (alternative)
  delta1_backward <- numeric(n)
  delta1_backward[1] <- NA  # No left neighbor
  for (i in 2:n) {
    delta1_backward[i] <- M_p[i] - M_p[i-1]
  }
  
  cat("\nFirst derivatives (Δ₁):\n")
  df_delta1 <- data.frame(
    p = p_vals,
    central = round(delta1_central, 4),
    forward = round(delta1_forward, 4),
    backward = round(delta1_backward, 4)
  )
  print(df_delta1)
  
  # Central difference for delta2
  delta2_central <- numeric(n)
  delta2_central[1] <- NA
  delta2_central[n] <- NA
  for (i in 2:(n-1)) {
    delta2_central[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
  }
  
  cat("\nSecond derivatives (Δ₂):\n")
  df_delta2 <- data.frame(
    p = p_vals,
    central = round(delta2_central, 4)
  )
  print(df_delta2)
  
  # Third derivative from delta2
  delta3 <- numeric(n)
  delta3[1] <- NA
  delta3[n] <- NA
  delta3[n-1] <- NA  # Can't compute delta2[n]
  for (i in 2:(n-2)) {
    delta3[i] <- delta2_central[i+1] - delta2_central[i]
  }
  
  cat("\nThird derivatives (Δ₃):\n")
  df_delta3 <- data.frame(
    p = p_vals,
    delta3 = round(delta3, 4)
  )
  print(df_delta3)
  
  # Current selection method
  valid_idx <- which(!is.na(delta3))
  argmin_delta3 <- valid_idx[which.min(delta3[valid_idx])]
  selected_current <- argmin_delta3
  selected_plus1 <- argmin_delta3 + 1
  
  cat(sprintf("\nCurrent method: argmin(Δ₃) = p=%d (wrong for p*=%d)\n", 
              p_vals[selected_current], true_p))
  cat(sprintf("With +1 fix: argmin(Δ₃)+1 = p=%d\n", 
              p_vals[selected_plus1]))
  
  # Key insight: What if we use different boundary conditions?
  cat("\n=== BOUNDARY CONDITION ANALYSIS ===\n")
  
  # For p=1: Can we compute forward difference?
  if (true_p == 1) {
    cat("\nAt p=1 (left boundary):\n")
    cat(sprintf("  M_p[1] = %.4f\n", M_p[1]))
    cat(sprintf("  M_p[2] = %.4f\n", M_p[2]))
    cat(sprintf("  Forward Δ₁[1] = %.4f (large negative indicates steep drop)\n", 
                delta1_forward[1]))
    cat(sprintf("  Backward Δ₁[2] = %.4f\n", delta1_backward[2]))
    cat("\nPROBLEM: Central difference at p=1 is NA, so Δ₂[1] and Δ₃ cannot detect this!\n")
  }
  
  # For p=20: Can we check if we're at the end?
  if (true_p == 20) {
    cat("\nAt p=20 (right boundary):\n")
    cat(sprintf("  M_p[20] = %.4f\n", M_p[n]))
    cat(sprintf("  R²[20] = %.4f (nearly perfect fit)\n", R2_vals[n]))
    cat(sprintf("  Backward Δ₁[20] = %.4f (small negative, slow decline)\n", 
                delta1_backward[n]))
    cat("\nPROBLEM: Central difference at p=20 is NA, so Δ₂[20] and Δ₃ cannot detect this!\n")
  }
  
  cat("\n=== PROPOSED SOLUTION ===\n")
  cat("Instead of using ONLY central differences, we need:\n")
  cat("1. Forward difference at LEFT boundary (p=1)\n")
  cat("2. Backward difference at RIGHT boundary (p=n)\n")
  cat("3. Central difference in the MIDDLE\n")
  cat("\nThis is the standard approach for numerical derivatives with boundary conditions!\n")
  
  invisible(list(
    p = p_vals,
    M_p = M_p,
    delta1_central = delta1_central,
    delta1_forward = delta1_forward,
    delta1_backward = delta1_backward,
    delta2 = delta2_central,
    delta3 = delta3
  ))
}

# Test on all three critical scenarios
cat(paste(rep("=", 80), collapse = ""))
cat("\nDERIVATIVE BOUNDARY CONDITION ANALYSIS\n")
cat(paste(rep("=", 80), collapse = ""))
cat("\n")

analyze_derivatives_at_boundaries("A1_Baseline_Uncorrelated", 3)
analyze_derivatives_at_boundaries("A2_Single_Predictor", 1)
analyze_derivatives_at_boundaries("A3_Full_Support", 20)
