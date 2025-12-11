#!/usr/bin/env Rscript

# Test M_p selection with proper boundary conditions for derivatives

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

# Compute derivatives with proper boundary conditions
compute_derivatives_proper <- function(M_p) {
  n <- length(M_p)
  
  # First derivative with boundary conditions
  delta1 <- numeric(n)
  delta1[1] <- M_p[2] - M_p[1]  # Forward difference at left boundary
  delta1[n] <- M_p[n] - M_p[n-1]  # Backward difference at right boundary
  for (i in 2:(n-1)) {
    delta1[i] <- (M_p[i+1] - M_p[i-1]) / 2  # Central difference in middle
  }
  
  # Second derivative with boundary conditions
  delta2 <- numeric(n)
  # At left boundary (p=1): use forward difference of delta1
  delta2[1] <- delta1[2] - delta1[1]
  # At right boundary (p=n): use backward difference of delta1
  delta2[n] <- delta1[n] - delta1[n-1]
  # In the middle: central difference
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
  }
  
  # Third derivative with boundary conditions
  delta3 <- numeric(n)
  # At left boundary: forward difference of delta2
  delta3[1] <- delta2[2] - delta2[1]
  # At right boundary: backward difference of delta2
  delta3[n] <- delta2[n] - delta2[n-1]
  # In the middle: central difference of delta2
  for (i in 2:(n-1)) {
    delta3[i] <- delta2[i+1] - delta2[i]
  }
  
  return(list(delta1 = delta1, delta2 = delta2, delta3 = delta3))
}

# Test on a scenario
test_scenario <- function(scenario_name, true_p) {
  cat(sprintf("\n=== %s (p*=%d) ===\n", scenario_name, true_p))
  
  data <- load_scenario(scenario_name)
  p_vals <- data$p
  R2_vals <- data$R2_mean
  n <- length(p_vals)
  
  # Compute M_p
  M_p <- R2_vals / p_vals
  
  # Compute derivatives with proper boundary conditions
  derivs <- compute_derivatives_proper(M_p)
  
  cat("\nM_p and derivatives (first 5 and last 5 values):\n")
  df <- data.frame(
    p = p_vals,
    M_p = round(M_p, 4),
    delta1 = round(derivs$delta1, 4),
    delta2 = round(derivs$delta2, 4),
    delta3 = round(derivs$delta3, 4)
  )
  print(rbind(head(df, 5), tail(df, 5)))
  
  # Now test selection methods
  
  # Method 1: argmin(delta3) (current - but now all values are defined!)
  selected_min_delta3 <- p_vals[which.min(derivs$delta3)]
  
  # Method 2: argmin(delta3) + 1
  idx_min_delta3 <- which.min(derivs$delta3)
  if (idx_min_delta3 < n) {
    selected_min_delta3_plus1 <- p_vals[idx_min_delta3 + 1]
  } else {
    selected_min_delta3_plus1 <- p_vals[n]
  }
  
  # Method 3: Where delta3 changes from negative to positive (inflection point)
  # This is where the curvature changes direction
  zero_crossings <- which(derivs$delta3[-1] * derivs$delta3[-n] < 0)
  if (length(zero_crossings) > 0) {
    # Take the first zero crossing where delta3 goes from negative to positive
    for (zc in zero_crossings) {
      if (derivs$delta3[zc] < 0 && derivs$delta3[zc+1] > 0) {
        selected_zero_cross <- p_vals[zc + 1]  # +1 because we want the point after crossing
        break
      }
    }
    if (!exists("selected_zero_cross")) selected_zero_cross <- NA
  } else {
    selected_zero_cross <- NA
  }
  
  cat(sprintf("\nSelection methods:\n"))
  cat(sprintf("  argmin(Δ₃)     = p=%d %s\n", 
              selected_min_delta3, 
              ifelse(selected_min_delta3 == true_p, "✓", "✗")))
  cat(sprintf("  argmin(Δ₃)+1   = p=%d %s\n", 
              selected_min_delta3_plus1, 
              ifelse(selected_min_delta3_plus1 == true_p, "✓", "✗")))
  if (!is.na(selected_zero_cross)) {
    cat(sprintf("  Δ₃ zero cross  = p=%d %s\n", 
                selected_zero_cross, 
                ifelse(selected_zero_cross == true_p, "✓", "✗")))
  } else {
    cat("  Δ₃ zero cross  = NA (no crossing found)\n")
  }
  
  return(list(
    min_delta3 = selected_min_delta3,
    min_delta3_plus1 = selected_min_delta3_plus1,
    zero_cross = selected_zero_cross,
    correct = true_p
  ))
}

# Define true p* for all scenarios
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

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("TESTING M_p SELECTION WITH PROPER DERIVATIVE BOUNDARY CONDITIONS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Test all scenarios
results <- list()
for (scenario in names(true_p_star)) {
  results[[scenario]] <- test_scenario(scenario, true_p_star[[scenario]])
}

# Summary
cat("\n\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("SUMMARY OF RESULTS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

success_min_delta3 <- sum(sapply(names(results), function(s) {
  results[[s]]$min_delta3 == results[[s]]$correct
}))

success_min_delta3_plus1 <- sum(sapply(names(results), function(s) {
  results[[s]]$min_delta3_plus1 == results[[s]]$correct
}))

success_zero_cross <- sum(sapply(names(results), function(s) {
  !is.na(results[[s]]$zero_cross) && results[[s]]$zero_cross == results[[s]]$correct
}))

total <- length(results)

cat(sprintf("argmin(Δ₃):       %d/%d (%.1f%%) correct\n", 
            success_min_delta3, total, 100*success_min_delta3/total))
cat(sprintf("argmin(Δ₃)+1:     %d/%d (%.1f%%) correct\n", 
            success_min_delta3_plus1, total, 100*success_min_delta3_plus1/total))
cat(sprintf("Δ₃ zero crossing: %d/%d (%.1f%%) correct\n", 
            success_zero_cross, total, 100*success_zero_cross/total))
