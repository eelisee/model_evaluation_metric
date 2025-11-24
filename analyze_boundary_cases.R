# Boundary Case Analysis: A2 and A3
# ===================================
# A2: p*=1 (single predictor)
# A3: p*=20 (full support)

analyze_boundary_scenario <- function(scenario_name, true_p) {
  
  csv_path <- file.path("results", scenario_name, "detailed_results.csv")
  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  
  R2_vals <- avg_data$R2
  p_vals <- avg_data$p
  n <- length(p_vals)
  
  cat("\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat(sprintf("SCENARIO: %s (true p* = %d)\n", scenario_name, true_p))
  cat(paste(rep("=", 80), collapse = ""), "\n\n")
  
  # Compute M_p
  M_p <- R2_vals / p_vals
  
  # Show M_p values
  cat("M_p values:\n")
  for (i in 1:min(10, n)) {
    marker <- if (i == true_p) " <<< TRUE p*" else ""
    cat(sprintf("  p=%2d: M_p = %.8f%s\n", p_vals[i], M_p[i], marker))
  }
  
  # First derivative
  delta1 <- diff(M_p)
  
  cat("\nFirst derivative (Δ₁):\n")
  for (i in 1:min(9, length(delta1))) {
    marker <- if (i == true_p || i == true_p - 1) " <<<" else ""
    cat(sprintf("  p=%2d→%2d: Δ₁ = %+.8f%s\n", p_vals[i], p_vals[i+1], delta1[i], marker))
  }
  
  # Second derivative
  delta2 <- numeric(n)
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
  }
  delta2[1] <- NA
  delta2[n] <- NA
  
  cat("\nSecond derivative (Δ₂):\n")
  for (i in 1:min(10, n)) {
    if (!is.na(delta2[i])) {
      sign_str <- if (delta2[i] < 0) "NEGATIVE" else "POSITIVE"
      marker <- if (i == true_p || i == true_p - 1 || i == true_p + 1) " <<<" else ""
      cat(sprintf("  p=%2d: Δ₂ = %+.8f [%s]%s\n", p_vals[i], delta2[i], sign_str, marker))
    }
  }
  
  # Third derivative
  delta3 <- diff(delta2)
  
  cat("\nThird derivative (Δ₃):\n")
  for (i in 1:min(9, length(delta3))) {
    if (!is.na(delta3[i])) {
      sign_str <- if (delta3[i] < 0) "DECREASING" else "INCREASING"
      marker <- if (i == true_p || i == true_p - 1) " <<<" else ""
      cat(sprintf("  p=%2d→%2d: Δ₃ = %+.8f [%s]%s\n", p_vals[i], p_vals[i+1], delta3[i], sign_str, marker))
    }
  }
  
  # Key observations
  cat("\n")
  cat(paste(rep("-", 80), collapse = ""), "\n")
  cat("KEY OBSERVATIONS:\n")
  cat(paste(rep("-", 80), collapse = ""), "\n\n")
  
  # Where is M_p maximum?
  max_Mp_idx <- which.max(M_p)
  cat(sprintf("1. Maximum M_p at p=%d (value: %.6f)\n", p_vals[max_Mp_idx], M_p[max_Mp_idx]))
  
  # Where does M_p drop most?
  max_drop_idx <- which.min(delta1)
  cat(sprintf("2. Largest drop in M_p at p=%d→%d (Δ₁=%.6f)\n", 
              p_vals[max_drop_idx], p_vals[max_drop_idx+1], delta1[max_drop_idx]))
  
  # Where does drop rate change most?
  valid_idx <- which(!is.na(delta2))
  if (length(valid_idx) > 0) {
    max_delta2_idx <- valid_idx[which.max(abs(delta2[valid_idx]))]
    cat(sprintf("3. Largest curvature change at p=%d (Δ₂=%.6f)\n", 
                p_vals[max_delta2_idx], delta2[max_delta2_idx]))
  }
  
  # Elbow detection: max distance from line p=1 to p=20
  # Line from (p[1], M_p[1]) to (p[n], M_p[n])
  x1 <- p_vals[1]
  y1 <- M_p[1]
  x2 <- p_vals[n]
  y2 <- M_p[n]
  
  distances <- numeric(n)
  for (i in 1:n) {
    a <- y2 - y1
    b <- -(x2 - x1)
    c <- (x2 - x1) * y1 - (y2 - y1) * x1
    distances[i] <- abs(a * p_vals[i] + b * M_p[i] + c) / sqrt(a^2 + b^2)
  }
  
  max_dist_idx <- which.max(distances)
  cat(sprintf("4. Maximum distance from line (elbow): p=%d (distance: %.6f)\n", 
              p_vals[max_dist_idx], distances[max_dist_idx]))
  
  # Ratio test: where does M_p / M_p[1] drop below threshold?
  ratio_to_max <- M_p / M_p[1]
  threshold_80pct <- which(ratio_to_max < 0.8)[1]
  threshold_50pct <- which(ratio_to_max < 0.5)[1]
  
  cat(sprintf("5. M_p drops below 80%% of max at p=%d\n", 
              ifelse(is.na(threshold_80pct), NA, p_vals[threshold_80pct])))
  cat(sprintf("6. M_p drops below 50%% of max at p=%d\n", 
              ifelse(is.na(threshold_50pct), NA, p_vals[threshold_50pct])))
  
  # Test if it's a boundary case
  cat("\n")
  if (true_p == 1) {
    cat("This is a MINIMAL SUPPORT case (p*=1)\n")
    cat("Expected behavior: Should select smallest p with high M_p\n")
    cat("Challenge: No inflection point before p*\n")
  } else if (true_p == n) {
    cat("This is a FULL SUPPORT case (p*=%d)\n", n)
    cat("Expected behavior: Should select all variables\n")
    cat("Challenge: M_p keeps decreasing, no clear inflection\n")
  }
  
  return(list(
    M_p = M_p,
    delta1 = delta1,
    delta2 = delta2,
    delta3 = delta3,
    max_Mp_idx = max_Mp_idx,
    max_dist_idx = max_dist_idx
  ))
}

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("BOUNDARY CASE ANALYSIS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Analyze A2
a2_results <- analyze_boundary_scenario("A2_Single_Predictor", 1)

# Analyze A3
a3_results <- analyze_boundary_scenario("A3_Full_Support", 20)

# Compare with a normal case (A1)
cat("\n\nFor comparison:\n")
a1_results <- analyze_boundary_scenario("A1_Baseline_Uncorrelated", 3)

# Now test alternative formulations specifically for boundary cases
cat("\n\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("TESTING BOUNDARY-AWARE FORMULATIONS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

test_boundary_formulations <- function(scenario_name, true_p) {
  
  csv_path <- file.path("results", scenario_name, "detailed_results.csv")
  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  
  R2_vals <- avg_data$R2
  p_vals <- avg_data$p
  n <- length(p_vals)
  
  M_p <- R2_vals / p_vals
  delta1 <- diff(M_p)
  
  delta2 <- numeric(n)
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
  }
  delta2[1] <- NA
  delta2[n] <- NA
  
  delta3 <- diff(delta2)
  
  results <- list()
  
  # 1. Maximum M_p (simplest)
  results[["max(M_p)"]] <- which.max(M_p)
  
  # 2. Elbow method (L-method)
  x1 <- p_vals[1]
  y1 <- M_p[1]
  x2 <- p_vals[n]
  y2 <- M_p[n]
  
  distances <- numeric(n)
  for (i in 1:n) {
    a <- y2 - y1
    b <- -(x2 - x1)
    c <- (x2 - x1) * y1 - (y2 - y1) * x1
    distances[i] <- abs(a * p_vals[i] + b * M_p[i] + c) / sqrt(a^2 + b^2)
  }
  results[["elbow (max dist)"]] <- which.max(distances)
  
  # 3. Largest absolute first derivative
  results[["max(|Δ₁|)"]] <- which.max(abs(delta1))
  
  # 4. min(Δ₃) + 1 (our current best)
  valid_idx <- which(!is.na(delta3))
  if (length(valid_idx) > 0) {
    idx <- valid_idx[which.min(delta3[valid_idx])]
    if (idx + 1 <= n) {
      results[["min(Δ₃) + 1"]] <- idx + 1
    } else {
      results[["min(Δ₃) + 1"]] <- n
    }
  }
  
  # 5. Hybrid: max(M_p) if at boundary, else min(Δ₃)+1
  max_Mp_idx <- which.max(M_p)
  if (max_Mp_idx == 1) {
    # Boundary case: maximum at p=1
    results[["hybrid 1"]] <- 1
  } else if (length(valid_idx) > 0) {
    idx <- valid_idx[which.min(delta3[valid_idx])]
    if (idx + 1 <= n) {
      results[["hybrid 1"]] <- idx + 1
    } else {
      results[["hybrid 1"]] <- n
    }
  }
  
  # 6. Hybrid: Check if M_p is still very high at p=20
  ratio_last_to_first <- M_p[n] / M_p[1]
  if (ratio_last_to_first > 0.8) {
    # Full support case
    results[["hybrid 2"]] <- n
  } else if (length(valid_idx) > 0) {
    idx <- valid_idx[which.min(delta3[valid_idx])]
    if (idx + 1 <= n) {
      results[["hybrid 2"]] <- idx + 1
    } else {
      results[["hybrid 2"]] <- n
    }
  }
  
  # 7. Diminishing returns threshold: where M_p drops below X% of max
  threshold <- 0.7  # 70% of maximum
  below_threshold <- which(M_p < threshold * max(M_p))
  if (length(below_threshold) > 0) {
    results[["70% threshold"]] <- below_threshold[1] - 1
    if (results[["70% threshold"]] < 1) results[["70% threshold"]] <- 1
  } else {
    results[["70% threshold"]] <- n
  }
  
  return(results)
}

scenarios <- list(
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

# Test all scenarios
all_tests <- list()
for (scenario_name in names(scenarios)) {
  all_tests[[scenario_name]] <- test_boundary_formulations(scenario_name, scenarios[[scenario_name]])
}

# Print results table
cat(sprintf("%-25s | True p* |", "Scenario"))
formulation_names <- names(all_tests[[1]])
for (fname in formulation_names) {
  cat(sprintf(" %-15s |", substr(fname, 1, 15)))
}
cat("\n")
cat(paste(rep("-", 80), collapse = ""), "\n")

for (scenario_name in names(scenarios)) {
  true_p <- scenarios[[scenario_name]]
  results <- all_tests[[scenario_name]]
  
  cat(sprintf("%-25s | %7d |", substr(scenario_name, 1, 25), true_p))
  
  for (fname in formulation_names) {
    p_sel <- results[[fname]]
    if (is.null(p_sel) || is.na(p_sel)) {
      cat(sprintf(" %15s |", "NA"))
    } else if (p_sel == true_p) {
      cat(sprintf(" %15s |", paste0("✓ ", p_sel)))
    } else {
      cat(sprintf(" %15s |", paste0("✗ ", p_sel)))
    }
  }
  cat("\n")
}

# Success rates
cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("SUCCESS RATES:\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

for (fname in formulation_names) {
  correct <- 0
  total <- 0
  
  for (scenario_name in names(scenarios)) {
    true_p <- scenarios[[scenario_name]]
    p_sel <- all_tests[[scenario_name]][[fname]]
    
    if (!is.null(p_sel) && !is.na(p_sel)) {
      total <- total + 1
      if (p_sel == true_p) {
        correct <- correct + 1
      }
    }
  }
  
  cat(sprintf("%-20s: %2d/%2d (%.1f%%)\n", fname, correct, total, correct/total*100))
}

cat("\n")
