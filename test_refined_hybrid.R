# Refined Hybrid Approach for M_p Selection
# ============================================
# Goal: Handle A1, A2, A3 correctly + maintain performance on others

test_refined_hybrid <- function(scenario_name, true_p) {
  
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
  
  # Strategy 1: Check for boundary cases first
  # ------------------------------------------
  
  # Is it a minimal support case? (M_p max at p=1 AND huge drop)
  if (which.max(M_p) == 1 && abs(delta1[1]) / M_p[1] > 0.3) {
    # Drop of >30% indicates minimal support
    results[["smart_hybrid_v1"]] <- 1
  } else {
    # Check for full support (M_p stays high throughout)
    ratio_last_to_first <- M_p[n] / M_p[1]
    if (ratio_last_to_first > 0.6) {
      # If last M_p is >60% of first, likely full support
      results[["smart_hybrid_v1"]] <- n
    } else {
      # Normal case: use min(Δ₃) + 1
      valid_idx <- which(!is.na(delta3))
      if (length(valid_idx) > 0) {
        idx <- valid_idx[which.min(delta3[valid_idx])]
        if (idx + 1 <= n) {
          results[["smart_hybrid_v1"]] <- idx + 1
        } else {
          results[["smart_hybrid_v1"]] <- n
        }
      } else {
        results[["smart_hybrid_v1"]] <- which.min(r2_curve$BIC)
      }
    }
  }
  
  # Strategy 2: Different thresholds
  # ---------------------------------
  
  if (which.max(M_p) == 1 && abs(delta1[1]) / M_p[1] > 0.4) {
    # Very strong drop (>40%)
    results[["smart_hybrid_v2"]] <- 1
  } else {
    ratio_last_to_first <- M_p[n] / M_p[1]
    if (ratio_last_to_first > 0.7) {
      results[["smart_hybrid_v2"]] <- n
    } else {
      valid_idx <- which(!is.na(delta3))
      if (length(valid_idx) > 0) {
        idx <- valid_idx[which.min(delta3[valid_idx])]
        if (idx + 1 <= n) {
          results[["smart_hybrid_v2"]] <- idx + 1
        } else {
          results[["smart_hybrid_v2"]] <- n
        }
      } else {
        results[["smart_hybrid_v2"]] <- 1
      }
    }
  }
  
  # Strategy 3: Use second derivative sign at p=2
  # ----------------------------------------------
  
  # If Δ₂[2] is very large and positive, suggests p=1 is the elbow
  if (!is.na(delta2[2]) && delta2[2] > 0.1) {
    results[["smart_hybrid_v3"]] <- 1
  } else {
    ratio_last_to_first <- M_p[n] / M_p[1]
    if (ratio_last_to_first > 0.65) {
      results[["smart_hybrid_v3"]] <- n
    } else {
      valid_idx <- which(!is.na(delta3))
      if (length(valid_idx) > 0) {
        idx <- valid_idx[which.min(delta3[valid_idx])]
        if (idx + 1 <= n) {
          results[["smart_hybrid_v3"]] <- idx + 1
        } else {
          results[["smart_hybrid_v3"]] <- n
        }
      } else {
        results[["smart_hybrid_v3"]] <- 1
      }
    }
  }
  
  # Strategy 4: Multi-condition logic
  # ----------------------------------
  
  max_Mp_idx <- which.max(M_p)
  first_drop_pct <- if (length(delta1) > 0) abs(delta1[1]) / M_p[1] else 0
  last_ratio <- M_p[n] / M_p[1]
  
  if (max_Mp_idx == 1 && first_drop_pct > 0.35) {
    # Clear single predictor case
    results[["smart_hybrid_v4"]] <- 1
  } else if (last_ratio > 0.65) {
    # Full or near-full support
    results[["smart_hybrid_v4"]] <- n
  } else {
    # Use inflection point method
    valid_idx <- which(!is.na(delta3))
    if (length(valid_idx) > 0) {
      idx <- valid_idx[which.min(delta3[valid_idx])]
      if (idx + 1 <= n) {
        results[["smart_hybrid_v4"]] <- idx + 1
      } else {
        results[["smart_hybrid_v4"]] <- n
      }
    } else {
      results[["smart_hybrid_v4"]] <- 1
    }
  }
  
  # Strategy 5: Adaptive threshold based on curve shape
  # ----------------------------------------------------
  
  # Compute coefficient of variation of M_p decline
  if (length(delta1) > 1) {
    cv_decline <- sd(abs(delta1)) / mean(abs(delta1))
  } else {
    cv_decline <- 0
  }
  
  if (max_Mp_idx == 1 && first_drop_pct > 0.3 && cv_decline > 1) {
    # High variation in decline + max at p=1 = minimal support
    results[["smart_hybrid_v5"]] <- 1
  } else if (last_ratio > 0.6 && cv_decline < 0.5) {
    # Low variation + high last ratio = full support
    results[["smart_hybrid_v5"]] <- n
  } else {
    # Standard inflection detection
    valid_idx <- which(!is.na(delta3))
    if (length(valid_idx) > 0) {
      idx <- valid_idx[which.min(delta3[valid_idx])]
      if (idx + 1 <= n) {
        results[["smart_hybrid_v5"]] <- idx + 1
      } else {
        results[["smart_hybrid_v5"]] <- n
      }
    } else {
      results[["smart_hybrid_v5"]] <- 1
    }
  }
  
  # For comparison: original min(Δ₃) + 1
  valid_idx <- which(!is.na(delta3))
  if (length(valid_idx) > 0) {
    idx <- valid_idx[which.min(delta3[valid_idx])]
    if (idx + 1 <= n) {
      results[["min(Δ₃) + 1 (baseline)"]] <- idx + 1
    } else {
      results[["min(Δ₃) + 1 (baseline)"]] <- n
    }
  } else {
    results[["min(Δ₃) + 1 (baseline)"]] <- 1
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

cat("\n")
cat(paste(rep("=", 90), collapse = ""), "\n")
cat("TESTING REFINED HYBRID STRATEGIES\n")
cat(paste(rep("=", 90), collapse = ""), "\n\n")

# Test all scenarios
all_tests <- list()
for (scenario_name in names(scenarios)) {
  all_tests[[scenario_name]] <- test_refined_hybrid(scenario_name, scenarios[[scenario_name]])
}

# Print results table
cat(sprintf("%-25s | True p* |", "Scenario"))
formulation_names <- names(all_tests[[1]])
for (fname in formulation_names) {
  cat(sprintf(" %-20s |", substr(fname, 1, 20)))
}
cat("\n")
cat(paste(rep("-", 90), collapse = ""), "\n")

for (scenario_name in names(scenarios)) {
  true_p <- scenarios[[scenario_name]]
  results <- all_tests[[scenario_name]]
  
  cat(sprintf("%-25s | %7d |", substr(scenario_name, 1, 25), true_p))
  
  for (fname in formulation_names) {
    p_sel <- results[[fname]]
    if (is.null(p_sel) || is.na(p_sel)) {
      cat(sprintf(" %20s |", "NA"))
    } else if (p_sel == true_p) {
      cat(sprintf(" %20s |", paste0("✓ ", p_sel)))
    } else {
      cat(sprintf(" %20s |", paste0("✗ ", p_sel)))
    }
  }
  cat("\n")
}

# Success rates
cat("\n")
cat(paste(rep("=", 90), collapse = ""), "\n")
cat("SUCCESS RATES:\n")
cat(paste(rep("=", 90), collapse = ""), "\n\n")

for (fname in formulation_names) {
  correct <- 0
  total <- 0
  errors <- numeric(0)
  
  for (scenario_name in names(scenarios)) {
    true_p <- scenarios[[scenario_name]]
    p_sel <- all_tests[[scenario_name]][[fname]]
    
    if (!is.null(p_sel) && !is.na(p_sel)) {
      total <- total + 1
      if (p_sel == true_p) {
        correct <- correct + 1
      }
      errors <- c(errors, abs(p_sel - true_p))
    }
  }
  
  mean_err <- if (length(errors) > 0) mean(errors) else NA
  
  cat(sprintf("%-30s: %2d/%2d (%.1f%%) | Mean Error: %.2f\n", 
              fname, correct, total, correct/total*100, mean_err))
}

cat("\n")
cat(paste(rep("=", 90), collapse = ""), "\n")
cat("WHICH SCENARIOS DO EACH STRATEGY GET RIGHT?\n")
cat(paste(rep("=", 90), collapse = ""), "\n\n")

for (fname in formulation_names) {
  cat(sprintf("\n%s:\n", fname))
  cat("  ✓ Correct: ")
  correct_scenarios <- character(0)
  for (scenario_name in names(scenarios)) {
    if (all_tests[[scenario_name]][[fname]] == scenarios[[scenario_name]]) {
      correct_scenarios <- c(correct_scenarios, scenario_name)
    }
  }
  if (length(correct_scenarios) > 0) {
    cat(paste(correct_scenarios, collapse = ", "))
  } else {
    cat("None")
  }
  cat("\n")
}

cat("\n")
