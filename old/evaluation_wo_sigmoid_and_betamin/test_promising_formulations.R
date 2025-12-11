# Test Promising Formulations Across All Scenarios
# ==================================================

# Define scenarios and true p*
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

# Test these formulations (the ones that worked for A1)
test_formulation <- function(scenario_name, true_p) {
  
  csv_path <- file.path("results", scenario_name, "detailed_results.csv")
  if (!file.exists(csv_path)) return(NULL)
  
  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  
  R2_vals <- avg_data$R2
  p_vals <- avg_data$p
  n <- length(p_vals)
  
  # Compute M_p and derivatives
  M_p <- R2_vals / p_vals
  
  # Δ₂ (central difference)
  delta2 <- numeric(n)
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
  }
  delta2[1] <- NA
  delta2[n] <- NA
  
  # Δ₃
  delta3 <- diff(delta2)
  
  results <- list()
  
  # Formulation 1: min(Δ₂)
  valid_idx <- which(!is.na(delta2))
  if (length(valid_idx) > 0) {
    idx <- valid_idx[which.min(delta2[valid_idx])]
    results[["min(Δ₂)"]] <- p_vals[idx]
  } else {
    results[["min(Δ₂)"]] <- NA
  }
  
  # Formulation 2: Δ₂ zero crossing -→+
  idx <- NA
  for (i in 1:(n-1)) {
    if (!is.na(delta2[i]) && !is.na(delta2[i+1])) {
      if (delta2[i] < 0 && delta2[i+1] > 0) {
        idx <- i
        break
      }
    }
  }
  results[["Δ₂ zero cross -→+"]] <- if (!is.na(idx)) p_vals[idx] else NA
  
  # Formulation 3: max(Δ₃)
  valid_idx3 <- which(!is.na(delta3))
  if (length(valid_idx3) > 0) {
    idx <- valid_idx3[which.max(delta3[valid_idx3])]
    results[["max(Δ₃)"]] <- p_vals[idx]
  } else {
    results[["max(Δ₃)"]] <- NA
  }
  
  # Formulation 4: max(|Δ₃|)
  if (length(valid_idx3) > 0) {
    idx <- valid_idx3[which.max(abs(delta3[valid_idx3]))]
    results[["max(|Δ₃|)"]] <- p_vals[idx]
  } else {
    results[["max(|Δ₃|)"]] <- NA
  }
  
  # Formulation 5: Δ₃ zero crossing +→-
  idx <- NA
  for (i in 1:(length(delta3)-1)) {
    if (!is.na(delta3[i]) && !is.na(delta3[i+1])) {
      if (delta3[i] > 0 && delta3[i+1] < 0) {
        idx <- i
        break
      }
    }
  }
  results[["Δ₃ zero cross +→-"]] <- if (!is.na(idx)) p_vals[idx] else NA
  
  # Formulation 6: min(Δ₃) + 1  (THE KEY ONE!)
  if (length(valid_idx3) > 0) {
    idx <- valid_idx3[which.min(delta3[valid_idx3])]
    if (idx + 1 <= n) {
      results[["min(Δ₃) + 1"]] <- p_vals[idx + 1]
    } else {
      results[["min(Δ₃) + 1"]] <- NA
    }
  } else {
    results[["min(Δ₃) + 1"]] <- NA
  }
  
  # Formulation 7: max(Δ₂) - 1
  if (length(valid_idx) > 0) {
    idx <- valid_idx[which.max(delta2[valid_idx])]
    if (idx - 1 >= 1) {
      results[["max(Δ₂) - 1"]] <- p_vals[idx - 1]
    } else {
      results[["max(Δ₂) - 1"]] <- NA
    }
  } else {
    results[["max(Δ₂) - 1"]] <- NA
  }
  
  # BONUS: Test some additional variations
  
  # Formulation 8: max(Δ₃) - 1
  if (length(valid_idx3) > 0) {
    idx <- valid_idx3[which.max(delta3[valid_idx3])]
    if (idx - 1 >= 1) {
      results[["max(Δ₃) - 1"]] <- p_vals[idx - 1]
    } else {
      results[["max(Δ₃) - 1"]] <- NA
    }
  } else {
    results[["max(Δ₃) - 1"]] <- NA
  }
  
  # Formulation 9: max(Δ₃) + 1
  if (length(valid_idx3) > 0) {
    idx <- valid_idx3[which.max(delta3[valid_idx3])]
    if (idx + 1 <= n) {
      results[["max(Δ₃) + 1"]] <- p_vals[idx + 1]
    } else {
      results[["max(Δ₃) + 1"]] <- NA
    }
  } else {
    results[["max(Δ₃) + 1"]] <- NA
  }
  
  return(results)
}

cat("\n")
cat(paste(rep("=", 100), collapse = ""), "\n")
cat("TESTING PROMISING FORMULATIONS ACROSS ALL SCENARIOS\n")
cat(paste(rep("=", 100), collapse = ""), "\n\n")

# Collect results
all_results <- list()
for (scenario_name in names(scenarios)) {
  true_p <- scenarios[[scenario_name]]
  results <- test_formulation(scenario_name, true_p)
  all_results[[scenario_name]] <- list(true_p = true_p, results = results)
}

# Get formulation names
formulation_names <- names(all_results[[1]]$results)

# Print table
cat(sprintf("%-25s | True p* |", "Scenario"))
for (fname in formulation_names) {
  cat(sprintf(" %-17s |", substr(fname, 1, 17)))
}
cat("\n")
cat(paste(rep("-", 100), collapse = ""), "\n")

for (scenario_name in names(scenarios)) {
  true_p <- all_results[[scenario_name]]$true_p
  results <- all_results[[scenario_name]]$results
  
  cat(sprintf("%-25s | %7d |", substr(scenario_name, 1, 25), true_p))
  
  for (fname in formulation_names) {
    p_sel <- results[[fname]]
    if (is.na(p_sel)) {
      cat(sprintf(" %17s |", "NA"))
    } else if (p_sel == true_p) {
      cat(sprintf(" %17s |", paste0("✓ ", p_sel)))
    } else {
      cat(sprintf(" %17s |", paste0("✗ ", p_sel)))
    }
  }
  cat("\n")
}

cat("\n")
cat(paste(rep("=", 100), collapse = ""), "\n")
cat("SUCCESS RATES\n")
cat(paste(rep("=", 100), collapse = ""), "\n\n")

for (fname in formulation_names) {
  correct <- 0
  total <- 0
  na_count <- 0
  errors <- numeric(0)
  
  for (scenario_name in names(scenarios)) {
    true_p <- all_results[[scenario_name]]$true_p
    p_sel <- all_results[[scenario_name]]$results[[fname]]
    
    if (is.na(p_sel)) {
      na_count <- na_count + 1
    } else {
      total <- total + 1
      if (p_sel == true_p) {
        correct <- correct + 1
      }
      errors <- c(errors, abs(p_sel - true_p))
    }
  }
  
  success_rate <- if (total > 0) correct / total * 100 else 0
  mean_error <- if (length(errors) > 0) mean(errors) else NA
  
  cat(sprintf("%-25s: %2d/%2d (%.1f%%) | %d NA | Mean Error: %.2f\n",
              fname, correct, total, success_rate, na_count, mean_error))
}

cat("\n")
cat(paste(rep("=", 100), collapse = ""), "\n\n")

cat("ANALYSIS:\n\n")
cat("The formulation 'min(Δ₃) + 1' is interesting because:\n")
cat("  - It's essentially: p* = argmin(Δ₃) + 1\n")
cat("  - This shifts the index by 1 from the current (wrong) implementation\n")
cat("  - The +1 accounts for the fact that Δ₃ measures the change BETWEEN points\n")
cat("  - When Δ₃ is minimum (most negative), curvature is decreasing fastest\n")
cat("  - The inflection happens at the NEXT point (+1)\n\n")

cat("Let's check if this simple fix works!\n\n")
