#!/usr/bin/env Rscript

# Detailed curvature analysis for all scenarios
# Check if Delta_2 is truly always positive

# Load scenario data
load_scenario <- function(scenario_name) {
  file_path <- sprintf("results/%s/detailed_results.csv", scenario_name)
  data <- read.csv(file_path)
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  names(avg_data)[2] <- "R2_mean"
  avg_data <- avg_data[order(avg_data$p), ]
  return(avg_data$R2_mean)
}

# Compute derivatives with proper boundary conditions
compute_derivatives <- function(M_p) {
  n <- length(M_p)
  delta1 <- numeric(n)
  delta2 <- numeric(n)
  
  # First derivative
  delta1[1] <- M_p[2] - M_p[1]
  delta1[n] <- M_p[n] - M_p[n-1]
  for (i in 2:(n-1)) {
    delta1[i] <- (M_p[i+1] - M_p[i-1]) / 2
  }
  
  # Second derivative (curvature)
  delta2[1] <- delta1[2] - delta1[1]
  delta2[n] <- delta1[n] - delta1[n-1]
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
  }
  
  return(list(delta1=delta1, delta2=delta2))
}

# Scenario details
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

scenario_names <- c("A1", "A2", "A3", "B1W", "B1S", "B2", "B3", "C1", "C2", "C3")
true_p <- c(3, 1, 20, 3, 3, 3, 3, 5, 10, 8)

cat("================================================================================\n")
cat("DETAILED CURVATURE ANALYSIS FOR ALL SCENARIOS\n")
cat("================================================================================\n\n")

results <- data.frame(
  Scenario = character(),
  p_star = integer(),
  Sign_Changes_Delta2 = character(),
  Delta2_Range = character(),
  Always_Positive = character(),
  Always_Negative = character(),
  Mixed_Signs = character(),
  Delta2_at_pstar = numeric(),
  Notes = character(),
  stringsAsFactors = FALSE
)

for (idx in seq_along(scenarios)) {
  scenario <- scenarios[idx]
  name <- scenario_names[idx]
  p_star <- true_p[idx]
  
  cat(sprintf("Analyzing %s (p*=%d):\n", name, p_star))
  cat(sprintf("  Full name: %s\n", scenario))
  
  # Load data
  R2 <- load_scenario(scenario)
  p_vals <- 1:length(R2)
  M_p <- R2 / p_vals
  
  # Compute derivatives
  derivs <- compute_derivatives(M_p)
  delta2 <- derivs$delta2
  
  # Analyze Delta_2
  n_positive <- sum(delta2 > 0)
  n_negative <- sum(delta2 < 0)
  n_zero <- sum(abs(delta2) < 1e-10)
  
  # Find sign changes
  sign_changes <- which(diff(sign(delta2)) != 0)
  n_sign_changes <- length(sign_changes)
  
  # Classification
  if (n_positive == length(delta2)) {
    classification <- "All positive (convex)"
  } else if (n_negative == length(delta2)) {
    classification <- "All negative (concave)"
  } else {
    classification <- sprintf("Mixed (%d pos, %d neg)", n_positive, n_negative)
  }
  
  # Details
  cat(sprintf("  Delta_2 analysis:\n"))
  cat(sprintf("    Positive curvature: %d/%d points\n", n_positive, length(delta2)))
  cat(sprintf("    Negative curvature: %d/%d points\n", n_negative, length(delta2)))
  cat(sprintf("    Sign changes: %d\n", n_sign_changes))
  if (n_sign_changes > 0) {
    cat(sprintf("    Sign change locations: p = %s\n", 
                paste(sign_changes, collapse=", ")))
  }
  cat(sprintf("    Range: [%.6f, %.6f]\n", min(delta2), max(delta2)))
  cat(sprintf("    Delta_2(p*=%d) = %.6f\n", p_star, delta2[p_star]))
  cat(sprintf("    Classification: %s\n", classification))
  
  # Special notes
  notes <- ""
  if (n_sign_changes > 0) {
    notes <- sprintf("Sign changes at p=%s", paste(sign_changes, collapse=","))
  } else {
    notes <- "No sign changes (no classical inflection point)"
  }
  
  # Add to results
  results <- rbind(results, data.frame(
    Scenario = name,
    p_star = p_star,
    Sign_Changes_Delta2 = ifelse(n_sign_changes > 0, 
                                  paste(sign_changes, collapse=","), 
                                  "None"),
    Delta2_Range = sprintf("[%.4f, %.4f]", min(delta2), max(delta2)),
    Always_Positive = ifelse(n_positive == length(delta2), "Yes", "No"),
    Always_Negative = ifelse(n_negative == length(delta2), "Yes", "No"),
    Mixed_Signs = ifelse(n_positive > 0 && n_negative > 0, "Yes", "No"),
    Delta2_at_pstar = delta2[p_star],
    Notes = notes,
    stringsAsFactors = FALSE
  ))
  
  cat("\n")
}

cat("================================================================================\n")
cat("SUMMARY TABLE\n")
cat("================================================================================\n\n")

print(results, row.names = FALSE)

# Save to CSV
write.csv(results, "curvature_analysis.csv", row.names = FALSE)
cat("\nSaved to: curvature_analysis.csv\n")

# Summary statistics
cat("\n================================================================================\n")
cat("SUMMARY STATISTICS\n")
cat("================================================================================\n\n")

cat(sprintf("Scenarios with only positive Delta_2 (always convex): %d/10\n", 
            sum(results$Always_Positive == "Yes")))
cat(sprintf("Scenarios with only negative Delta_2 (always concave): %d/10\n", 
            sum(results$Always_Negative == "Yes")))
cat(sprintf("Scenarios with mixed signs (have inflection points): %d/10\n", 
            sum(results$Mixed_Signs == "Yes")))
cat(sprintf("\nScenarios with at least one sign change in Delta_2: %d/10\n",
            sum(results$Sign_Changes_Delta2 != "None")))

cat("\n")
