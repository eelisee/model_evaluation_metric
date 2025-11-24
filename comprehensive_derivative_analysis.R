#!/usr/bin/env Rscript

# Comprehensive analysis of derivative formulations
# Saves CSV and creates plots for all scenarios

# Load scenario data
load_scenario <- function(scenario_name) {
  file_path <- sprintf("results/%s/detailed_results.csv", scenario_name)
  data <- read.csv(file_path)
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  names(avg_data)[2] <- "R2_mean"
  avg_data <- avg_data[order(avg_data$p), ]
  return(avg_data)
}

# Formulation 1: Current code implementation (with M_p[0]=0 assumption)
compute_current_code <- function(M_p) {
  n <- length(M_p)
  
  delta1 <- numeric(n)
  for (i in 2:(n-1)) {
    delta1[i] <- (M_p[i+1] - M_p[i-1]) / 2
  }
  delta1[1] <- M_p[2] - M_p[1]
  delta1[n] <- M_p[n] - M_p[n-1]
  
  delta2 <- numeric(n)
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
  }
  delta2[1] <- -2 * M_p[1] + M_p[2]  # Assumes M_p[0] = 0
  delta2[n] <- M_p[n-1] - 2 * M_p[n]  # Assumes M_p[n+1] = M_p[n]
  
  delta3 <- numeric(n-1)
  for (i in 1:(n-1)) {
    delta3[i] <- delta2[i+1] - delta2[i]
  }
  
  argmin_delta3 <- which.min(delta3)
  selected_p <- argmin_delta3
  
  return(list(
    delta1 = delta1,
    delta2 = delta2,
    delta3 = c(delta3, NA),
    selected_p = selected_p,
    formulation = "current_code"
  ))
}

# Formulation 2: Proper boundary conditions (forward/backward at boundaries)
compute_proper_boundaries <- function(M_p) {
  n <- length(M_p)
  
  # First derivative
  delta1 <- numeric(n)
  delta1[1] <- M_p[2] - M_p[1]  # Forward difference
  delta1[n] <- M_p[n] - M_p[n-1]  # Backward difference
  for (i in 2:(n-1)) {
    delta1[i] <- (M_p[i+1] - M_p[i-1]) / 2  # Central difference
  }
  
  # Second derivative from delta1
  delta2 <- numeric(n)
  delta2[1] <- delta1[2] - delta1[1]  # Forward difference of delta1
  delta2[n] <- delta1[n] - delta1[n-1]  # Backward difference of delta1
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
  }
  
  # Third derivative from delta2
  delta3 <- numeric(n)
  delta3[1] <- delta2[2] - delta2[1]
  delta3[n] <- delta2[n] - delta2[n-1]
  for (i in 2:(n-1)) {
    delta3[i] <- delta2[i+1] - delta2[i]
  }
  
  argmin_delta3 <- which.min(delta3)
  selected_p <- argmin_delta3
  
  return(list(
    delta1 = delta1,
    delta2 = delta2,
    delta3 = delta3,
    selected_p = selected_p,
    formulation = "proper_boundaries"
  ))
}

# Formulation 3: Proper boundaries with +1 correction
compute_proper_plus1 <- function(M_p) {
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
  
  argmin_delta3 <- which.min(delta3)
  if (argmin_delta3 < n) {
    selected_p <- argmin_delta3 + 1
  } else {
    selected_p <- n
  }
  
  return(list(
    delta1 = delta1,
    delta2 = delta2,
    delta3 = delta3,
    selected_p = selected_p,
    formulation = "proper_plus1"
  ))
}

# Create plots for a scenario
create_plots <- function(scenario_name, true_p, p_vals, M_p, results_list) {
  
  # Save as PDF with base R plots
  pdf(sprintf("derivative_analysis_%s.pdf", scenario_name), width = 16, height = 12)
  par(mfrow = c(3, 4))
  
  # Plot M_p
  plot(p_vals, M_p, type = "b", col = "blue", lwd = 2, pch = 16,
       main = sprintf("%s: M_p = R²/p", scenario_name), xlab = "p", ylab = "M_p")
  abline(v = true_p, col = "red", lty = 2, lwd = 2)
  legend("topright", legend = sprintf("True p*=%d", true_p), 
         col = "red", lty = 2, bty = "n")
  
  # Plot derivatives for each formulation
  colors <- c("orange", "darkgreen", "purple")
  form_names <- c("current_code", "proper_boundaries", "proper_plus1")
  
  for (i in 1:3) {
    res <- results_list[[i]]
    color <- colors[i]
    
    # Delta1
    plot(p_vals, res$delta1, type = "b", col = color, lwd = 2, pch = 16,
         main = sprintf("Δ₁ (%s, p=%d)", form_names[i], res$selected_p),
         xlab = "p", ylab = "Δ₁")
    abline(h = 0, lty = 3)
    abline(v = res$selected_p, col = color, lty = 2)
    abline(v = true_p, col = "red", lty = 2)
    
    # Delta2
    plot(p_vals, res$delta2, type = "b", col = color, lwd = 2, pch = 16,
         main = sprintf("Δ₂ (%s, p=%d)", form_names[i], res$selected_p),
         xlab = "p", ylab = "Δ₂")
    abline(h = 0, lty = 3)
    abline(v = res$selected_p, col = color, lty = 2)
    abline(v = true_p, col = "red", lty = 2)
    
    # Delta3
    plot(p_vals, res$delta3, type = "b", col = color, lwd = 2, pch = 16,
         main = sprintf("Δ₃ (%s, p=%d)", form_names[i], res$selected_p),
         xlab = "p", ylab = "Δ₃")
    abline(h = 0, lty = 3)
    abline(v = res$selected_p, col = color, lty = 2)
    abline(v = true_p, col = "red", lty = 2)
  }
  
  dev.off()
  
  return(NULL)
}

# Main analysis
cat("================================================================================\n")
cat("COMPREHENSIVE DERIVATIVE FORMULATION ANALYSIS\n")
cat("================================================================================\n\n")

# Define scenarios
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

# Initialize results storage
all_results <- list()

# Process each scenario
for (scenario in names(true_p_star)) {
  cat(sprintf("\nProcessing %s...\n", scenario))
  
  data <- load_scenario(scenario)
  p_vals <- data$p
  R2_vals <- data$R2_mean
  M_p <- R2_vals / p_vals
  true_p <- true_p_star[[scenario]]
  n <- length(p_vals)
  
  # Compute all three formulations
  res_current <- compute_current_code(M_p)
  res_proper <- compute_proper_boundaries(M_p)
  res_plus1 <- compute_proper_plus1(M_p)
  
  # Store results for this scenario
  for (i in 1:n) {
    all_results[[length(all_results) + 1]] <- data.frame(
      scenario = scenario,
      true_p = true_p,
      p = p_vals[i],
      M_p = M_p[i],
      R2 = R2_vals[i],
      
      # Current code formulation
      delta1_current = res_current$delta1[i],
      delta2_current = res_current$delta2[i],
      delta3_current = res_current$delta3[i],
      selected_p_current = res_current$selected_p,
      
      # Proper boundaries formulation
      delta1_proper = res_proper$delta1[i],
      delta2_proper = res_proper$delta2[i],
      delta3_proper = res_proper$delta3[i],
      selected_p_proper = res_proper$selected_p,
      
      # Proper +1 formulation
      delta1_plus1 = res_plus1$delta1[i],
      delta2_plus1 = res_plus1$delta2[i],
      delta3_plus1 = res_plus1$delta3[i],
      selected_p_plus1 = res_plus1$selected_p,
      
      stringsAsFactors = FALSE
    )
  }
  
  # Create plots
  create_plots(scenario, true_p, p_vals, M_p, 
               list(res_current, res_proper, res_plus1))
  
  cat(sprintf("  Current code: selected p=%d (true=%d) %s\n", 
              res_current$selected_p, true_p, 
              ifelse(res_current$selected_p == true_p, "✓", "✗")))
  cat(sprintf("  Proper boundaries: selected p=%d (true=%d) %s\n", 
              res_proper$selected_p, true_p, 
              ifelse(res_proper$selected_p == true_p, "✓", "✗")))
  cat(sprintf("  Proper +1: selected p=%d (true=%d) %s\n", 
              res_plus1$selected_p, true_p, 
              ifelse(res_plus1$selected_p == true_p, "✓", "✗")))
}

# Combine all results into single dataframe
df_results <- do.call(rbind, all_results)

# Save to CSV
csv_file <- "derivative_formulations_analysis.csv"
write.csv(df_results, csv_file, row.names = FALSE)
cat(sprintf("\n\nResults saved to: %s\n", csv_file))

# Create formulation documentation
doc <- data.frame(
  Formulation = c("current_code", "proper_boundaries", "proper_plus1"),
  
  Delta1_Interior = rep("(M_p[i+1] - M_p[i-1]) / 2", 3),
  Delta1_Left = c("M_p[2] - M_p[1]", "M_p[2] - M_p[1]", "M_p[2] - M_p[1]"),
  Delta1_Right = c("M_p[n] - M_p[n-1]", "M_p[n] - M_p[n-1]", "M_p[n] - M_p[n-1]"),
  
  Delta2_Interior = rep("M_p[i-1] - 2*M_p[i] + M_p[i+1]", 3),
  Delta2_Left = c("-2*M_p[1] + M_p[2] (assumes M_p[0]=0)", 
                  "delta1[2] - delta1[1]", 
                  "delta1[2] - delta1[1]"),
  Delta2_Right = c("M_p[n-1] - 2*M_p[n] (assumes M_p[n+1]=M_p[n])", 
                   "delta1[n] - delta1[n-1]", 
                   "delta1[n] - delta1[n-1]"),
  
  Delta3_Computation = c("delta2[i+1] - delta2[i] (length n-1, last value NA)", 
                         "delta2[i+1] - delta2[i] at interior; forward/backward at boundaries (length n)", 
                         "Same as proper_boundaries"),
  
  Selection_Rule = c("argmin(delta3)", 
                     "argmin(delta3)", 
                     "argmin(delta3) + 1"),
  
  stringsAsFactors = FALSE
)

doc_file <- "derivative_formulations_documentation.csv"
write.csv(doc, doc_file, row.names = FALSE)
cat(sprintf("Formulation documentation saved to: %s\n", doc_file))

# Summary statistics
cat("\n")
cat("================================================================================\n")
cat("SUMMARY STATISTICS\n")
cat("================================================================================\n\n")

summary_df <- data.frame(
  Scenario = character(),
  True_p = integer(),
  Current_Selected = integer(),
  Current_Correct = character(),
  Proper_Selected = integer(),
  Proper_Correct = character(),
  Plus1_Selected = integer(),
  Plus1_Correct = character(),
  stringsAsFactors = FALSE
)

for (scenario in names(true_p_star)) {
  scenario_data <- df_results[df_results$scenario == scenario, ][1, ]
  
  summary_df <- rbind(summary_df, data.frame(
    Scenario = scenario,
    True_p = scenario_data$true_p,
    Current_Selected = scenario_data$selected_p_current,
    Current_Correct = ifelse(scenario_data$selected_p_current == scenario_data$true_p, "✓", "✗"),
    Proper_Selected = scenario_data$selected_p_proper,
    Proper_Correct = ifelse(scenario_data$selected_p_proper == scenario_data$true_p, "✓", "✗"),
    Plus1_Selected = scenario_data$selected_p_plus1,
    Plus1_Correct = ifelse(scenario_data$selected_p_plus1 == scenario_data$true_p, "✓", "✗"),
    stringsAsFactors = FALSE
  ))
}

print(summary_df)

cat("\n")
current_success <- sum(summary_df$Current_Correct == "✓")
proper_success <- sum(summary_df$Proper_Correct == "✓")
plus1_success <- sum(summary_df$Plus1_Correct == "✓")
total <- nrow(summary_df)

cat(sprintf("Current code (with M_p[0]=0):  %d/%d (%.1f%%)\n", current_success, total, 100*current_success/total))
cat(sprintf("Proper boundaries:             %d/%d (%.1f%%)\n", proper_success, total, 100*proper_success/total))
cat(sprintf("Proper boundaries + 1:         %d/%d (%.1f%%)\n", plus1_success, total, 100*plus1_success/total))

cat("\n\nAll plots saved to derivative_analysis_[scenario].pdf files\n")
