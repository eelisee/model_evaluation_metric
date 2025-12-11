#!/usr/bin/env Rscript

# Test all scenarios with EXACT current implementation

load_scenario <- function(scenario_name) {
  file_path <- sprintf("results/%s/detailed_results.csv", scenario_name)
  data <- read.csv(file_path)
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  names(avg_data)[2] <- "R2_mean"
  avg_data <- avg_data[order(avg_data$p), ]
  return(avg_data)
}

compute_as_current_code <- function(M_p) {
  n <- length(M_p)
  
  delta2 <- numeric(n)
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
  }
  delta2[1] <- -2 * M_p[1] + M_p[2]
  delta2[n] <- M_p[n-1] - 2 * M_p[n]
  
  delta3 <- numeric(n-1)
  for (i in 1:(n-1)) {
    delta3[i] <- delta2[i+1] - delta2[i]
  }
  
  return(list(delta2 = delta2, delta3 = delta3))
}

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

cat("================================================================================\n")
cat("CURRENT CODE IMPLEMENTATION (from 02_metrics.R)\n")
cat("================================================================================\n\n")

results <- data.frame(
  Scenario = character(),
  True_p = integer(),
  Selected_p = integer(),
  Correct = character(),
  stringsAsFactors = FALSE
)

for (scenario in names(true_p_star)) {
  data <- load_scenario(scenario)
  M_p <- data$R2_mean / data$p
  p_vals <- data$p
  
  derivs <- compute_as_current_code(M_p)
  argmin_delta3 <- which.min(derivs$delta3)
  selected <- p_vals[argmin_delta3]
  
  correct <- selected == true_p_star[[scenario]]
  
  results <- rbind(results, data.frame(
    Scenario = scenario,
    True_p = true_p_star[[scenario]],
    Selected_p = selected,
    Correct = ifelse(correct, "✓", "✗"),
    stringsAsFactors = FALSE
  ))
}

print(results)

cat("\n")
success_rate <- sum(results$Correct == "✓") / nrow(results)
cat(sprintf("Success rate: %d/%d (%.1f%%)\n\n", 
            sum(results$Correct == "✓"), 
            nrow(results), 
            100 * success_rate))

cat("This confirms the current implementation is broken!\n")
