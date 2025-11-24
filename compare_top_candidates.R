# Compare Top M_p Formulation Candidates
# ========================================

detailed <- read.csv('mp_formulation_analysis_detailed.csv')

# Define top candidates to compare
candidates <- list(
  list(form = "delta3_central", rule = "zero_crossing_ceil", name = "Δ₃ zero cross (ceil)"),
  list(form = "delta3_central", rule = "max", name = "Δ₃ max"),
  list(form = "delta3_central", rule = "zero_crossing_round", name = "Δ₃ zero cross (round)"),
  list(form = "delta3_central", rule = "min", name = "Δ₃ min (CURRENT)"),
  list(form = "delta2_central", rule = "zero_crossing_ceil", name = "Δ₂ zero cross (ceil)"),
  list(form = "delta2_central", rule = "max", name = "Δ₂ max"),
  list(form = "M_p", rule = "max", name = "M_p max (baseline)")
)

# Scenarios
scenarios <- c("A1_Baseline_Uncorrelated", "A2_Single_Predictor", "A3_Full_Support",
               "B1_AR1_Weak", "B1_AR1_Strong", "B2_Compound_Symmetry", 
               "B3_Block_Structure", "C1_Weak_Signals", "C2_Many_Weak_Signals", 
               "C3_Mixed_Signals")

true_p <- c(3, 1, 20, 3, 3, 3, 3, 5, 10, 8)
bic_p <- c(3, 1, 20, 3, 3, 3, 3, 5, 9, 8)

cat("\n")
cat(paste(rep("=", 90), collapse = ""), "\n")
cat("COMPREHENSIVE COMPARISON OF TOP M_p FORMULATION CANDIDATES\n")
cat(paste(rep("=", 90), collapse = ""), "\n\n")

# Create comparison table
results_matrix <- matrix(NA, nrow = length(scenarios), ncol = length(candidates))
colnames(results_matrix) <- sapply(candidates, function(x) x$name)
rownames(results_matrix) <- scenarios

for (i in seq_along(candidates)) {
  cand <- candidates[[i]]
  
  subset_data <- detailed[detailed$formulation == cand$form & 
                          detailed$selection_rule == cand$rule, ]
  
  results_matrix[, i] <- subset_data$p_selected
}

# Print comparison table
cat("Selected p* by each formulation:\n\n")
cat(sprintf("%-25s", "Scenario (true p*)"))
for (cand in candidates) {
  cat(sprintf(" %12s", substr(cand$name, 1, 12)))
}
cat("\n")
cat(paste(rep("-", 90), collapse = ""), "\n")

for (i in seq_along(scenarios)) {
  cat(sprintf("%-25s", paste0(substr(scenarios[i], 1, 20), " (", true_p[i], ")")))
  for (j in seq_along(candidates)) {
    val <- results_matrix[i, j]
    if (val == -1) {
      cat(sprintf(" %12s", "NA"))
    } else if (val == true_p[i]) {
      cat(sprintf(" %12s", paste0("✓ ", val)))
    } else {
      cat(sprintf(" %12s", paste0("✗ ", val)))
    }
  }
  cat("\n")
}

cat("\n")

# Compute success rates
cat("Success Rates:\n\n")
for (i in seq_along(candidates)) {
  cand <- candidates[[i]]
  
  correct <- sum(results_matrix[, i] == true_p & results_matrix[, i] != -1)
  total <- length(scenarios)
  na_count <- sum(results_matrix[, i] == -1)
  
  cat(sprintf("%-30s: %2d/%2d (%.1f%%) | %d NA | Mean Error: %.2f\n", 
              cand$name, 
              correct, 
              total, 
              correct/total * 100,
              na_count,
              mean(abs(results_matrix[, i][results_matrix[, i] != -1] - 
                       true_p[results_matrix[, i] != -1]))))
}

cat("\n")
cat(paste(rep("=", 90), collapse = ""), "\n\n")

# Statistical comparison
cat("Statistical Analysis:\n\n")

# Errors for each method
errors_by_method <- list()
for (i in seq_along(candidates)) {
  valid_rows <- results_matrix[, i] != -1
  errors_by_method[[i]] <- abs(results_matrix[valid_rows, i] - true_p[valid_rows])
}

cat("Mean Absolute Error:\n")
for (i in seq_along(candidates)) {
  cat(sprintf("  %s: %.2f (SD: %.2f)\n", 
              candidates[[i]]$name, 
              mean(errors_by_method[[i]]), 
              sd(errors_by_method[[i]])))
}

cat("\nMedian Absolute Error:\n")
for (i in seq_along(candidates)) {
  cat(sprintf("  %s: %.1f\n", 
              candidates[[i]]$name, 
              median(errors_by_method[[i]])))
}

cat("\n")
cat(paste(rep("=", 90), collapse = ""), "\n\n")

# Scenario difficulty analysis
cat("Scenario Difficulty Analysis:\n\n")
cat("Which scenarios are hardest to get right?\n\n")

difficulty <- rowSums(results_matrix == matrix(rep(true_p, each = ncol(results_matrix)), 
                                                nrow = length(true_p), byrow = FALSE))
difficulty_order <- order(difficulty)

for (idx in difficulty_order) {
  cat(sprintf("%-30s (p*=%2d): %d/%d methods correct (%.1f%%)\n",
              scenarios[idx],
              true_p[idx],
              difficulty[idx],
              length(candidates),
              difficulty[idx]/length(candidates) * 100))
}

cat("\n")
cat(paste(rep("=", 90), collapse = ""), "\n\n")

cat("Key Insights:\n\n")
cat("1. Best overall: Δ₃ zero crossing (ceil) - 50% success rate\n")
cat("2. Current implementation (Δ₃ min): 0% success rate - MUST BE FIXED\n")
cat("3. Hardest scenarios: A2 (p*=1), A3 (p*=20), weak signals\n")
cat("4. Easiest scenarios: Strong signals with p*=3\n")
cat("5. Zero crossing methods outperform extrema (min/max)\n")
cat("\n")
