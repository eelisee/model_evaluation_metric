# Quick Summary of Alternative Metrics Results
# ==============================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("ALTERNATIVE METRICS - QUICK SUMMARY\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Read results
results <- read.csv("alternative_metrics/comparison_results.csv")

cat("KEY FINDINGS:\n\n")

cat("1. M_p_scaled is IDENTICAL to M_p:\n")
cat(sprintf("   - Selections match: %d/%d scenarios (%.0f%%)\n", 
            sum(results$Mp == results$Mp_scaled, na.rm = TRUE),
            nrow(results),
            100 * mean(results$Mp == results$Mp_scaled, na.rm = TRUE)))
cat("   - Proof: M_p_scaled = M_p × p_max (constant multiplier)\n")
cat("   - The steep drop ordering is preserved under scaling!\n\n")

cat("2. M_p_minus slightly OVER-selects vs M_p:\n")
cat(sprintf("   - Mean deviation from true: M_p_minus = %.2f vs M_p = %.2f\n",
            mean(results$Mp_minus - results$p_true, na.rm = TRUE),
            mean(results$Mp - results$p_true, na.rm = TRUE)))
cat(sprintf("   - But still under-selects overall (negative deviation)\n"))
cat(sprintf("   - Selections match M_p: %d/%d scenarios\n\n",
            sum(results$Mp == results$Mp_minus, na.rm = TRUE),
            nrow(results)))

cat("3. M_p_sqrt uses SUBLINEAR penalization (sqrt(p) vs p):\n")
cat(sprintf("   - Mean deviation from true: M_p_sqrt = %.2f vs M_p = %.2f\n",
            mean(results$Mp_sqrt - results$p_true, na.rm = TRUE),
            mean(results$Mp - results$p_true, na.rm = TRUE)))
cat(sprintf("   - Softer complexity penalty leads to over-selection\n"))
cat(sprintf("   - Selections match M_p: %d/%d scenarios\n\n",
            sum(results$Mp == results$Mp_sqrt, na.rm = TRUE),
            nrow(results)))

cat("4. Performance comparison (F1 scores):\n")
cat(sprintf("   - M_p:        Mean=%.3f, Median=%.3f\n", 
            mean(results$F1_Mp, na.rm = TRUE),
            median(results$F1_Mp, na.rm = TRUE)))
cat(sprintf("   - M_p_minus:  Mean=%.3f, Median=%.3f\n",
            mean(results$F1_Mp_minus, na.rm = TRUE),
            median(results$F1_Mp_minus, na.rm = TRUE)))
cat(sprintf("   - M_p_sqrt:   Mean=%.3f, Median=%.3f\n",
            mean(results$F1_Mp_sqrt, na.rm = TRUE),
            median(results$F1_Mp_sqrt, na.rm = TRUE)))
cat(sprintf("   - M_p_scaled: Mean=%.3f, Median=%.3f (identical to M_p!)\n",
            mean(results$F1_Mp_scaled, na.rm = TRUE),
            median(results$F1_Mp_scaled, na.rm = TRUE)))
cat(sprintf("   - BIC:        Mean=%.3f, Median=%.3f (much better!)\n\n",
            mean(results$F1_BIC, na.rm = TRUE),
            median(results$F1_BIC, na.rm = TRUE)))

cat("CONCLUSION:\n")
cat("  - M_p_scaled is mathematically equivalent to M_p ✓\n")
cat("  - M_p_minus and M_p_sqrt both alter penalization (softer penalty)\n")
cat("  - All three \"joke\" metrics are inferior to BIC\n")
cat("  - The exercise confirms: Linear penalization (R²/p) is well-justified! 😄\n\n")

cat(paste(rep("=", 70), collapse = ""), "\n\n")
