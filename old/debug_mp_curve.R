# Debug M_p Curve for A1 Baseline
# =================================
# Analyze the M_p curve to understand why inflection point method fails

source("R/data_generation.R")
source("R/model_evaluation.R")
source("R/selection_rules.R")
source("R/scenarios.R")

# Generate A1 data
config <- scenario_a1_baseline_uncorrelated(seed = 1001)
data <- generate_data_advanced(config)

cat("TRUE SUPPORT:", which(data$true_beta != 0), "\n")
cat("TRUE BETA:", data$true_beta[data$true_beta != 0], "\n\n")

# Enumerate all models
subsets <- enumerate_models(config$p_max, strategy = "full")

# Evaluate all models
results <- evaluate_all_models_batch(
  X = data$X,
  y = data$y,
  subsets = subsets
)

# Get best models by p
best_models <- find_best_models_by_p(results)

cat("====================================================================\n")
cat("M_p CURVE ANALYSIS\n")
cat("====================================================================\n\n")

# Display M_p curve
cat("p\tM_p\t\tR²\t\tΔM_p\t\tΔ²M_p\n")
cat("--------------------------------------------------------------------\n")

p_vals <- best_models$p
Mp_vals <- best_models$Mp
R2_vals <- best_models$R2

dMp <- diff(Mp_vals)
d2Mp <- diff(dMp)

for (i in 1:nrow(best_models)) {
  delta_mp <- if (i < nrow(best_models)) dMp[i] else NA
  delta2_mp <- if (i <= length(d2Mp)) d2Mp[i] else NA
  
  cat(sprintf("%d\t%.6f\t%.6f\t%.6f\t%.6f\n", 
              p_vals[i], Mp_vals[i], R2_vals[i],
              ifelse(is.na(delta_mp), 0, delta_mp),
              ifelse(is.na(delta2_mp), 0, delta2_mp)))
}

cat("\n====================================================================\n")
cat("SECOND DERIVATIVE ANALYSIS\n")
cat("====================================================================\n\n")

cat("Second derivatives (d²M_p):\n")
for (i in 1:length(d2Mp)) {
  sign_char <- if (d2Mp[i] < 0) "−" else if (d2Mp[i] > 0) "+" else "0"
  cat(sprintf("  Position %d→%d→%d: d²M_p = %.6f [%s]\n", 
              i, i+1, i+2, d2Mp[i], sign_char))
}

cat("\n")
cat("Sign changes (− to +):\n")
sign_changes <- which(d2Mp[-length(d2Mp)] < 0 & d2Mp[-1] > 0)
if (length(sign_changes) > 0) {
  for (sc in sign_changes) {
    cat(sprintf("  ✓ Inflection at position %d (p = %d)\n", sc+1, p_vals[sc+1]))
  }
} else {
  cat("  ✗ NO INFLECTION POINT FOUND\n")
  cat("  → M_p curve is monotone or has wrong curvature\n")
}

cat("\n====================================================================\n")
cat("SELECTION RESULT\n")
cat("====================================================================\n\n")

# Apply selection rule
selection <- selection_rule_mp(best_models)

cat("Method:", selection$method, "\n")
cat("Selected p*:", selection$p_star, "\n")
cat("Inflection point:", selection$inflection_point, "\n")
cat("Is degenerate:", selection$is_degenerate, "\n\n")

cat("Selected model subset:\n")
print(selection$selected_model$subset[[1]])

cat("\n====================================================================\n")
cat("PLOT M_p CURVE\n")
cat("====================================================================\n\n")

# Create plot
png("debug_mp_curve_a1.png", width = 800, height = 600)
par(mfrow = c(2, 2))

# Plot 1: M_p curve
plot(p_vals, Mp_vals, type = "b", pch = 19, col = "blue",
     xlab = "Model Complexity (p)", ylab = "M_p",
     main = "M_p Curve")
abline(v = selection$p_star, col = "red", lty = 2)
abline(v = which(data$true_beta != 0)[1], col = "green", lty = 2)
legend("topright", c("Selected p*", "True p"), col = c("red", "green"), lty = 2)

# Plot 2: R² curve
plot(p_vals, R2_vals, type = "b", pch = 19, col = "darkgreen",
     xlab = "Model Complexity (p)", ylab = "R²",
     main = "R² Curve")
abline(v = selection$p_star, col = "red", lty = 2)

# Plot 3: First derivative
plot(p_vals[-1], dMp, type = "b", pch = 19, col = "orange",
     xlab = "Model Complexity (p)", ylab = "ΔM_p",
     main = "First Derivative (ΔM_p)")
abline(h = 0, col = "gray", lty = 2)

# Plot 4: Second derivative
plot(p_vals[-c(1,2)], d2Mp, type = "b", pch = 19, col = "purple",
     xlab = "Model Complexity (p)", ylab = "Δ²M_p",
     main = "Second Derivative (Δ²M_p)")
abline(h = 0, col = "gray", lty = 2)
if (length(sign_changes) > 0) {
  points(p_vals[sign_changes+1], d2Mp[sign_changes], pch = 8, col = "red", cex = 2)
}

dev.off()

cat("Plot saved to: debug_mp_curve_a1.png\n\n")
