# Test script for sigmoid_mp implementation
# ==========================================

# Load modules
source("R/01_data_generation.R")
source("R/02_metrics.R")
source("R/03_evaluation.R")

# Test scenario A1 with sigmoid method
cat("\n=== Testing sigmoid_mp on scenario A1 ===\n\n")

# Define simple test scenario
test_scenario <- list(
  name = "A1_Baseline_Uncorrelated",
  description = "Uncorrelated predictors, sparse β, Gaussian noise",
  n = 500,
  p = 20,
  sigma_structure = "identity",
  rho = 0,
  support_spec = 3,
  signal_strength = "strong",
  sigma_eps = 0.2,
  seed = 1001
)

# Generate data
cat("Generating data...\n")
data <- generate_data(test_scenario)
cat(sprintf("  True p*: %d\n", data$p_true))
cat(sprintf("  True support: %s\n\n", paste(data$support_true, collapse = ", ")))

# Compute R² curve
cat("Computing R² curve...\n")
r2_curve <- compute_r2_curve(data$X, data$y, n_cores = 1)
cat("  ✓ Done\n\n")

# Test sigmoid method
cat("Testing sigmoid_mp method...\n")
sigmoid_result <- metric_sigmoid_mp(r2_curve)

cat(sprintf("\n  Selected p*: %d\n", sigmoid_result$p_star))
cat(sprintf("  Fit method: %s\n", sigmoid_result$method))

if (!is.null(sigmoid_result$params)) {
  cat("\n  Fitted parameters:\n")
  params <- sigmoid_result$params
  cat(sprintf("    alpha (lower asymptote):  %.6f\n", params["alpha"]))
  cat(sprintf("    beta  (scale):            %.6f\n", params["beta"]))
  cat(sprintf("    gamma (steepness):        %.6f\n", params["gamma"]))
  cat(sprintf("    delta (inflection):       %.6f\n", params["delta"]))
  cat(sprintf("    delta (unrounded):        %.6f\n", sigmoid_result$delta))
} else {
  cat("\n  Fit failed!\n")
}

# Compare with derivative method
cat("\n\nTesting derivative M_p method for comparison...\n")
derivative_result <- metric_mp(r2_curve)
cat(sprintf("  Selected p*: %d\n", derivative_result$p_star))
cat(sprintf("  Method: %s\n", derivative_result$method))

# Visualize the fit
if (!is.null(sigmoid_result$fitted_curve)) {
  cat("\n\nCreating comparison plot...\n")
  
  p_vals <- r2_curve$p
  M_vals <- r2_curve$R2 / p_vals
  
  png("test_sigmoid_fit.png", width = 10, height = 6, units = "in", res = 300)
  par(mfrow = c(1, 2), mar = c(4, 4.5, 3, 2))
  
  # Panel 1: M_p curve with sigmoid fit
  plot(p_vals, M_vals, type = "p", pch = 19, col = "#2E86AB",
       xlab = "p", ylab = expression(M[p] == R^2/p),
       main = "Sigmoid Fit to M_p Curve",
       cex = 1.2, cex.lab = 1.2, cex.main = 1.3)
  lines(p_vals, sigmoid_result$fitted_curve, col = "#E63946", lwd = 2)
  abline(v = data$p_true, lty = 1, col = "green3", lwd = 2)
  abline(v = sigmoid_result$p_star, lty = 2, col = "#E63946", lwd = 2)
  abline(v = sigmoid_result$delta, lty = 3, col = "#E63946", lwd = 1.5)
  
  legend("topright", 
         legend = c("Observed M_p", "Sigmoid fit", 
                   sprintf("True p* = %d", data$p_true),
                   sprintf("Selected p* = %d", sigmoid_result$p_star),
                   sprintf("Delta (raw) = %.2f", sigmoid_result$delta)),
         col = c("#2E86AB", "#E63946", "green3", "#E63946", "#E63946"),
         lty = c(NA, 1, 1, 2, 3), pch = c(19, NA, NA, NA, NA),
         lwd = c(NA, 2, 2, 2, 1.5), cex = 0.9)
  
  # Panel 2: Residuals
  residuals <- M_vals - sigmoid_result$fitted_curve
  plot(p_vals, residuals, type = "h", col = "#A23B72", lwd = 2,
       xlab = "p", ylab = "Residuals",
       main = "Fit Residuals",
       cex.lab = 1.2, cex.main = 1.3)
  abline(h = 0, lty = 2, col = "gray50")
  points(p_vals, residuals, pch = 19, col = "#A23B72", cex = 1.2)
  
  dev.off()
  cat("  ✓ Plot saved to: test_sigmoid_fit.png\n")
}

cat("\n=== Test complete ===\n\n")
