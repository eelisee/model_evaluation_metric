#!/usr/bin/env Rscript
# Detailed analysis of sigmoid fit quality

source("R/01_data_generation.R")
source("R/02_metrics.R")

# Load A2 results
results <- read.csv('results/A2_Single_Predictor/detailed_results.csv')

# Average M_p values
p_vals <- 1:20
avg_mp <- numeric(20)
for (p in p_vals) {
  avg_mp[p] <- mean(results[results$p == p, 'M_p'])
}

cat("=== A2 SCENARIO: Sigmoid Fit Quality Analysis ===\n\n")

# Fit sigmoid
r2_avg <- avg_mp * p_vals
avg_r2_curve <- data.frame(
  p = p_vals,
  R2 = r2_avg,
  subset_Mp = NA
)

sigmoid_result <- metric_sigmoid_mp(avg_r2_curve)

cat("Fitted parameters:\n")
cat(sprintf("  α (lower asymptote): %.4f\n", sigmoid_result$params["alpha"]))
cat(sprintf("  β (range):           %.4f\n", sigmoid_result$params["beta"]))
cat(sprintf("  γ (steepness):       %.4f\n", sigmoid_result$params["gamma"]))
cat(sprintf("  δ (inflection):      %.4f\n", sigmoid_result$params["delta"]))
cat(sprintf("\nSelected p*: %d\n", sigmoid_result$p_star))

# Show actual vs fitted values
cat("\n=== Comparison: Actual vs Fitted ===\n")
cat(sprintf("%-4s | %-10s | %-10s | %-10s | %-10s\n", 
            "p", "M_p(data)", "M_p(fit)", "Error", "Sq.Error"))
cat(strrep("-", 60), "\n")

fitted <- sigmoid_result$fitted_curve
total_sq_error <- 0
for (i in 1:10) {
  error <- avg_mp[i] - fitted[i]
  sq_error <- error^2
  total_sq_error <- total_sq_error + sq_error
  cat(sprintf("%-4d | %-10.4f | %-10.4f | %+-10.4f | %-10.6f\n", 
              i, avg_mp[i], fitted[i], error, sq_error))
}

rmse <- sqrt(mean((avg_mp - fitted)^2))
cat(sprintf("\nRMSE (all p): %.6f\n", rmse))
cat(sprintf("Sum of squared errors (p=1-10): %.6f\n", total_sq_error))

# Show what the sigmoid formula predicts
cat("\n=== Understanding the Sigmoid Formula ===\n")
cat("M_p(δ) = α + β / (1 + exp(γ × (p - δ)))\n\n")

alpha <- sigmoid_result$params["alpha"]
beta <- sigmoid_result$params["beta"]
gamma <- sigmoid_result$params["gamma"]
delta <- sigmoid_result$params["delta"]

cat("At inflection point (p = δ = 0.77):\n")
p_delta <- delta
M_at_delta <- alpha + beta / (1 + exp(gamma * (p_delta - delta)))
cat(sprintf("  M_p(%.2f) = %.4f + %.4f / (1 + exp(0)) = %.4f + %.4f/2 = %.4f\n",
            delta, alpha, beta, alpha, beta, M_at_delta))
cat(sprintf("  This is the midpoint: (upper + lower)/2 = (%.4f + %.4f)/2 = %.4f\n",
            alpha + beta, alpha, (alpha + beta + alpha)/2))

cat("\nAt p=1 (actual M_p = 0.9488):\n")
M_fit_1 <- fitted[1]
cat(sprintf("  Fitted: %.4f\n", M_fit_1))
cat(sprintf("  Error: %.4f (underpredicts by %.1f%%)\n", 
            M_fit_1 - avg_mp[1], 100 * (M_fit_1 - avg_mp[1]) / avg_mp[1]))

cat("\nAt p=2 (actual M_p = 0.4746, huge drop!):\n")
M_fit_2 <- fitted[2]
cat(sprintf("  Fitted: %.4f\n", M_fit_2))
cat(sprintf("  Error: %.4f (overpredicts by %.1f%%)\n", 
            M_fit_2 - avg_mp[2], 100 * (M_fit_2 - avg_mp[2]) / avg_mp[2]))

cat("\n=== Why the fit is poor ===\n")
cat("The sigmoid assumes a SMOOTH S-curve transition.\n")
cat("But A2 has a SHARP DROP at p=1→2 (0.474 decline).\n")
cat("The optimizer tries to balance:\n")
cat("  - Fitting the high value at p=1\n")
cat("  - Fitting the low values at p=2+\n")
cat("  - Minimizing overall squared error\n")
cat("\nResult: It compromises and fits neither region well!\n")
cat("The sigmoid is too smooth for this data.\n")
