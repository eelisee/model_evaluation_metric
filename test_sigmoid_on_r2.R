#!/usr/bin/env Rscript
# Test fitting sigmoid to R² instead of M_p

source('run_experiment.R')
set.seed(1001)
scenarios <- define_scenarios()
data <- generate_data(scenarios[[1]])
r2_curve <- compute_r2_curve(data$X, data$y, n_cores=1)

# Current approach: fit to M_p
cat("=== Fitting to M_p (current approach) ===\n")
result_mp <- metric_sigmoid_mp(r2_curve)
cat("p_star:", result_mp$p_star, "\n")
cat("delta:", result_mp$delta, "\n\n")

# Alternative: fit to R²
cat("=== Fitting to R² (alternative) ===\n")
p_vals <- r2_curve$p
R2_vals <- r2_curve$R2

# Fit sigmoid to R²: f(p) = alpha + beta / (1 + exp(gamma * (p - delta)))
alpha_start <- max(R2_vals)
beta_start <- -(max(R2_vals) - min(R2_vals))  # negative for increasing curve
delta_start <- median(p_vals)
gamma_start <- 0.5

library(minpack.lm)
fit <- try(nlsLM(
  R2_vals ~ alpha + beta / (1 + exp(gamma * (p_vals - delta))),
  start = list(alpha = alpha_start, beta = beta_start, gamma = gamma_start, delta = delta_start),
  control = nls.lm.control(maxiter = 200)
), silent = TRUE)

if (class(fit)[1] != "try-error") {
  params <- coef(fit)
  cat("Sigmoid fit successful:\n")
  print(params)
  cat("Inflection point (delta):", params["delta"], "\n")
  cat("Rounded p*:", round(params["delta"]), "\n")
} else {
  cat("Sigmoid fit failed\n")
}
