#!/usr/bin/env Rscript
# Test sigmoid fit with manual M_p values

# Manual M_p values from your output
p_vals <- 1:20
M_vals <- c(0.55677778, 0.43679526, 0.33037693, 0.24779682, 0.19824204,
            0.16520493, 0.14160635, 0.12390712, 0.11014053, 0.09912741,
            0.09011652, 0.08260742, 0.07625345, 0.07080707, 0.06608682,
            0.06195657, 0.05831213, 0.05507259, 0.05217405, 0.04956536)

# Estimate inflection point from curvature change
delta1 <- diff(M_vals)
delta2 <- diff(delta1)

cat("=== First derivative (delta1) ===\n")
print(data.frame(p = p_vals[1:19], delta1 = delta1))

cat("\n=== Second derivative (delta2) ===\n")
print(data.frame(p = p_vals[2:19], delta2 = delta2))

cat("\n=== Inflection point analysis ===\n")
inflection_idx <- which.min(abs(delta2))
cat("Index where |delta2| is minimum:", inflection_idx, "\n")
cat("Corresponds to p =", p_vals[inflection_idx + 2], "\n")

# Now try sigmoid fit with improved starting values (from main function)
alpha_start <- min(M_vals)
beta_start <- max(M_vals) - min(M_vals)

# Find where decline is steepest
if (length(M_vals) >= 3) {
  delta1 <- diff(M_vals)
  steepest_idx <- which.min(delta1)  # Most negative
  delta_start <- p_vals[steepest_idx + 1]
  cat("Steepest decline at index:", steepest_idx, "-> p =", delta_start, "\n")
} else {
  delta_start <- median(p_vals)
}

# Adaptive gamma based on transition location
if (delta_start < 5) {
  gamma_start <- 2.0  # Sharp transition for early p
} else {
  gamma_start <- 1.0  # Gentler for later p
}

cat("\n=== Starting values ===\n")
cat("alpha_start:", alpha_start, "\n")
cat("beta_start:", beta_start, "\n")
cat("delta_start:", delta_start, "\n")
cat("gamma_start:", gamma_start, "\n")

# Try fit with nls (port algorithm for bounds)
fit <- try(nls(
  M_vals ~ alpha + beta / (1 + exp(gamma * (p_vals - delta))),
  start = list(alpha = alpha_start, beta = beta_start, gamma = gamma_start, delta = delta_start),
  algorithm = "port",
  lower = c(alpha = 0, beta = 0, gamma = 0.01, delta = -50),
  upper = c(alpha = max(M_vals), beta = 2 * beta_start, gamma = 50, delta = max(p_vals)),
  control = nls.control(maxiter = 200, warnOnly = TRUE)
), silent = FALSE)

if (class(fit)[1] != "try-error") {
  params <- coef(fit)
  cat("\n=== Fit results ===\n")
  print(params)
  cat("Inflection point (delta):", params["delta"], "\n")
  cat("Rounded p*:", round(params["delta"]), "\n")
  
  # Check fitted values and find where curvature is maximum IN OUR RANGE
  fitted_M <- params["alpha"] + params["beta"] / (1 + exp(params["gamma"] * (p_vals - params["delta"])))
  
  # Calculate second derivative of fitted curve
  # f''(p) = -beta * gamma^2 * exp(gamma*(p-delta)) * (1 - exp(gamma*(p-delta))) / (1 + exp(gamma*(p-delta)))^3
  exp_term <- exp(params["gamma"] * (p_vals - params["delta"]))
  second_deriv <- -params["beta"] * params["gamma"]^2 * exp_term * (1 - exp_term) / (1 + exp_term)^3
  
  cat("\n=== Curvature analysis (second derivative of fitted sigmoid) ===\n")
  print(data.frame(p = p_vals[1:10], second_deriv = second_deriv[1:10]))
  cat("\nPoint of maximum absolute curvature: p =", p_vals[which.max(abs(second_deriv))], "\n")
  
  cat("\n=== Fit quality ===\n")
  cat("RMSE:", sqrt(mean((M_vals - fitted_M)^2)), "\n")
  cat("R²:", 1 - sum((M_vals - fitted_M)^2) / sum((M_vals - mean(M_vals))^2), "\n")
  
} else {
  cat("\n=== Fit failed ===\n")
  print(fit)
}
