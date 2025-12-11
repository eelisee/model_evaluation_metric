# Debug script to check sigmoid fitting stability
source("R/01_data_generation.R")
source("R/02_metrics.R")

# Define scenario
scenario <- list(
  name = "A1_Baseline_Uncorrelated",
  description = "Uncorrelated predictors, sparse β, Gaussian noise",
  n = 500,
  p = 20,
  support_spec = 3,
  signal = "strong",
  structure = "identity",
  rho = 0.0,
  noise_sd = 0.2
)

set.seed(123)

# Run 5 iterations and examine sigmoid fits
for (iter in 1:5) {
  data <- generate_data(scenario)
  r2_curve <- compute_r2_curve(data$X, data$y, data$support, p_max = 20, n_cores = 1)
  result <- metric_sigmoid_mp(r2_curve)
  
  cat(sprintf("\n=== Iteration %d ===\n", iter))
  cat(sprintf("Selected p* = %d\n", result$p_star))
  
  if (!is.null(result$params)) {
    cat(sprintf("Sigmoid params: alpha=%.4f, beta=%.4f, gamma=%.4f, delta=%.4f\n",
                result$params["alpha"], result$params["beta"], 
                result$params["gamma"], result$params["delta"]))
    
    # Calculate first derivatives at p=1,2,3,4,5
    alpha <- result$params["alpha"]
    beta <- result$params["beta"]
    gamma <- result$params["gamma"]
    delta <- result$params["delta"]
    
    cat("\n  p   M_p      derivative\n")
    for (p in 1:5) {
      exp_term <- exp(gamma * (p - delta))
      deriv <- -beta * gamma * exp_term / (1 + exp_term)^2
      cat(sprintf("  %d   %.4f   %.6f\n", p, r2_curve$M_p[p], deriv))
    }
  }
}
