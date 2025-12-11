# ============================================================================
# TOY EXAMPLE: Manual Beta Configuration for Development/Testing
# ============================================================================
# 
# This script allows manual configuration of beta values to test the sigmoid
# method without random iterations. Useful for understanding behavior.
#
# Usage:
#   Rscript toy_example.R
#   or source("toy_example.R") in R console

# Load required functions
source("R/01_data_generation.R")
source("R/02_metrics.R")
source("R/04_visualization.R")

library(MASS)  # For mvrnorm

# ============================================================================
# CONFIGURATION
# ============================================================================

# Fixed parameters
n <- 500              # Sample size
p <- 10               # Number of predictors (reduced from 20)
sigma_eps <- 0.2      # Noise standard deviation
Sigma <- diag(p)      # Identity covariance matrix

# Output directory
output_dir <- "results/toy_examples"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# SCENARIO DEFINITIONS
# ============================================================================

scenarios <- list(
  
  # Scenario 1: Equal strong signals
  S1 = list(
    name = "S1_Equal_Strong",
    description = "3 equal strong coefficients (β=3.0)",
    support = c(1, 2, 3),
    beta_values = c(3.0, 3.0, 3.0),
    p_true = 3
  ),
  
  # Scenario 2: Unequal strong signals
  S2 = list(
    name = "S2_Unequal_Strong",
    description = "3 unequal strong coefficients",
    support = c(1, 2, 3),
    beta_values = c(3.0, 2.0, 4.0),
    p_true = 3
  ),
  
  # Scenario 3: 5 equal signals
  S3 = list(
    name = "S3_Five_Equal",
    description = "5 equal coefficients (β=3.0)",
    support = c(1, 2, 3, 4, 5),
    beta_values = c(3.0, 3.0, 3.0, 3.0, 3.0),
    p_true = 5
  ),
  
  # Scenario 4: 5 very unequal signals
  S4 = list(
    name = "S4_Five_Unequal",
    description = "5 very unequal coefficients",
    support = c(1, 2, 3, 4, 5),
    beta_values = c(3.0, 2.0, 4.0, 5.0, 7.0),
    p_true = 5
  ),

    # Scenario 5: 7 very unequal signals
  S5 = list(
    name = "S5_Seven_Unequal",
    description = "7 very unequal coefficients",
    support = c(1, 2, 3, 4, 5, 6, 7),
    beta_values = c(3.0, 2.0, 4.0, 5.0, 7.0, 1.5, 6.0),
    p_true = 7
  ),

    # Scenario 4: 10 very unequal signals
  S6 = list(
    name = "S6_Ten_Unequal",
    description = "10 very unequal coefficients",
    support = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    beta_values = c(3.0, 2.0, 4.0, 5.0, 7.0, 1.5, 6.0, 2.5, 3.5, 4.5),
    p_true = 10
  ),

    # Scenario 4: 10 very unequal signals
  S7 = list(
    name = "S7_Ten_equal",
    description = "10 very equal coefficients",
    support = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    beta_values = c(3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0),
    p_true = 10
  ),

      # Scenario 4: 10 very unequal signals
  S8 = list(
    name = "S8_Ten_Unequal_weird",
    description = "10 very weirdly unequal coefficients",
    support = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    beta_values = c(3.0, 3.0, 3.0, 5.0, 7.0, 1.5, 1.0, 1.5, 4.5, 4.5),
    p_true = 10
  )

)


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Generate Data with Fixed Beta
#'
#' @param n Integer. Sample size
#' @param p Integer. Number of predictors
#' @param beta Vector. Fixed beta coefficients (length p)
#' @param Sigma Matrix. Covariance matrix
#' @param sigma_eps Numeric. Noise std dev
#' @return List with X, y, beta, support
generate_toy_data <- function(n, p, beta, Sigma, sigma_eps) {
  
  # Generate design matrix
  X <- mvrnorm(n = n, mu = rep(0, p), Sigma = Sigma)
  
  # Generate noise
  epsilon <- rnorm(n, 0, sigma_eps)
  
  # Generate response
  y <- X %*% beta + epsilon
  
  # Identify support
  support <- which(beta != 0)
  
  return(list(
    X = X,
    y = as.vector(y),
    beta = beta,
    support = support,
    p_true = length(support)
  ))
}


#' Run Single Toy Scenario
#'
#' @param scenario List. Scenario configuration
#' @param n Integer. Sample size
#' @param p Integer. Total number of predictors
#' @param Sigma Matrix. Covariance matrix
#' @param sigma_eps Numeric. Noise std dev
#' @return List with results
run_toy_scenario <- function(scenario, n, p, Sigma, sigma_eps) {
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("SCENARIO:", scenario$name, "\n")
  cat(scenario$description, "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  # Construct beta vector
  beta <- rep(0, p)
  beta[scenario$support] <- scenario$beta_values
  
  cat("Beta configuration:\n")
  for (i in seq_along(beta)) {
    if (beta[i] != 0) {
      cat(sprintf("  β_%d = %.2f  (support)\n", i, beta[i]))
    }
  }
  cat("\n")
  
  # Generate data
  data <- generate_toy_data(n, p, beta, Sigma, sigma_eps)
  
  # Compute R² curve
  cat("Computing R² curve...\n")
  r2_curve <- compute_r2_curve(data$X, data$y, n_cores = 1)
  
  # Apply sigmoid method with 4-parameter model (includes delta for flexibility)
  cat("Applying sigmoid method (4-parameter)...\n")
  result_sigmoid <- metric_sigmoid_mp(r2_curve, use_3param = FALSE)
  
  # Apply other metrics for comparison
  result_aic <- metric_aic(r2_curve)
  result_bic <- metric_bic(r2_curve)
  
  # Print results
  cat("\n")
  cat("RESULTS:\n")
  cat(sprintf("  True p*:       %d\n", scenario$p_true))
  cat(sprintf("  Sigmoid p*:    %d  (error: %+d)\n", 
              result_sigmoid$p_star, result_sigmoid$p_star - scenario$p_true))
  cat(sprintf("  AIC p*:        %d  (error: %+d)\n", 
              result_aic$p_star, result_aic$p_star - scenario$p_true))
  cat(sprintf("  BIC p*:        %d  (error: %+d)\n", 
              result_bic$p_star, result_bic$p_star - scenario$p_true))
  
  if (!is.null(result_sigmoid$params)) {
    cat("\n")
    cat("Sigmoid parameters:\n")
    cat(sprintf("  alpha (lower asymptote): %.4f\n", result_sigmoid$params["alpha"]))
    cat(sprintf("  beta (scale):            %.4f\n", result_sigmoid$params["beta"]))
    cat(sprintf("  gamma (steepness):       %.4f\n", result_sigmoid$params["gamma"]))
    if ("delta" %in% names(result_sigmoid$params)) {
      cat(sprintf("  delta (inflection):      %.4f\n", result_sigmoid$params["delta"]))
    }
  }
  
  # Create output directory
  scenario_dir <- file.path(output_dir, scenario$name)
  dir.create(scenario_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Save R² and M_p values as CSV
  cat("\nSaving R² and M_p data...\n")
  r2_mp_data <- data.frame(
    p = r2_curve$p,
    R2 = r2_curve$R2,
    M_p = r2_curve$R2 / r2_curve$p,
    AIC = r2_curve$AIC,
    BIC = r2_curve$BIC
  )
  csv_file <- file.path(scenario_dir, "r2_mp_curve.csv")
  write.csv(r2_mp_data, csv_file, row.names = FALSE)
  cat(sprintf("✓ Data saved to: %s\n", csv_file))
  
  # Create plots
  cat("\nCreating plots...\n")
  
  # Plot 1: M_p curve with sigmoid fit
  plot_toy_sigmoid(r2_curve, result_sigmoid, scenario$p_true, 
                   file.path(scenario_dir, "01_sigmoid_fit.png"))
  
  # Plot 2: R² curve
  plot_toy_r2(r2_curve, scenario$p_true, result_sigmoid$p_star, 
              file.path(scenario_dir, "02_r2_curve.png"))
  
  # Plot 3: Criterion comparison
  plot_toy_criteria(r2_curve, result_sigmoid$p_star, result_aic$p_star, 
                    result_bic$p_star, scenario$p_true,
                    file.path(scenario_dir, "03_criteria_comparison.png"))
  
  cat(sprintf("✓ Plots saved to: %s\n", scenario_dir))
  
  return(list(
    scenario = scenario,
    data = data,
    r2_curve = r2_curve,
    sigmoid = result_sigmoid,
    aic = result_aic,
    bic = result_bic
  ))
}


#' Plot Sigmoid Fit for Toy Example
plot_toy_sigmoid <- function(r2_curve, result_sigmoid, p_true, filename) {
  
  p_vals <- r2_curve$p
  M_p_vals <- r2_curve$R2 / p_vals
  fitted_curve <- result_sigmoid$fitted_curve
  p_star <- result_sigmoid$p_star
  
  png(filename, width = 10, height = 6, units = "in", res = 300)
  par(mar = c(4, 4.5, 3, 2))
  
  ylim <- if (!is.null(fitted_curve)) {
    range(c(M_p_vals, fitted_curve), na.rm = TRUE)
  } else {
    range(M_p_vals, na.rm = TRUE)
  }
  
  plot(p_vals, M_p_vals, type = "p", pch = 19, col = "#2E86AB", cex = 1.5,
       xlab = "Number of Predictors (p)", 
       ylab = expression(M[p] == R^2 / p),
       main = "Sigmoid Fit to M_p Curve (Toy Example)",
       xaxt = "n", cex.lab = 1.2, cex.main = 1.3, ylim = ylim)
  axis(1, at = p_vals)
  grid(col = "gray90", lty = 3)
  
  if (!is.null(fitted_curve) && !all(is.na(fitted_curve))) {
    lines(p_vals, fitted_curve, col = "#9B59B6", lwd = 3, lty = 1)
  }
  
  abline(v = p_true, lty = 1, col = "green3", lwd = 2.5)
  abline(v = p_star, lty = 2, col = "#E63946", lwd = 2.5)
  
  if (!is.null(result_sigmoid$params)) {
    params <- result_sigmoid$params
    model_type <- if (!is.null(result_sigmoid$model_type)) result_sigmoid$model_type else "sigmoid_4param"
    
    # Format parameters based on model type (use ASCII names to avoid encoding issues)
    if (model_type == "exponential") {
      param_text <- sprintf(
        "Exponential Model\nalpha=%.3f\nbeta=%.3f\ngamma=%.2f\np*=%d (true=%d)",
        params["alpha"], params["beta"], params["gamma"],
        p_star, p_true
      )
    } else if (model_type == "sigmoid_3param") {
      param_text <- sprintf(
        "Sigmoid (3p)\nalpha=%.3f\nbeta=%.3f\ngamma=%.2f\np*=%d (true=%d)",
        params["alpha"], params["beta"], params["gamma"],
        p_star, p_true
      )
    } else if ("delta" %in% names(params)) {
      # Full 4-parameter sigmoid
      param_text <- sprintf(
        "Sigmoid (4p)\nalpha=%.3f\nbeta=%.3f\ngamma=%.2f\ndelta=%.2f\np*=%d (true=%d)",
        params["alpha"], params["beta"], 
        params["gamma"], params["delta"],
        p_star, p_true
      )
    } else {
      # Fallback for unknown model
      param_text <- sprintf(
        "Model\nalpha=%.3f\nbeta=%.3f\ngamma=%.2f\np*=%d (true=%d)",
        params["alpha"], params["beta"], params["gamma"],
        p_star, p_true
      )
    }
  }
  
  legend("topright", 
         legend = c("M_p data", "Sigmoid fit", "True p*", "Selected p*"),
         col = c("#2E86AB", "#9B59B6", "green3", "#E63946"),
         lty = c(NA, 1, 1, 2), pch = c(19, NA, NA, NA),
         lwd = c(NA, 3, 2.5, 2.5), cex = 1.0, bg = "white")
  
  # Add parameters below legend using plot coordinates
  if (!is.null(result_sigmoid$params)) {
    usr <- par("usr")  # Get plot coordinates: c(x1, x2, y1, y2)
    x_pos <- usr[2] - (usr[2] - usr[1]) * 0.01  # 1% from right edge
    y_pos <- usr[4] - (usr[4] - usr[3]) * 0.35  # 35% down from top
    text(x = x_pos, y = y_pos, 
         labels = param_text, adj = 1, cex = 0.85, 
         col = "black", family = "mono")
  }
  
  dev.off()
}


#' Plot R² Curve
plot_toy_r2 <- function(r2_curve, p_true, p_selected, filename) {
  
  png(filename, width = 10, height = 6, units = "in", res = 300)
  par(mar = c(4, 4.5, 3, 2))
  
  plot(r2_curve$p, r2_curve$R2, type = "b", pch = 19, col = "#2E86AB",
       xlab = "Number of Predictors (p)", ylab = expression(R^2),
       main = "R² Curve", lwd = 2, cex = 1.5,
       xaxt = "n", cex.lab = 1.2, cex.main = 1.3)
  axis(1, at = r2_curve$p)
  grid(col = "gray90", lty = 3)
  
  abline(v = p_true, lty = 1, col = "green3", lwd = 2.5)
  abline(v = p_selected, lty = 2, col = "#E63946", lwd = 2.5)
  
  legend("bottomright",
         legend = c("R² curve", "True p*", "Selected p*"),
         col = c("#2E86AB", "green3", "#E63946"),
         lty = c(1, 1, 2), pch = c(19, NA, NA),
         lwd = c(2, 2.5, 2.5), cex = 1.0, bg = "white")
  
  dev.off()
}


#' Plot Criteria Comparison
plot_toy_criteria <- function(r2_curve, p_sigmoid, p_aic, p_bic, p_true, filename) {
  
  p_vals <- r2_curve$p
  R2_vals <- r2_curve$R2
  M_p_vals <- R2_vals / p_vals
  AIC_vals <- r2_curve$AIC
  BIC_vals <- r2_curve$BIC
  
  # Normalize for comparison
  normalize <- function(x) (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
  Mp_norm <- normalize(M_p_vals)
  AIC_norm <- 1 - normalize(AIC_vals)  # Invert (lower is better)
  BIC_norm <- 1 - normalize(BIC_vals)  # Invert (lower is better)
  
  png(filename, width = 10, height = 6, units = "in", res = 300)
  par(mar = c(4, 4.5, 3, 2))
  
  plot(p_vals, Mp_norm, type = "b", pch = 19, col = "#9B59B6",
       xlab = "Number of Predictors (p)", 
       ylab = "Normalized Score (higher is better)",
       main = "Model Selection Criteria Comparison",
       lwd = 2, cex = 1.2, xaxt = "n", cex.lab = 1.2, cex.main = 1.3,
       ylim = c(0, 1))
  axis(1, at = p_vals)
  grid(col = "gray90", lty = 3)
  
  lines(p_vals, AIC_norm, type = "b", pch = 17, col = "#2E86AB", lwd = 2, cex = 1.2)
  lines(p_vals, BIC_norm, type = "b", pch = 15, col = "#F18F01", lwd = 2, cex = 1.2)
  
  abline(v = p_true, lty = 1, col = "green3", lwd = 2.5)
  
  legend("right",
         legend = c("M_p (Sigmoid)", "AIC", "BIC", "True p*"),
         col = c("#9B59B6", "#2E86AB", "#F18F01", "green3"),
         lty = c(1, 1, 1, 1), pch = c(19, 17, 15, NA),
         lwd = c(2, 2, 2, 2.5), cex = 1.0, bg = "white")
  
  text(x = p_sigmoid, y = 0.95, labels = sprintf("p*=%d", p_sigmoid), 
       col = "#9B59B6", cex = 0.9, pos = 3)
  text(x = p_aic, y = 0.85, labels = sprintf("p*=%d", p_aic), 
       col = "#2E86AB", cex = 0.9, pos = 3)
  text(x = p_bic, y = 0.75, labels = sprintf("p*=%d", p_bic), 
       col = "#F18F01", cex = 0.9, pos = 3)
  
  dev.off()
}


# ============================================================================
# MAIN EXECUTION
# ============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("TOY EXAMPLE: Manual Beta Configuration\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat(sprintf("\nConfiguration:\n"))
cat(sprintf("  n = %d\n", n))
cat(sprintf("  p = %d\n", p))
cat(sprintf("  sigma_eps = %.2f\n", sigma_eps))
cat(sprintf("  Sigma = Identity matrix\n"))
cat(sprintf("\nNumber of scenarios: %d\n", length(scenarios)))

# Run all scenarios
results <- list()

for (i in seq_along(scenarios)) {
  results[[i]] <- run_toy_scenario(
    scenario = scenarios[[i]],
    n = n,
    p = p,
    Sigma = Sigma,
    sigma_eps = sigma_eps
  )
}

# Summary table
cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SUMMARY TABLE\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat(sprintf("%-20s %8s %8s %8s %8s\n", "Scenario", "True p*", "Sigmoid", "AIC", "BIC"))
cat(paste(rep("-", 70), collapse = ""), "\n")

for (i in seq_along(results)) {
  r <- results[[i]]
  cat(sprintf("%-20s %8d %8d %8d %8d\n",
              r$scenario$name,
              r$scenario$p_true,
              r$sigmoid$p_star,
              r$aic$p_star,
              r$bic$p_star))
}

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("✓ All toy examples completed!\n")
cat(sprintf("✓ Results saved to: %s\n", output_dir))
cat(paste(rep("=", 70), collapse = ""), "\n\n")
