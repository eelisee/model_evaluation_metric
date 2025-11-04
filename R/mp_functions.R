# Core Functions for M_p Model Evaluation Metric - Synthetic Data Generation
# ===========================================================================

#' Creates a dataset with normally distributed predictors and a linear response
#' with specified true coefficients and noise level.
#'
#' @param n Integer. Number of observations
#' @param p_max Integer. Maximum number of potential predictors
#' @param true_beta Numeric vector. True coefficient values (length p_max)
#' @param sigma Numeric. Standard deviation of the error term
#' @return List containing X (design matrix) and y (response vector)
#' @export
generate_data <- function(n = 100, 
                         p_max = 10, 
                         true_beta = c(1, 0.8, 0.5, rep(0, 7)),
                         sigma = 0.2) {
  
  # Validate inputs
  if (length(true_beta) != p_max) {
    stop("Length of true_beta must equal p_max")
  }
  
  # Generate design matrix with standard normal predictors
  X <- matrix(rnorm(n * p_max), nrow = n, ncol = p_max)
  colnames(X) <- paste0("X", 1:p_max)
  
  # Generate response variable
  epsilon <- rnorm(n, mean = 0, sd = sigma)
  y <- X %*% true_beta + epsilon
  y <- as.vector(y)
  
  # Return as list
  list(
    X = X,
    y = y,
    true_beta = true_beta,
    n = n,
    p_max = p_max,
    sigma = sigma
  )
}


#' Generate All Possible Model Subsets
#'
#' Creates all 2^p_max possible subsets of predictors (excluding empty model)
#'
#' @param p_max Integer. Maximum number of predictors
#' @return List of integer vectors, each representing a subset of predictor indices
#' @export
generate_all_subsets <- function(p_max) {
  
  # Total number of models (excluding empty model)
  n_models <- 2^p_max - 1
  
  # Initialize list to store subsets
  subsets <- vector("list", n_models)
  
  # Generate all combinations
  idx <- 1
  for (p in 1:p_max) {
    # Generate all combinations of size p
    combs <- combn(p_max, p, simplify = FALSE)
    
    # Add to subsets list
    for (comb in combs) {
      subsets[[idx]] <- comb
      idx <- idx + 1
    }
  }
  
  return(subsets)
}


#' Fit Linear Model for Given Subset
#'
#' Fits OLS regression using specified subset of predictors
#'
#' @param X Matrix. Full design matrix
#' @param y Vector. Response variable
#' @param subset Integer vector. Indices of predictors to include
#' @return List containing fitted values, residuals, and model fit statistics
#' @export
fit_model_subset <- function(X, y, subset) {
  
  # Extract relevant columns
  X_sub <- X[, subset, drop = FALSE]
  
  # Fit model
  model <- lm(y ~ X_sub)
  
  # Extract fitted values and residuals
  y_hat <- fitted(model)
  residuals <- residuals(model)
  
  # Calculate statistics
  RSS <- sum(residuals^2)
  TSS <- sum((y - mean(y))^2)
  R2 <- 1 - RSS / TSS
  
  # Return results
  list(
    subset = subset,
    p = length(subset),
    y_hat = y_hat,
    residuals = residuals,
    RSS = RSS,
    TSS = TSS,
    R2 = R2,
    coefficients = coef(model)
  )
}


#' Calculate M_p Metric
#'
#' Computes the M_p metric as R² divided by the number of parameters
#'
#' @param R2 Numeric. Coefficient of determination
#' @param p Integer. Number of active parameters
#' @return Numeric. M_p value
#' @export
compute_mp <- function(R2, p) {
  if (p == 0) {
    return(NA)
  }
  return(R2 / p)
}


#' Evaluate All Models
#'
#' Fits all possible regression models and computes M_p for each
#'
#' @param X Matrix. Design matrix
#' @param y Vector. Response variable
#' @param verbose Logical. Print progress messages
#' @return Data frame with columns: model_id, subset, p, R2, Mp, RSS
#' @export
evaluate_all_models <- function(X, y, verbose = TRUE) {
  
  p_max <- ncol(X)
  n_models <- 2^p_max - 1
  
  if (verbose) {
    cat(sprintf("Evaluating %d models with p_max = %d\n", n_models, p_max))
  }
  
  # Generate all subsets
  subsets <- generate_all_subsets(p_max)
  
  # Initialize results storage
  results <- data.frame(
    model_id = integer(n_models),
    p = integer(n_models),
    R2 = numeric(n_models),
    Mp = numeric(n_models),
    RSS = numeric(n_models),
    subset_str = character(n_models),
    stringsAsFactors = FALSE
  )
  
  # Evaluate each model
  if (verbose && n_models > 100) {
    pb <- txtProgressBar(min = 0, max = n_models, style = 3)
  }
  
  for (i in 1:n_models) {
    subset <- subsets[[i]]
    
    # Fit model
    fit <- fit_model_subset(X, y, subset)
    
    # Store results
    results$model_id[i] <- i
    results$p[i] <- fit$p
    results$R2[i] <- fit$R2
    results$Mp[i] <- compute_mp(fit$R2, fit$p)
    results$RSS[i] <- fit$RSS
    results$subset_str[i] <- paste(subset, collapse = ",")
    
    if (verbose && n_models > 100) {
      setTxtProgressBar(pb, i)
    }
  }
  
  if (verbose && n_models > 100) {
    close(pb)
  }
  
  if (verbose) {
    cat("\nEvaluation complete!\n")
  }
  
  return(results)
}


#' Find Best Model for Each Cardinality
#'
#' Groups models by number of predictors and identifies best M_p for each group
#'
#' @param results Data frame. Output from evaluate_all_models()
#' @return Data frame with best model for each p value
#' @export
find_best_models_by_p <- function(results) {
  
  # Find best (maximum) Mp for each p
  best_models <- do.call(rbind, lapply(split(results, results$p), function(df) {
    best_idx <- which.max(df$Mp)
    df[best_idx, ]
  }))
  
  # Order by p
  best_models <- best_models[order(best_models$p), ]
  rownames(best_models) <- NULL
  
  return(best_models)
}


#' Find Optimal Model Complexity
#'
#' Identifies the optimal p* based on M_p curve analysis
#' Uses discrete derivative to find point of maximum decrease
#'
#' @param results Data frame. Output from evaluate_all_models()
#' @return List containing p_star, best_models, and diagnostics
#' @export
find_optimal_model <- function(results) {
  
  # Get best models for each p
  best_models <- find_best_models_by_p(results)
  
  # Calculate discrete derivatives (differences)
  p_vals <- best_models$p
  Mp_vals <- best_models$Mp
  
  if (length(p_vals) < 2) {
    warning("Not enough models to determine optimal p")
    return(list(
      p_star = 1,
      best_models = best_models,
      optimal_model = best_models[1, ]
    ))
  }
  
  # Calculate first differences (rate of change)
  dMp <- diff(Mp_vals)
  
  # Calculate second differences (curvature)
  if (length(dMp) > 1) {
    d2Mp <- diff(dMp)
    
    # Find point of maximum curvature (largest negative second derivative)
    # This represents the inflection point
    inflection_idx <- which.min(d2Mp)
    
    # The optimal p is at or just after the inflection point
    # We choose the point before the largest drop
    p_star <- p_vals[inflection_idx + 1]
  } else {
    # If only two points, choose the first
    p_star <- p_vals[1]
  }
  
  # Alternative: find where Mp drops below a threshold
  # Or where the decrease becomes less than a certain percentage
  
  # Get optimal model
  optimal_model <- best_models[best_models$p == p_star, ]
  
  return(list(
    p_star = p_star,
    best_models = best_models,
    optimal_model = optimal_model,
    first_diff = dMp,
    second_diff = if (length(dMp) > 1) d2Mp else NULL
  ))
}


#' Calculate AIC and BIC for Comparison
#'
#' Computes AIC and BIC for all models to compare with M_p
#'
#' @param X Matrix. Design matrix
#' @param y Vector. Response variable
#' @param results Data frame. Output from evaluate_all_models()
#' @return Data frame with AIC and BIC added
#' @export
calculate_information_criteria <- function(X, y, results) {
  
  n <- length(y)
  
  # Add AIC and BIC columns
  results$AIC <- NA
  results$BIC <- NA
  
  for (i in 1:nrow(results)) {
    subset <- as.integer(strsplit(results$subset_str[i], ",")[[1]])
    p <- results$p[i]
    RSS <- results$RSS[i]
    
    # Calculate log-likelihood (assuming normal errors)
    # log L = -n/2 * log(2*pi) - n/2 * log(RSS/n) - n/2
    log_lik <- -n/2 * log(2*pi) - n/2 * log(RSS/n) - n/2
    
    # AIC = -2*log L + 2*k (where k = p + 1 for intercept + sigma^2)
    results$AIC[i] <- -2 * log_lik + 2 * (p + 2)
    
    # BIC = -2*log L + k*log(n)
    results$BIC[i] <- -2 * log_lik + (p + 2) * log(n)
  }
  
  return(results)
}


#' Print Summary of Results
#'
#' Displays key information about the optimal model
#'
#' @param optimal_result List. Output from find_optimal_model()
#' @param true_p Integer. True number of non-zero coefficients (if known)
#' @export
print_summary <- function(optimal_result, true_p = NULL) {
  
  cat("\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("M_p Model Selection Summary\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  
  cat(sprintf("Optimal model complexity: p* = %d\n", optimal_result$p_star))
  cat(sprintf("Best R² at p*: %.4f\n", optimal_result$optimal_model$R2))
  cat(sprintf("Best M_p at p*: %.4f\n", optimal_result$optimal_model$Mp))
  cat(sprintf("Selected predictors: %s\n", optimal_result$optimal_model$subset_str))
  
  if (!is.null(true_p)) {
    cat(sprintf("\nTrue model complexity: p_true = %d\n", true_p))
    cat(sprintf("Difference: p* - p_true = %d\n", optimal_result$p_star - true_p))
  }
  
  cat("\n")
  cat("Best models by cardinality:\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  print(optimal_result$best_models[, c("p", "R2", "Mp", "subset_str")])
  
  cat("\n")
}
