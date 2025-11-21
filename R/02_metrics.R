# Metrics Module
# ===============
# Implements M_p (inflection point), AIC, BIC, and variants
# Uses parallel processing for exhaustive search

# Load parallel library
library(parallel)

#' Compute R² for All Model Sizes
#'
#' Uses exhaustive search: for each p = 1, ..., p_max,
#' find the best subset of size p that maximizes R².
#' Parallelized across subsets for faster computation.
#'
#' @param X Matrix. n x p design matrix
#' @param y Vector. Response
#' @param n_cores Integer. Number of cores to use (default: detectCores() - 1)
#' @return Data frame with columns: p, R2, subset, RSS, AIC, BIC
#' @export
compute_r2_curve <- function(X, y, n_cores = NULL) {
  
  n <- nrow(X)
  p_max <- ncol(X)
  
  # Determine number of cores
  if (is.null(n_cores)) {
    n_cores <- max(1, detectCores() - 1)
  }
  
  cat(sprintf("    Computing R² curve for p_max=%d (using %d cores)...\n", p_max, n_cores))
  
  # Total sum of squares
  TSS <- sum((y - mean(y))^2)
  
  # Storage
  results <- list()
  
  for (p in 1:p_max) {
    
    # Enumerate all subsets of size p
    subsets <- combn(1:p_max, p, simplify = FALSE)
    n_subsets <- length(subsets)
    cat(sprintf("      p=%d: evaluating %d subsets", p, n_subsets))
    
    # Parallel evaluation of all subsets
    subset_results <- mclapply(subsets, function(S) {
      
      # Fit model with subset S
      X_S <- X[, S, drop = FALSE]
      
      # OLS: β̂ = (X'X)^{-1} X'y
      fit <- lm.fit(X_S, y)
      y_hat <- fitted(fit)
      residuals <- y - y_hat
      RSS <- sum(residuals^2)
      
      # R²
      R2 <- 1 - RSS / TSS
      
      # AIC = n·log(RSS/n) + 2p
      AIC_val <- n * log(RSS / n) + 2 * p
      
      # BIC = n·log(RSS/n) + p·log(n)
      BIC_val <- n * log(RSS / n) + p * log(n)
      
      list(R2 = R2, RSS = RSS, AIC = AIC_val, BIC = BIC_val, subset = S)
      
    }, mc.cores = n_cores)
    
    # Find best subset
    R2_vals <- sapply(subset_results, function(x) x$R2)
    best_idx <- which.max(R2_vals)
    best <- subset_results[[best_idx]]
    
    results[[p]] <- data.frame(
      p = p,
      R2 = best$R2,
      RSS = best$RSS,
      AIC = best$AIC,
      BIC = best$BIC,
      subset = I(list(best$subset))
    )
    
    cat(" ✓\n")
  }
  
  # Combine
  df <- do.call(rbind, results)
  return(df)
}


#' M_p Metric: Inflection Point Method
#'
#' Finds p* via discrete second derivative of R² curve.
#'
#' Definition:
#'   M(p) = R²(p) / p
#'   Δ₁(p) = M(p+1) - M(p)         # First difference
#'   Δ₂(p) = Δ₁(p+1) - Δ₁(p)       # Second difference
#'   p* = argmin_p Δ₂(p)           # Minimum of second derivative
#'
#' Equivalent formulation:
#'   p* = argmax_p [Δ₁(p-1) - Δ₁(p)]
#'
#' @param r2_curve Data frame from compute_r2_curve()
#' @return List with p_star, subset, method, diagnostics
#' @export
metric_mp <- function(r2_curve) {
  
  p_vals <- r2_curve$p
  R2_vals <- r2_curve$R2
  
  if (length(p_vals) < 3) {
    # Not enough points
    return(list(
      metric = "M_p",
      p_star = 1,
      subset = r2_curve$subset[[1]],
      method = "insufficient_points"
    ))
  }
  
  # Compute M_p curve: M(p) = R²(p) / p
  M_p <- R2_vals / p_vals
  
  # First differences: Δ₁(p) = M(p+1) - M(p)
  delta1 <- diff(M_p)
  
  # Second differences: Δ₂(p) = Δ₁(p+1) - Δ₁(p)
  delta2 <- diff(delta1)
  
  # Find minimum Δ₂ (inflection point)
  # This is where M_p changes from concave to convex
  min_idx <- which.min(delta2)
  
  # The inflection point corresponds to p at position min_idx + 1
  # (since delta2 is doubly differenced)
  p_star <- p_vals[min_idx + 1]
  
  subset <- r2_curve$subset[[min_idx + 1]]
  
  return(list(
    metric = "M_p",
    p_star = p_star,
    subset = subset,
    M_p = M_p,
    delta1 = delta1,
    delta2 = delta2,
    inflection_index = min_idx + 1,
    method = "inflection_point_of_Mp"
  ))
}


#' AIC Selection
#'
#' Select p* = argmin AIC
#'
#' @param r2_curve Data frame
#' @return List with p_star, subset
#' @export
metric_aic <- function(r2_curve) {
  
  min_idx <- which.min(r2_curve$AIC)
  p_star <- r2_curve$p[min_idx]
  subset <- r2_curve$subset[[min_idx]]
  
  return(list(
    metric = "AIC",
    p_star = p_star,
    subset = subset,
    method = "minimize_AIC"
  ))
}


#' BIC Selection
#'
#' Select p* = argmin BIC
#'
#' @param r2_curve Data frame
#' @return List with p_star, subset
#' @export
metric_bic <- function(r2_curve) {
  
  min_idx <- which.min(r2_curve$BIC)
  p_star <- r2_curve$p[min_idx]
  subset <- r2_curve$subset[[min_idx]]
  
  return(list(
    metric = "BIC",
    p_star = p_star,
    subset = subset,
    method = "minimize_BIC"
  ))
}


#' Apply All Metrics
#'
#' @param r2_curve Data frame
#' @return List of metric results
#' @export
apply_all_metrics <- function(r2_curve) {
  
  list(
    M_p = metric_mp(r2_curve),
    AIC = metric_aic(r2_curve),
    BIC = metric_bic(r2_curve)
  )
}
