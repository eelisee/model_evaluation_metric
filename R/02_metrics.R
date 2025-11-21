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
    # Reduced output: only show every 5th p
    if (p %% 5 == 1 || p == p_max) {
      cat(sprintf("      p=%d: %d subsets", p, n_subsets))
    }
    
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
    
    # Find best subsets by different criteria
    R2_vals <- sapply(subset_results, function(x) x$R2)
    AIC_vals <- sapply(subset_results, function(x) x$AIC)
    BIC_vals <- sapply(subset_results, function(x) x$BIC)
    
    # Compute M_p values for all subsets at this p
    M_p_vals <- R2_vals / p
    
    best_R2_idx <- which.max(R2_vals)
    best_AIC_idx <- which.min(AIC_vals)
    best_BIC_idx <- which.min(BIC_vals)
    best_Mp_idx <- which.max(M_p_vals)  # Best M_p subset at this p
    
    best_R2 <- subset_results[[best_R2_idx]]
    best_AIC <- subset_results[[best_AIC_idx]]
    best_BIC <- subset_results[[best_BIC_idx]]
    best_Mp <- subset_results[[best_Mp_idx]]
    
    results[[p]] <- data.frame(
      p = p,
      R2 = best_R2$R2,
      RSS = best_R2$RSS,
      AIC = best_AIC$AIC,        # Use AIC from best AIC model
      BIC = best_BIC$BIC,        # Use BIC from best BIC model
      subset = I(list(best_R2$subset)),
      subset_AIC = I(list(best_AIC$subset)),
      subset_BIC = I(list(best_BIC$subset)),
      subset_Mp = I(list(best_Mp$subset))  # Best M_p subset at this p
    )
    
    # Only print confirmation for every 5th p
    if (p %% 5 == 1 || p == p_max) {
      cat(" ✓\n")
    }
  }
  
  # Combine and print summary
  df <- do.call(rbind, results)
  cat(sprintf("    ✓ Completed R² curve computation for all p\n"))
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
  n_points <- length(M_p)
  
  # Second derivative using central finite differences with h=1
  # u''(x) ≈ [u(x-h) - 2u(x) + u(x+h)] / h²
  # Since h=1, we have: u''(x) = u(x-1) - 2u(x) + u(x+1)
  
  delta2 <- numeric(n_points)
  
  # Interior points: standard central difference
  for (i in 2:(n_points - 1)) {
    delta2[i] <- M_p[i - 1] - 2 * M_p[i] + M_p[i + 1]
  }
  
  # Boundary points: forward/backward differences
  # First point: u''(1) = -2*u(1) + u(2) (assuming u(0) = 0)
  delta2[1] <- -2 * M_p[1] + M_p[2]
  
  # Last point: u''(n) = u(n-1) - 2*u(n) (assuming u(n+1) = u(n))
  delta2[n_points] <- M_p[n_points - 1] - 2 * M_p[n_points]
  
  # Find inflection point as maximum of second derivative
  # This represents where the M_p curve transitions from concave to convex
  argmax_delta2 <- which.max(delta2)
  
  # p* is one step before the maximum (since delta2[i] represents change at i)
  p_star <- p_vals[argmax_delta2 - 1]
  
  # Get corresponding subset - use the M_p-optimal subset at this p*
  subset <- r2_curve$subset_Mp[[argmax_delta2 - 1]]
  
  return(list(
    metric = "M_p",
    p_star = p_star,
    subset = subset,
    M_p = M_p,
    delta2 = delta2,
    inflection_index = argmax_delta2,
    method = "argmax_delta2_minus_1"
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
  subset <- r2_curve$subset_AIC[[min_idx]]
  
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
  subset <- r2_curve$subset_BIC[[min_idx]]
  
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
