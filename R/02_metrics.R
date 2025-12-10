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
  
  if (length(p_vals) < 4) {
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

  # Third derivative
  # u'''(p) = u''(p+1) - u''(p)
  # Dies misst, wie schnell sich die Krümmung ändert
  
  delta3 <- numeric(n_points - 1)
  for (i in 1:(n_points - 1)) {
    delta3[i] <- delta2[i + 1] - delta2[i]
  }
  
  # Find inflection point as minimum of third derivative (strongest negative gradient of curvature (second derivative decreasing most rapidly) )
  # This represents where the M_p curve transitions from concave to convex
  argmin_delta3 <- which.min(delta3)
  
  # p* is one step before the minimum (since delta3[i] represents change at i)
  p_star <- p_vals[argmin_delta3]
  
  # Get corresponding subset - use the M_p-optimal subset at this p*
  subset <- r2_curve$subset_Mp[[argmin_delta3]]
  
  return(list(
    metric = "M_p",
    p_star = p_star,
    subset = subset,
    M_p = M_p,
    delta2 = delta2,
    delta3 = delta3,
    method = "argmin_delta3"
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


#' Sigmoid M_p: Inverted Sigmoid Fit Method
#'
#' Fits an inverted sigmoid function to the M_p curve:
#'   f(p) = alpha + beta / (1 + exp(gamma * (p - delta)))
#' 
#' The parameter delta represents the horizontal position (inflection point)
#' and is returned as p*.
#'
#' @param r2_curve Data frame from compute_r2_curve()
#' @return List with p_star, subset, method, fitted parameters
#' @export
metric_sigmoid_mp <- function(r2_curve) {
  
  p_vals <- r2_curve$p
  R2_vals <- r2_curve$R2
  M_vals <- R2_vals / p_vals
  
  # Check if we have enough points
  if (length(p_vals) < 4) {
    return(list(
      metric = "sigmoid_mp",
      p_star = 1,
      subset = r2_curve$subset[[1]],
      method = "insufficient_points",
      params = NULL,
      fitted_curve = NULL
    ))
  }
  
  # Robust starting values
  # For inverted sigmoid: f(p) = alpha + beta / (1 + exp(gamma * (p - delta)))
  # alpha = lower asymptote (M_p at high p)
  # alpha + beta = upper asymptote (M_p at low p)
  # delta = inflection point
  
  alpha_start <- min(M_vals)
  beta_start <- max(M_vals) - min(M_vals)
  
  # Estimate inflection point from steepest descent
  # Find where the first derivative (approximated) is most negative
  if (length(M_vals) >= 3) {
    delta1 <- diff(M_vals)
    min_idx <- which.min(delta1)
    # Inflection should be around the steepest descent
    delta_start <- p_vals[min_idx + 1]
  } else {
    delta_start <- median(p_vals)
  }
  
  # Estimate gamma from slope at inflection
  # At inflection: df/dp = -beta * gamma / 4
  # Approximate slope: delta1[min_idx]
  if (exists("min_idx") && min_idx <= length(delta1)) {
    slope_approx <- delta1[min_idx]
    gamma_start <- abs(4 * slope_approx / beta_start)
    # Ensure reasonable bounds
    gamma_start <- max(0.1, min(gamma_start, 5))
  } else {
    gamma_start <- 1
  }
  
  # Try nls fit with algorithm="port" for bounded parameters
  fit_success <- FALSE
  fit <- NULL
  
  tryCatch({
    fit <- nls(
      M_vals ~ alpha + beta / (1 + exp(gamma * (p_vals - delta))),
      start = list(
        alpha = alpha_start,
        beta = beta_start,
        gamma = gamma_start,
        delta = delta_start
      ),
      algorithm = "port",
      lower = c(alpha = 0, beta = 0, gamma = 0.01, delta = min(p_vals)),
      upper = c(alpha = max(M_vals), beta = 2 * beta_start, gamma = 10, delta = max(p_vals)),
      control = nls.control(maxiter = 200, warnOnly = TRUE)
    )
    fit_success <- TRUE
  }, error = function(e) {
    # Fallback: try nlsLM if available
    if (requireNamespace("minpack.lm", quietly = TRUE)) {
      tryCatch({
        fit <<- minpack.lm::nlsLM(
          M_vals ~ alpha + beta / (1 + exp(gamma * (p_vals - delta))),
          start = list(
            alpha = alpha_start,
            beta = beta_start,
            gamma = gamma_start,
            delta = delta_start
          ),
          lower = c(alpha = 0, beta = 0, gamma = 0.01, delta = min(p_vals)),
          upper = c(alpha = max(M_vals), beta = 2 * beta_start, gamma = 10, delta = max(p_vals)),
          control = minpack.lm::nls.lm.control(maxiter = 200)
        )
        fit_success <<- TRUE
      }, error = function(e2) {
        # Both methods failed
      })
    }
  })
  
  # If fit failed, return p_star = 1
  if (!fit_success || is.null(fit)) {
    return(list(
      metric = "sigmoid_mp",
      p_star = 1,
      subset = r2_curve$subset[[1]],
      method = "fit_failed",
      params = NULL,
      fitted_curve = NULL
    ))
  }
  
  # Extract parameters
  params <- coef(fit)
  alpha <- params["alpha"]
  beta <- params["beta"]
  gamma <- params["gamma"]
  delta <- params["delta"]
  
  # Handle inverted case (gamma < 0)
  if (gamma < 0) {
    beta <- -beta
    gamma <- -gamma
    params["beta"] <- beta
    params["gamma"] <- gamma
  }
  
  # Determine p_star
  p_star <- round(delta)
  p_star <- max(min(p_star, max(p_vals)), min(p_vals))  # clamp to domain
  
  # Get corresponding subset
  subset <- r2_curve$subset_Mp[[p_star]]
  
  # Compute fitted curve for all p values
  fitted_curve <- alpha + beta / (1 + exp(gamma * (p_vals - delta)))
  
  return(list(
    metric = "sigmoid_mp",
    p_star = p_star,
    subset = subset,
    method = "sigmoid_fit",
    params = params,
    fitted_curve = fitted_curve,
    delta = delta  # raw delta value before rounding
  ))
}


#' Apply All Metrics
#'
#' @param r2_curve Data frame
#' @param methods Character vector. Which methods to apply. 
#'   Options: "all", "derivative", "sigmoid", or specific names like c("M_p", "sigmoid_mp")
#' @return List of metric results
#' @export
apply_all_metrics <- function(r2_curve, methods = "all") {
  
  # Normalize methods input
  if (identical(methods, "all")) {
    methods <- c("M_p", "AIC", "BIC")
  } else if (identical(methods, "derivative")) {
    methods <- c("M_p", "AIC", "BIC")
  } else if (identical(methods, "sigmoid")) {
    methods <- c("sigmoid_mp", "AIC", "BIC")
  }
  
  # Build result list based on requested methods
  results <- list()
  
  if ("M_p" %in% methods) {
    results$M_p <- metric_mp(r2_curve)
  }
  
  if ("sigmoid_mp" %in% methods) {
    results$sigmoid_mp <- metric_sigmoid_mp(r2_curve)
  }
  
  if ("AIC" %in% methods) {
    results$AIC <- metric_aic(r2_curve)
  }
  
  if ("BIC" %in% methods) {
    results$BIC <- metric_bic(r2_curve)
  }
  
  return(results)
}
