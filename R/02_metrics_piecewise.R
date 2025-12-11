#' Piecewise Linear M_p: Two-Segment Linear Fit Method
#'
#' Fits a piecewise linear function to the M_p curve:
#'   M_p = a - b1×p  for p ≤ p_break
#'   M_p = a - b1×p_break - b2×(p - p_break)  for p > p_break
#' 
#' The breakpoint p_break is optimized and returned as p*.
#' This model captures sharp transitions better than sigmoid.
#'
#' @param r2_curve Data frame from compute_r2_curve()
#' @return List with p_star, subset, method, fitted parameters
#' @export
metric_piecewise_mp <- function(r2_curve) {
  
  p_vals <- r2_curve$p
  R2_vals <- r2_curve$R2
  M_vals <- R2_vals / p_vals
  
  # Check if we have enough points
  if (length(p_vals) < 4) {
    return(list(
      metric = "piecewise_mp",
      p_star = p_vals[1],
      subset = r2_curve$subset_Mp[[1]],
      method = "insufficient_points",
      params = NULL,
      fitted_curve = NULL
    ))
  }
  
  # Try each possible breakpoint and find best fit
  best_rmse <- Inf
  best_p_break <- NULL
  best_params <- NULL
  best_fitted <- NULL
  
  # Try breakpoints from p=2 to p=max-2 (need points on both sides)
  for (p_break in p_vals[2:(length(p_vals)-2)]) {
    
    # Split data
    idx_before <- which(p_vals <= p_break)
    idx_after <- which(p_vals > p_break)
    
    if (length(idx_before) < 2 || length(idx_after) < 2) next
    
    # Fit two linear segments
    # Segment 1: M = a - b1*p  (p <= p_break)
    # Segment 2: M = c - b2*p  (p > p_break)
    # With continuity: c - b2*p_break = a - b1*p_break
    
    tryCatch({
      # Fit first segment: M = a + b1*p
      fit1 <- lm(M_vals[idx_before] ~ p_vals[idx_before])
      a <- coef(fit1)[1]
      b1 <- coef(fit1)[2]
      
      # Fit second segment: M = c + b2*p
      fit2 <- lm(M_vals[idx_after] ~ p_vals[idx_after])
      b2 <- coef(fit2)[2]
      
      # Enforce continuity at breakpoint
      M_at_break <- a + b1 * p_break
      c <- M_at_break - b2 * p_break
      
      # Calculate fitted values
      fitted <- numeric(length(p_vals))
      fitted[idx_before] <- a + b1 * p_vals[idx_before]
      fitted[idx_after] <- c + b2 * p_vals[idx_after]
      
      # Calculate RMSE
      rmse <- sqrt(mean((M_vals - fitted)^2))
      
      # Update best if better
      if (rmse < best_rmse) {
        best_rmse <- rmse
        best_p_break <- p_break
        best_params <- c(a = a, b1 = b1, b2 = b2, c = c)
        best_fitted <- fitted
      }
      
    }, error = function(e) {
      # Skip this breakpoint if fitting fails
    })
  }
  
  # If no valid fit found
  if (is.null(best_p_break)) {
    return(list(
      metric = "piecewise_mp",
      p_star = p_vals[1],
      subset = r2_curve$subset_Mp[[1]],
      method = "fit_failed",
      params = NULL,
      fitted_curve = NULL
    ))
  }
  
  # Find index of breakpoint
  p_star_idx <- which(p_vals == best_p_break)
  p_star <- best_p_break
  subset <- r2_curve$subset_Mp[[p_star_idx]]
  
  return(list(
    metric = "piecewise_mp",
    p_star = p_star,
    subset = subset,
    method = "piecewise_linear_fit",
    params = best_params,
    fitted_curve = best_fitted,
    breakpoint = best_p_break,
    rmse = best_rmse
  ))
}
