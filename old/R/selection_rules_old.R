# Model Selection Rules Module
# ==============================
# Implements the M_p selection rule for optimal model complexity

#' Selection Rule: M_p (Curvature Method)
#'
#' Identifies the optimal p where the M_p curve shows maximum curvature change.
#' This represents the bias-variance tradeoff: where marginal gains flatten out.
#'
#' Strategy:
#' 1. M_p = R²/p typically decreases but at different rates
#' 2. Compute second differences (discrete d²M_p/dp²)
#' 3. Find where |d²M_p| is LARGEST (maximum curvature)
#' 4. This is where the "elbow" occurs in the curve
#'
#' Interpretation:
#' - Before elbow: R² increases rapidly (low cost per dimension)
#' - At elbow: Steepest change in rate (optimal tradeoff)
#' - After elbow: R² plateaus (diminishing returns)
#'
#' The maximum |d²M_p| identifies where the curve bends most sharply,
#' corresponding to the classic "elbow method" used in k-means, scree plots, etc.
#'
#' @param best_models Data frame. Best models by cardinality
#' @return List with p_star, selected_model, and diagnostics
#' @export
selection_rule_mp <- function(best_models) {
  
  # Order by p
  best_models <- best_models[order(best_models$p), ]
  
  if (nrow(best_models) < 3) {
    # Need at least 3 points for second derivative
    # Default to smallest p
    return(list(
      rule = "mp_curvature",
      p_star = best_models$p[1],
      selected_model = best_models[1, ],
      is_degenerate = TRUE,
      elbow_index = NA,
      max_curvature = NA,
      method = "insufficient_points"
    ))
  }
  
  # Extract values
  p_vals <- best_models$p
  Mp_vals <- best_models$Mp
  
  # First differences (discrete derivative dM_p/dp)
  dMp <- diff(Mp_vals)
  
  # Second differences (discrete second derivative d²M_p/dp²)
  d2Mp <- diff(dMp)
  
  # Find maximum absolute curvature (elbow point)
  # This is where the curve bends most sharply
  max_curvature_idx <- which.max(abs(d2Mp))
  max_curvature_value <- d2Mp[max_curvature_idx]
  
  # The elbow is at position max_curvature_idx + 1 in p_vals
  # (since d2Mp is doubly differenced, starting from index 3)
  elbow_idx <- max_curvature_idx + 1
  p_star <- p_vals[elbow_idx]
  
  selected_model <- best_models[best_models$p == p_star, ][1, ]
  
  # Check if Mp is approximately constant (degenerate case)
  mp_range <- max(Mp_vals) - min(Mp_vals)
  is_degenerate <- mp_range < 1e-6
  
  return(list(
    rule = "mp_curvature",
    p_star = p_star,
    selected_model = selected_model,
    is_degenerate = is_degenerate,
    elbow_index = elbow_idx,
    max_curvature = max_curvature_value,
    second_derivatives = d2Mp,
    method = "maximum_curvature"
  ))
}


#' Apply Selection Rule and Compare with AIC/BIC
#'
#' @param results Data frame. Full results from evaluate_all_models_batch
#' @return List with results from M_p rule and comparison methods
#' @export
apply_selection_rule <- function(results) {
  
  # Get best models by p
  best_models <- find_best_models_by_p(results)
  
  # Apply M_p rule
  mp <- selection_rule_mp(best_models)
  
  # Also get AIC and BIC selections for comparison
  p_star_aic <- best_models$p[which.min(best_models$AIC)]
  p_star_bic <- best_models$p[which.min(best_models$BIC)]
  p_star_adjr2 <- best_models$p[which.max(best_models$adjR2)]
  
  return(list(
    best_models = best_models,
    mp = mp,
    p_star_aic = p_star_aic,
    p_star_bic = p_star_bic,
    p_star_adjr2 = p_star_adjr2
  ))
}

#' Apply All Selection Rules (backward compatibility alias)
#' @export
apply_all_selection_rules <- apply_selection_rule


#' Find Best Model for Each Cardinality (backward compatibility)
#' @export
find_best_models_by_p <- function(results) {
  
  # Remove rows with p=0 if present
  results <- results[results$p > 0, ]
  
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


#' Find Optimal Model (backward compatibility wrapper)
#'
#' Uses M_p rule
#'
#' @param results Data frame. Model evaluation results
#' @return List with optimal model information
#' @export
find_optimal_model <- function(results) {
  
  selection_results <- apply_selection_rule(results)
  
  # Use M_p rule
  mp <- selection_results$mp
  
  return(list(
    p_star = mp$p_star,
    best_models = selection_results$best_models,
    optimal_model = mp$selected_model,
    selection_results = selection_results
  ))
}
