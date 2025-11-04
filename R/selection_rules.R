# Model Selection Rules Module
# ==============================
# Implements the M_p selection rule for optimal model complexity

#' Selection Rule: M_p
#'
#' Identifies the p where M_p drops most sharply to next p,
#' then chooses p just before that largest drop.
#'
#' @param best_models Data frame. Best models by cardinality
#' @return List with p_star, selected_model, and diagnostics
#' @export
selection_rule_mp <- function(best_models) {
  
  # Order by p
  best_models <- best_models[order(best_models$p), ]
  
  if (nrow(best_models) < 2) {
    # Only one cardinality available
    return(list(
      rule = "mp",
      p_star = best_models$p[1],
      selected_model = best_models[1, ],
      is_degenerate = TRUE,
      largest_drop = NA,
      drop_location = NA
    ))
  }
  
  # Calculate first differences (rate of change in Mp)
  p_vals <- best_models$p
  Mp_vals <- best_models$Mp
  dMp <- diff(Mp_vals)
  
  # Find largest negative drop
  largest_drop_idx <- which.min(dMp)
  largest_drop_value <- dMp[largest_drop_idx]
  
  # p* is the p just before the largest drop
  # i.e., at position largest_drop_idx (before transition to largest_drop_idx + 1)
  p_star <- p_vals[largest_drop_idx]
  
  # Special case: if all drops are positive or zero (Mp increasing),
  # choose the last p (maximum p)
  if (largest_drop_value >= 0) {
    p_star <- max(p_vals)
  }
  
  selected_model <- best_models[best_models$p == p_star, ][1, ]
  
  # Check if Mp is approximately constant (degenerate case)
  mp_range <- max(Mp_vals) - min(Mp_vals)
  is_degenerate <- mp_range < 1e-6
  
  return(list(
    rule = "mp",
    p_star = p_star,
    selected_model = selected_model,
    is_degenerate = is_degenerate,
    largest_drop = largest_drop_value,
    drop_location = p_vals[largest_drop_idx],
    all_drops = dMp
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
