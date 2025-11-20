# Alternative Selection Rules (Experimental/Joke Implementations)
# =================================================================
# This file contains alternative metrics that are likely inferior to M_p
# but interesting to test empirically

#' Alternative Rule 1: M_p_minus (R² / (p-1))
#'
#' Divides R² by (p-1) instead of p. Special case: p=1 gives M_p_minus = 0
#' 
#' Rationale: "The first predictor is free!" - Only penalize additional predictors
#' Expected behavior: Likely to over-select compared to M_p
#'
#' @param best_models Data frame with best model per p (from aggregate_by_p)
#' @return List with p_star, selected_model, diagnostics, and is_degenerate flag
selection_rule_mp_minus <- function(best_models) {
  
  # Calculate M_p_minus = R² / (p-1), with special case for p=1
  best_models$Mp_minus <- ifelse(
    best_models$p == 1,
    0,  # Special case: first predictor is "free"
    best_models$R2 / (best_models$p - 1)
  )
  
  # Check for degeneracy (all values very similar)
  if (length(unique(round(best_models$Mp_minus, 6))) <= 2) {
    return(list(
      p_star = NA,
      selected_model = NULL,
      Mp_minus_values = best_models$Mp_minus,
      diagnostics = "Degenerate: M_p_minus values are constant",
      is_degenerate = TRUE
    ))
  }
  
  # Find steepest drop
  n_p <- nrow(best_models)
  if (n_p < 2) {
    return(list(
      p_star = 1,
      selected_model = best_models[1, ],
      Mp_minus_values = best_models$Mp_minus,
      diagnostics = "Only one complexity level available",
      is_degenerate = FALSE
    ))
  }
  
  drops <- rep(0, n_p - 1)
  for (i in 1:(n_p - 1)) {
    drops[i] <- best_models$Mp_minus[i] - best_models$Mp_minus[i + 1]
  }
  
  # Find p with maximum drop
  max_drop_idx <- which.max(drops)
  p_star <- best_models$p[max_drop_idx]
  
  return(list(
    p_star = p_star,
    selected_model = best_models[best_models$p == p_star, ],
    Mp_minus_values = best_models$Mp_minus,
    drops = drops,
    diagnostics = sprintf("Steepest drop at p=%d (drop=%.4f)", p_star, drops[max_drop_idx]),
    is_degenerate = FALSE
  ))
}


#' Alternative Rule 2: M_p_sqrt (R² / √p)
#'
#' Divides R² by the square root of p instead of p
#' 
#' Rationale: "Softer penalty for complexity" - Sublinear penalization
#' Expected behavior: Likely to over-select compared to M_p
#'
#' @param best_models Data frame with best model per p (from aggregate_by_p)
#' @return List with p_star, selected_model, diagnostics, and is_degenerate flag
selection_rule_mp_sqrt <- function(best_models) {
  
  # Calculate M_p_sqrt = R² / sqrt(p)
  best_models$Mp_sqrt <- best_models$R2 / sqrt(best_models$p)
  
  # Check for degeneracy
  if (length(unique(round(best_models$Mp_sqrt, 6))) <= 2) {
    return(list(
      p_star = NA,
      selected_model = NULL,
      Mp_sqrt_values = best_models$Mp_sqrt,
      diagnostics = "Degenerate: M_p_sqrt values are constant",
      is_degenerate = TRUE
    ))
  }
  
  # Find steepest drop
  n_p <- nrow(best_models)
  if (n_p < 2) {
    return(list(
      p_star = 1,
      selected_model = best_models[1, ],
      Mp_sqrt_values = best_models$Mp_sqrt,
      diagnostics = "Only one complexity level available",
      is_degenerate = FALSE
    ))
  }
  
  drops <- rep(0, n_p - 1)
  for (i in 1:(n_p - 1)) {
    drops[i] <- best_models$Mp_sqrt[i] - best_models$Mp_sqrt[i + 1]
  }
  
  # Find p with maximum drop
  max_drop_idx <- which.max(drops)
  p_star <- best_models$p[max_drop_idx]
  
  return(list(
    p_star = p_star,
    selected_model = best_models[best_models$p == p_star, ],
    Mp_sqrt_values = best_models$Mp_sqrt,
    drops = drops,
    diagnostics = sprintf("Steepest drop at p=%d (drop=%.4f)", p_star, drops[max_drop_idx]),
    is_degenerate = FALSE
  ))
}


#' Alternative Rule 3: M_p_scaled (R² / (p/p_max))
#'
#' Divides R² by the relative complexity p/p_max instead of absolute p
#' Equivalently: M_p_scaled = (R² * p_max) / p
#' 
#' Rationale: "Complexity should be relative to the problem size"
#' Expected behavior: Scales M_p values by p_max, but relative ordering unchanged
#'
#' @param best_models Data frame with best model per p (from aggregate_by_p)
#' @param p_max Integer. Maximum number of predictors in the problem
#' @return List with p_star, selected_model, diagnostics, and is_degenerate flag
selection_rule_mp_scaled <- function(best_models, p_max) {
  
  # Calculate M_p_scaled = R² / (p/p_max) = R² * p_max / p
  best_models$Mp_scaled <- (best_models$R2 * p_max) / best_models$p
  
  # Check for degeneracy
  if (length(unique(round(best_models$Mp_scaled, 6))) <= 2) {
    return(list(
      p_star = NA,
      selected_model = NULL,
      Mp_scaled_values = best_models$Mp_scaled,
      diagnostics = "Degenerate: M_p_scaled values are constant",
      is_degenerate = TRUE
    ))
  }
  
  # Find steepest drop
  n_p <- nrow(best_models)
  if (n_p < 2) {
    return(list(
      p_star = 1,
      selected_model = best_models[1, ],
      Mp_scaled_values = best_models$Mp_scaled,
      diagnostics = "Only one complexity level available",
      is_degenerate = FALSE
    ))
  }
  
  drops <- rep(0, n_p - 1)
  for (i in 1:(n_p - 1)) {
    drops[i] <- best_models$Mp_scaled[i] - best_models$Mp_scaled[i + 1]
  }
  
  # Find p with maximum drop
  max_drop_idx <- which.max(drops)
  p_star <- best_models$p[max_drop_idx]
  
  return(list(
    p_star = p_star,
    selected_model = best_models[best_models$p == p_star, ],
    Mp_scaled_values = best_models$Mp_scaled,
    drops = drops,
    diagnostics = sprintf("Steepest drop at p=%d (drop=%.4f)", p_star, drops[max_drop_idx]),
    is_degenerate = FALSE
  ))
}


#' Apply All Alternative Selection Rules
#'
#' Wrapper function that applies all alternative rules and standard comparisons
#'
#' @param results Data frame with all model evaluation results
#' @param p_max Integer. Maximum number of predictors
#' @return List with selection results from all rules
apply_alternative_rules <- function(results, p_max) {
  
  # Get best models by p (needed for all rules)
  # For each p, find the model with maximum R2
  best_models <- do.call(rbind, lapply(unique(results$p), function(p_val) {
    subset_p <- results[results$p == p_val, ]
    best_idx <- which.max(subset_p$R2)
    subset_p[best_idx, c("p", "R2", "adjR2", "Mp", "AIC", "BIC", "subset_str")]
  }))
  
  # Apply alternative rules
  mp_minus_result <- selection_rule_mp_minus(best_models)
  mp_sqrt_result <- selection_rule_mp_sqrt(best_models)
  mp_scaled_result <- selection_rule_mp_scaled(best_models, p_max)
  
  # Also get standard selections for comparison
  p_star_aic <- best_models$p[which.min(best_models$AIC)]
  p_star_bic <- best_models$p[which.min(best_models$BIC)]
  
  # Return comprehensive results
  return(list(
    mp_minus = mp_minus_result,
    mp_sqrt = mp_sqrt_result,
    mp_scaled = mp_scaled_result,
    p_star_aic = p_star_aic,
    p_star_bic = p_star_bic,
    best_models = best_models,
    aic_selected = best_models[best_models$p == p_star_aic, "subset_str"],
    bic_selected = best_models[best_models$p == p_star_bic, "subset_str"]
  ))
}
