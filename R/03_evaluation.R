# Evaluation Module
# ==================
# Computes MAE, Bias, Variance, Hit Rate for metrics

#' Evaluate Single Iteration
#'
#' @param metric_result List. Result from apply_all_metrics()
#' @param p_true Integer. True model size
#' @return Data frame with metric, p_star, error, hit
#' @export
evaluate_iteration <- function(metric_result, p_true) {
  
  metrics <- names(metric_result)
  
  results <- lapply(metrics, function(m) {
    
    res <- metric_result[[m]]
    p_star <- res$p_star
    error <- p_star - p_true
    
    # Classification
    if (p_star == p_true) {
      classification <- "correct"
    } else if (p_star < p_true) {
      classification <- "underfit"
    } else {
      classification <- "overfit"
    }
    
    data.frame(
      metric = m,
      p_star = p_star,
      error = error,
      abs_error = abs(error),
      hit = (p_star == p_true),
      classification = classification,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, results)
}


#' Compute Summary Statistics Across Iterations
#'
#' @param all_iterations List of evaluation results
#' @param all_r2_curves List of R² curves from all iterations
#' @param p_true Integer. True model size
#' @return Data frame with MAE, Bias, Var, Hit Rate per metric + metric values per p
#' @export
compute_summary_statistics <- function(all_iterations, all_r2_curves, p_true) {
  
  # Combine all iterations
  df <- do.call(rbind, all_iterations)
  
  # Group by metric
  metrics <- unique(df$metric)
  
  summary <- lapply(metrics, function(m) {
    
    subset <- df[df$metric == m, ]
    
    # MAE
    MAE <- mean(subset$abs_error)
    
    # Bias
    Bias <- mean(subset$error)
    
    # Variance
    Var <- var(subset$p_star)
    
    # Hit Rate
    HitRate <- mean(subset$hit)
    
    # Classification counts
    n_correct <- sum(subset$classification == "correct")
    n_underfit <- sum(subset$classification == "underfit")
    n_overfit <- sum(subset$classification == "overfit")
    
    data.frame(
      metric = m,
      MAE = MAE,
      Bias = Bias,
      Variance = Var,
      HitRate = HitRate,
      n_correct = n_correct,
      n_underfit = n_underfit,
      n_overfit = n_overfit,
      stringsAsFactors = FALSE
    )
  })
  
  summary_df <- do.call(rbind, summary)
  
  # Add metric values per p (from first iteration as representative)
  r2_curve <- all_r2_curves[[1]]
  
  # Compute all metric values
  M_p <- r2_curve$R2 / r2_curve$p
  M_p_sqrt <- r2_curve$R2 / sqrt(r2_curve$p)
  
  # Add columns for each p
  for (p in r2_curve$p) {
    idx <- which(r2_curve$p == p)
    
    summary_df[summary_df$metric == "M_p", paste0("p", p, "_value")] <- M_p[idx]
    summary_df[summary_df$metric == "M_p_sqrt", paste0("p", p, "_value")] <- M_p_sqrt[idx]
    summary_df[summary_df$metric == "AIC", paste0("p", p, "_value")] <- r2_curve$AIC[idx]
    summary_df[summary_df$metric == "BIC", paste0("p", p, "_value")] <- r2_curve$BIC[idx]
  }
  
  return(summary_df)
}
