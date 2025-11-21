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
  
  # Add R² values for each p (same for all metrics)
  for (p in r2_curve$p) {
    idx <- which(r2_curve$p == p)
    summary_df[, paste0("p", p, "_R2")] <- r2_curve$R2[idx]
  }
  
  # Add metric-specific values for each p
  for (p in r2_curve$p) {
    idx <- which(r2_curve$p == p)
    
    summary_df[summary_df$metric == "M_p", paste0("p", p, "_value")] <- M_p[idx]
    summary_df[summary_df$metric == "AIC", paste0("p", p, "_value")] <- r2_curve$AIC[idx]
    summary_df[summary_df$metric == "BIC", paste0("p", p, "_value")] <- r2_curve$BIC[idx]
  }
  
  return(summary_df)
}


#' Create Detailed Results Data Frame
#'
#' Exports all metric values for each iteration and each p.
#' This allows flexible post-hoc analysis and custom plotting.
#'
#' @param all_iterations List of evaluation results
#' @param all_r2_curves List of R² curves from all iterations
#' @param all_metric_results List of metric results from all iterations
#' @return Data frame with columns: iteration, p, R2, M_p, AIC, BIC, selected_by_Mp, selected_by_AIC, selected_by_BIC
#' @export
create_detailed_results <- function(all_iterations, all_r2_curves, all_metric_results) {
  
  detailed_rows <- list()
  
  for (iter in seq_along(all_r2_curves)) {
    
    r2_curve <- all_r2_curves[[iter]]
    metric_results <- all_metric_results[[iter]]
    
    # Compute M_p curve
    M_p <- r2_curve$R2 / r2_curve$p
    
    # Get selected p* for each metric
    p_star_mp <- metric_results$M_p$p_star
    p_star_aic <- metric_results$AIC$p_star
    p_star_bic <- metric_results$BIC$p_star
    
    # Create row for each p
    for (i in seq_along(r2_curve$p)) {
      p <- r2_curve$p[i]
      
      detailed_rows[[length(detailed_rows) + 1]] <- data.frame(
        iteration = iter,
        p = p,
        R2 = r2_curve$R2[i],
        M_p = M_p[i],
        AIC = r2_curve$AIC[i],
        BIC = r2_curve$BIC[i],
        selected_by_Mp = (p == p_star_mp),
        selected_by_AIC = (p == p_star_aic),
        selected_by_BIC = (p == p_star_bic),
        stringsAsFactors = FALSE
      )
    }
  }
  
  detailed_df <- do.call(rbind, detailed_rows)
  return(detailed_df)
}

