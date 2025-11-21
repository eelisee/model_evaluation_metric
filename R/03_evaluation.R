# Evaluation Module
# ==================
# Computes MAE, Bias, Variance, Hit Rate for metrics

#' Evaluate Single Iteration
#'
#' @param metric_result List. Result from apply_all_metrics()
#' @param p_true Integer. True model size
#' @param support_true Integer vector. Indices of true active variables
#' @return Data frame with metric, p_star, error, hit, and subset metrics
#' @export
evaluate_iteration <- function(metric_result, p_true, support_true = NULL) {
  
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
    
    # Subset evaluation metrics (if support_true is provided)
    if (!is.null(support_true)) {
      chosen_subset <- res$subset
      
      # True Positives: correctly identified active variables
      TP <- length(intersect(support_true, chosen_subset))
      
      # False Positives: incorrectly identified inactive variables
      FP <- length(setdiff(chosen_subset, support_true))
      
      # False Negatives: missed active variables
      FN <- length(setdiff(support_true, chosen_subset))
      
      # Jaccard Index: intersection / union
      jaccard <- TP / (TP + FP + FN)
      
      # Precision: TP / (TP + FP)
      precision <- if (TP + FP > 0) TP / (TP + FP) else 0
      
      # Recall (Sensitivity): TP / (TP + FN)
      recall <- if (TP + FN > 0) TP / (TP + FN) else 0
      
      # F1 Score: harmonic mean of precision and recall
      f1_score <- if (precision + recall > 0) 2 * (precision * recall) / (precision + recall) else 0
      
      # Perfect subset recovery
      subset_hit <- (TP == length(support_true) && FP == 0)
      
    } else {
      # If support_true not provided, set all to NA
      TP <- NA
      FP <- NA
      FN <- NA
      jaccard <- NA
      precision <- NA
      recall <- NA
      f1_score <- NA
      subset_hit <- NA
    }
    
    data.frame(
      metric = m,
      p_star = p_star,
      error = error,
      abs_error = abs(error),
      hit = (p_star == p_true),
      classification = classification,
      TP = TP,
      FP = FP,
      FN = FN,
      jaccard = jaccard,
      precision = precision,
      recall = recall,
      f1_score = f1_score,
      subset_hit = subset_hit,
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
#' @param all_metric_results List of metric results (optional, for delta2 extraction)
#' @return Data frame with MAE, Bias, Var, Hit Rate per metric + metric values per p
#' @export
compute_summary_statistics <- function(all_iterations, all_r2_curves, p_true, all_metric_results = NULL) {
  
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
    
    # Subset evaluation metrics (average across iterations)
    avg_jaccard <- mean(subset$jaccard, na.rm = TRUE)
    avg_precision <- mean(subset$precision, na.rm = TRUE)
    avg_recall <- mean(subset$recall, na.rm = TRUE)
    avg_f1 <- mean(subset$f1_score, na.rm = TRUE)
    avg_TP <- mean(subset$TP, na.rm = TRUE)
    avg_FP <- mean(subset$FP, na.rm = TRUE)
    avg_FN <- mean(subset$FN, na.rm = TRUE)
    subset_hit_rate <- mean(subset$subset_hit, na.rm = TRUE)
    
    data.frame(
      metric = m,
      MAE = MAE,
      Bias = Bias,
      Variance = Var,
      HitRate = HitRate,
      n_correct = n_correct,
      n_underfit = n_underfit,
      n_overfit = n_overfit,
      Jaccard = avg_jaccard,
      Precision = avg_precision,
      Recall = avg_recall,
      F1 = avg_f1,
      avg_TP = avg_TP,
      avg_FP = avg_FP,
      avg_FN = avg_FN,
      SubsetHitRate = subset_hit_rate,
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
  
  # Note: delta2 values are not included in summary table
  # They are shown in the summary.txt interpretation section for M_p only
  
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
#' @param all_support_true List of support_true vectors from all iterations
#' @param all_beta_true List of beta_true vectors from all iterations
#' @return Data frame with columns: iteration, p, R2, M_p, AIC, BIC, selected_by_*, subsets, support_true
#' @export
create_detailed_results <- function(all_iterations, all_r2_curves, all_metric_results, all_support_true = NULL, all_beta_true = NULL) {
  
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
    
    # Get support_true for this iteration
    support_true_str <- if (!is.null(all_support_true)) {
      paste(all_support_true[[iter]], collapse = ",")
    } else {
      NA
    }
    
    # Order variables by absolute coefficient magnitude (for subset_p)
    if (!is.null(all_beta_true)) {
      beta_true <- all_beta_true[[iter]]
      # Order by absolute value of coefficients (descending)
      ordered_indices <- order(abs(beta_true), decreasing = TRUE)
    } else {
      ordered_indices <- NULL
    }
    
    # Create row for each p
    for (i in seq_along(r2_curve$p)) {
      p <- r2_curve$p[i]
      
      # Get subset for this p ordered by coefficient magnitude
      if (!is.null(ordered_indices)) {
        subset_p <- paste(ordered_indices[1:p], collapse = ",")
      } else {
        subset_p <- paste(r2_curve$subset[[i]], collapse = ",")
      }
      
      # Use GLOBAL final subsets from metric_results
      # But show them progressively: for each p, show only the first p variables from the final subset
      
      # Get the first p variables from each final subset
      final_subset_mp <- metric_results$M_p$subset
      final_subset_aic <- metric_results$AIC$subset
      final_subset_bic <- metric_results$BIC$subset
      
      # Truncate to first p variables (or full subset if p >= length)
      subset_mp_at_p <- if (p <= length(final_subset_mp)) {
        paste(final_subset_mp[1:p], collapse = ",")
      } else {
        paste(final_subset_mp, collapse = ",")
      }
      
      subset_aic_at_p <- if (p <= length(final_subset_aic)) {
        paste(final_subset_aic[1:p], collapse = ",")
      } else {
        paste(final_subset_aic, collapse = ",")
      }
      
      subset_bic_at_p <- if (p <= length(final_subset_bic)) {
        paste(final_subset_bic[1:p], collapse = ",")
      } else {
        paste(final_subset_bic, collapse = ",")
      }
      
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
        subset_p = subset_p,
        subset_Mp = subset_mp_at_p,      # First p variables from M_p's final subset
        subset_AIC = subset_aic_at_p,    # First p variables from AIC's final subset
        subset_BIC = subset_bic_at_p,    # First p variables from BIC's final subset
        support_true = support_true_str,
        stringsAsFactors = FALSE
      )
    }
  }
  
  detailed_df <- do.call(rbind, detailed_rows)
  return(detailed_df)
}

