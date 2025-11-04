# Recovery and Performance Metrics Module
# ========================================
# Computes variable selection performance metrics

#' Compute Variable Selection Performance Metrics
#'
#' @param selected_subset Integer vector. Selected predictor indices
#' @param true_subset Integer vector. True active predictor indices
#' @param p_max Integer. Total number of candidate predictors
#' @return List with TP, FP, FN, TN, precision, recall, F1, Hamming distance
#' @export
compute_recovery_metrics <- function(selected_subset, true_subset, p_max) {
  
  # Convert to logical vectors
  selected_vec <- rep(FALSE, p_max)
  selected_vec[selected_subset] <- TRUE
  
  true_vec <- rep(FALSE, p_max)
  true_vec[true_subset] <- TRUE
  
  # Confusion matrix elements
  TP <- sum(selected_vec & true_vec)
  FP <- sum(selected_vec & !true_vec)
  FN <- sum(!selected_vec & true_vec)
  TN <- sum(!selected_vec & !true_vec)
  
  # Performance metrics
  precision <- if (TP + FP > 0) TP / (TP + FP) else 0
  recall <- if (TP + FN > 0) TP / (TP + FN) else 0
  F1 <- if (precision + recall > 0) 2 * precision * recall / (precision + recall) else 0
  
  # Hamming distance (number of disagreements)
  hamming <- sum(selected_vec != true_vec)
  
  # Exact match
  exact_match <- all(selected_vec == true_vec)
  
  return(list(
    TP = TP,
    FP = FP,
    FN = FN,
    TN = TN,
    precision = precision,
    recall = recall,
    F1 = F1,
    hamming_distance = hamming,
    exact_match = exact_match,
    p_selected = length(selected_subset),
    p_true = length(true_subset)
  ))
}


#' Parse Subset String to Indices
#'
#' @param subset_str Character. Comma-separated indices
#' @return Integer vector
#' @export
parse_subset_string <- function(subset_str) {
  if (subset_str == "null" || subset_str == "") {
    return(integer(0))
  }
  as.integer(strsplit(subset_str, ",")[[1]])
}


#' Evaluate Selection Rules Against True Model
#'
#' @param selection_results List. Output from apply_selection_rule
#' @param true_beta Vector. True coefficients
#' @return Data frame with metrics for each rule
#' @export
evaluate_selection_rules <- function(selection_results, true_beta) {
  
  p_max <- length(true_beta)
  true_indices <- which(true_beta != 0)
  p_true <- length(true_indices)
  
  # Extract M_p rule
  rules <- list(
    mp = selection_results$mp
  )
  
  # Also evaluate AIC, BIC for comparison
  best_models <- selection_results$best_models
  
  aic_model <- best_models[best_models$p == selection_results$p_star_aic, ][1, ]
  bic_model <- best_models[best_models$p == selection_results$p_star_bic, ][1, ]
  
  rules$AIC <- list(p_star = selection_results$p_star_aic, selected_model = aic_model)
  rules$BIC <- list(p_star = selection_results$p_star_bic, selected_model = bic_model)
  
  # Compute metrics for each rule
  results_list <- lapply(names(rules), function(rule_name) {
    rule <- rules[[rule_name]]
    
    selected_indices <- parse_subset_string(rule$selected_model$subset_str)
    metrics <- compute_recovery_metrics(selected_indices, true_indices, p_max)
    
    data.frame(
      rule = rule_name,
      p_star = rule$p_star,
      p_true = p_true,
      p_diff = rule$p_star - p_true,
      TP = metrics$TP,
      FP = metrics$FP,
      FN = metrics$FN,
      precision = metrics$precision,
      recall = metrics$recall,
      F1 = metrics$F1,
      hamming_distance = metrics$hamming_distance,
      exact_match = metrics$exact_match,
      selected_subset = rule$selected_model$subset_str,
      stringsAsFactors = FALSE
    )
  })
  
  results_df <- do.call(rbind, results_list)
  rownames(results_df) <- NULL
  
  return(results_df)
}


#' Compute Selection Stability Across Runs
#'
#' @param selection_history List of character vectors. Each element is subset_str from a run
#' @return Data frame with stability metrics
#' @export
compute_selection_stability <- function(selection_history) {
  
  # Count frequency of each subset
  subset_counts <- table(unlist(selection_history))
  
  # Most common selection
  most_common <- names(subset_counts)[which.max(subset_counts)]
  most_common_freq <- max(subset_counts) / length(selection_history)
  
  # Average pairwise Hamming distance
  if (length(selection_history) > 1) {
    hamming_distances <- c()
    for (i in 1:(length(selection_history) - 1)) {
      for (j in (i + 1):length(selection_history)) {
        s1 <- parse_subset_string(selection_history[[i]])
        s2 <- parse_subset_string(selection_history[[j]])
        
        # Compute Hamming distance
        p_max <- max(c(s1, s2, 1))
        v1 <- rep(FALSE, p_max)
        v1[s1] <- TRUE
        v2 <- rep(FALSE, p_max)
        v2[s2] <- TRUE
        
        hamming_distances <- c(hamming_distances, sum(v1 != v2))
      }
    }
    avg_hamming <- mean(hamming_distances)
    sd_hamming <- sd(hamming_distances)
  } else {
    avg_hamming <- NA
    sd_hamming <- NA
  }
  
  return(data.frame(
    n_runs = length(selection_history),
    n_unique_selections = length(unique(selection_history)),
    most_common_subset = most_common,
    selection_frequency = most_common_freq,
    avg_pairwise_hamming = avg_hamming,
    sd_pairwise_hamming = sd_hamming,
    stringsAsFactors = FALSE
  ))
}


#' Compute Ranking Correlations
#'
#' @param results Data frame. Full model evaluation results
#' @param true_beta Vector. True coefficients (optional)
#' @return Data frame with rank correlations
#' @export
compute_ranking_correlations <- function(results, true_beta = NULL) {
  
  # Remove NA values
  valid_rows <- complete.cases(results[, c("Mp", "R2", "adjR2", "AIC", "BIC")])
  results <- results[valid_rows, ]
  
  # Rank by each metric
  rank_Mp <- rank(-results$Mp)  # Higher is better
  rank_R2 <- rank(-results$R2)
  rank_adjR2 <- rank(-results$adjR2)
  rank_AIC <- rank(results$AIC)  # Lower is better
  rank_BIC <- rank(results$BIC)
  
  # Compute correlations
  cor_Mp_R2 <- cor(rank_Mp, rank_R2, method = "spearman")
  cor_Mp_adjR2 <- cor(rank_Mp, rank_adjR2, method = "spearman")
  cor_Mp_AIC <- cor(rank_Mp, rank_AIC, method = "spearman")
  cor_Mp_BIC <- cor(rank_Mp, rank_BIC, method = "spearman")
  
  correlations <- data.frame(
    Mp_vs_R2 = cor_Mp_R2,
    Mp_vs_adjR2 = cor_Mp_adjR2,
    Mp_vs_AIC = cor_Mp_AIC,
    Mp_vs_BIC = cor_Mp_BIC,
    stringsAsFactors = FALSE
  )
  
  # If true model known, compute correlation with closeness to truth
  if (!is.null(true_beta)) {
    p_max <- length(true_beta)
    true_indices <- which(true_beta != 0)
    
    # Compute Hamming distance to true model for each evaluated model
    hamming_to_true <- sapply(1:nrow(results), function(i) {
      selected <- parse_subset_string(results$subset_str[i])
      metrics <- compute_recovery_metrics(selected, true_indices, p_max)
      metrics$hamming_distance
    })
    
    # Rank by closeness (negative Hamming = closer is better)
    rank_closeness <- rank(-hamming_to_true)
    
    # Correlations
    correlations$Mp_vs_truth_closeness <- cor(rank_Mp, rank_closeness, method = "spearman")
    correlations$R2_vs_truth_closeness <- cor(rank_R2, rank_closeness, method = "spearman")
    correlations$AIC_vs_truth_closeness <- cor(rank_AIC, rank_closeness, method = "spearman")
    correlations$BIC_vs_truth_closeness <- cor(rank_BIC, rank_closeness, method = "spearman")
  }
  
  return(correlations)
}
