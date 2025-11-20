# Model Enumeration and Evaluation Module
# =========================================
# Enhanced model enumeration with sampling strategies and
# comprehensive evaluation metrics

#' Generate All Subsets of 1:p_max
#'
#' Helper function to generate power set
#'
#' @param p_max Integer. Number of predictors
#' @return List of integer vectors (all non-empty subsets)
#' @keywords internal
generate_all_subsets <- function(p_max) {
  # Generate all non-empty subsets of 1:p_max
  n_models <- 2^p_max - 1  # Exclude empty set
  subsets <- vector("list", n_models)
  
  for (i in 1:n_models) {
    # Convert i to binary representation to get subset
    binary <- as.integer(intToBits(i))[1:p_max]
    subsets[[i]] <- which(binary == 1)
  }
  
  return(subsets)
}


#' Enumerate Models with Various Strategies
#'
#' @param p_max Integer. Maximum number of predictors
#' @param strategy Character. "full", "cardinality_limited", "random"
#' @param max_p Integer. For cardinality_limited, maximum subset size
#' @param n_samples Integer. For random, number of subsets to sample
#' @param include_null Logical. Include empty model
#' @return List of integer vectors (subset indices)
#' @export
enumerate_models <- function(p_max, 
                             strategy = "full",
                             max_p = NULL,
                             n_samples = NULL,
                             include_null = FALSE) {
  
  if (strategy == "full") {
    # Full power set enumeration
    if (p_max > 14) {
      warning(sprintf("Full enumeration with p_max=%d will create %d models. Consider strategy='cardinality_limited' or 'random'",
                     p_max, 2^p_max - 1))
    }
    
    subsets <- generate_all_subsets(p_max)
    
  } else if (strategy == "cardinality_limited") {
    # Only enumerate subsets up to size max_p
    if (is.null(max_p)) {
      max_p <- min(6, p_max)
    }
    
    subsets <- list()
    idx <- 1
    for (p in 1:min(max_p, p_max)) {
      combs <- combn(p_max, p, simplify = FALSE)
      for (comb in combs) {
        subsets[[idx]] <- comb
        idx <- idx + 1
      }
    }
    
  } else if (strategy == "random") {
    # Random sampling of subsets
    if (is.null(n_samples)) {
      n_samples <- min(1000, 2^p_max - 1)
    }
    
    subsets <- list()
    for (i in 1:n_samples) {
      # Random subset size (1 to p_max)
      p <- sample(1:p_max, 1)
      # Random subset of that size
      subsets[[i]] <- sort(sample(1:p_max, p))
    }
    
    # Remove duplicates
    subsets <- unique(subsets)
    
  } else {
    stop("Unknown enumeration strategy")
  }
  
  # Add null model if requested
  if (include_null) {
    subsets <- c(list(integer(0)), subsets)
  }
  
  return(subsets)
}


#' Evaluate Single Model with Comprehensive Metrics
#'
#' @param X Matrix. Design matrix (n x p_max)
#' @param y Vector. Response (length n)
#' @param subset Integer vector. Predictor indices to include
#' @return List with extensive model statistics
#' @export
evaluate_model <- function(X, y, subset) {
  
  n <- length(y)
  
  # Handle empty subset (null model)
  if (length(subset) == 0) {
    y_hat <- rep(mean(y), n)
    residuals <- y - y_hat
    RSS <- sum(residuals^2)
    TSS <- sum((y - mean(y))^2)
    R2 <- 0
    adjR2 <- 0
    Mp <- NA
    
    # Log-likelihood for null model
    log_lik <- -n/2 * log(2*pi) - n/2 * log(RSS/n) - n/2
    AIC <- -2 * log_lik + 2 * 1  # Only estimate sigma^2
    BIC <- -2 * log_lik + 1 * log(n)
    
    return(list(
      subset = subset,
      p = 0,
      subset_str = "null",
      RSS = RSS,
      TSS = TSS,
      R2 = R2,
      adjR2 = adjR2,
      Mp = Mp,
      AIC = AIC,
      BIC = BIC,
      coef = numeric(0),
      se = numeric(0),
      y_hat = y_hat
    ))
  }
  
  # Extract relevant columns
  X_sub <- X[, subset, drop = FALSE]
  
  # Fit model with intercept
  model <- lm(y ~ X_sub)
  
  # Extract coefficients and standard errors
  coef_full <- coef(model)
  se_full <- summary(model)$coefficients[, "Std. Error"]
  
  # Predictions and residuals
  y_hat <- fitted(model)
  residuals <- residuals(model)
  
  # Basic statistics
  RSS <- sum(residuals^2)
  TSS <- sum((y - mean(y))^2)
  R2 <- 1 - RSS / TSS
  
  # Adjusted R²
  p <- length(subset)
  if (n > p + 1) {
    adjR2 <- 1 - (RSS / (n - p - 1)) / (TSS / (n - 1))
  } else {
    adjR2 <- NA
  }
  
  # M_p metric
  Mp <- R2 / p
  
  # Information criteria
  # k = p + 1 (intercept) + 1 (sigma^2 estimate)
  k <- p + 2
  log_lik <- -n/2 * log(2*pi) - n/2 * log(RSS/n) - n/2
  AIC <- -2 * log_lik + 2 * k
  BIC <- -2 * log_lik + k * log(n)
  
  # Format subset string
  subset_str <- paste(subset, collapse = ",")
  
  return(list(
    subset = subset,
    p = p,
    subset_str = subset_str,
    RSS = RSS,
    TSS = TSS,
    R2 = R2,
    adjR2 = adjR2,
    Mp = Mp,
    AIC = AIC,
    BIC = BIC,
    coef = coef_full[-1],  # Exclude intercept
    se = se_full[-1],
    intercept = coef_full[1],
    intercept_se = se_full[1],
    y_hat = y_hat
  ))
}


#' Evaluate All Models (wrapper for batch evaluation)
#'
#' @param X Matrix. Design matrix
#' @param y Vector. Response
#' @param subsets List. List of subset vectors (from enumerate_models)
#' @param verbose Logical. Print progress
#' @return Data frame with all model results
#' @export
evaluate_all_models_batch <- function(X, y, subsets, verbose = TRUE) {
  
  n_models <- length(subsets)
  
  if (verbose) {
    cat(sprintf("Evaluating %d models...\n", n_models))
  }
  
  # Initialize results storage
  results_list <- vector("list", n_models)
  
  # Progress bar
  if (verbose && n_models > 100) {
    pb <- txtProgressBar(min = 0, max = n_models, style = 3)
  }
  
  # Evaluate each model
  for (i in 1:n_models) {
    fit <- evaluate_model(X, y, subsets[[i]])
    
    results_list[[i]] <- data.frame(
      model_id = i,
      p = fit$p,
      subset_str = fit$subset_str,
      R2 = fit$R2,
      adjR2 = fit$adjR2,
      Mp = fit$Mp,
      AIC = fit$AIC,
      BIC = fit$BIC,
      RSS = fit$RSS,
      TSS = fit$TSS,
      stringsAsFactors = FALSE
    )
    
    if (verbose && n_models > 100) {
      setTxtProgressBar(pb, i)
    }
  }
  
  if (verbose && n_models > 100) {
    close(pb)
  }
  
  # Combine results
  results <- do.call(rbind, results_list)
  
  if (verbose) {
    cat(sprintf("  Evaluated %d models\n", nrow(results)))
    cat(sprintf("  R² range: [%.4f, %.4f]\n", min(results$R2, na.rm = TRUE), max(results$R2, na.rm = TRUE)))
    cat(sprintf("  M_p range: [%.4f, %.4f]\n", min(results$Mp, na.rm = TRUE), max(results$Mp, na.rm = TRUE)))
  }
  
  return(results)
}


#' Backward compatibility wrapper
#' @export
evaluate_all_models <- function(X, y, verbose = TRUE) {
  p_max <- ncol(X)
  subsets <- enumerate_models(p_max, strategy = "full")
  evaluate_all_models_batch(X, y, subsets, verbose)
}


#' Aggregate Results by Cardinality
#'
#' @param results Data frame. Output from evaluate_all_models_batch
#' @return Data frame with summary statistics per p
#' @export
aggregate_by_p <- function(results) {
  
  # Remove null model if present
  results_nonull <- results[results$p > 0, ]
  
  # Split by p
  by_p <- split(results_nonull, results_nonull$p)
  
  # Compute summaries
  summary_list <- lapply(by_p, function(df) {
    data.frame(
      p = unique(df$p),
      n_models = nrow(df),
      Mp_mean = mean(df$Mp, na.rm = TRUE),
      Mp_sd = sd(df$Mp, na.rm = TRUE),
      Mp_median = median(df$Mp, na.rm = TRUE),
      Mp_max = max(df$Mp, na.rm = TRUE),
      R2_mean = mean(df$R2, na.rm = TRUE),
      R2_max = max(df$R2, na.rm = TRUE),
      adjR2_mean = mean(df$adjR2, na.rm = TRUE),
      adjR2_max = max(df$adjR2, na.rm = TRUE),
      AIC_min = min(df$AIC, na.rm = TRUE),
      BIC_min = min(df$BIC, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  
  summary_df <- do.call(rbind, summary_list)
  rownames(summary_df) <- NULL
  
  return(summary_df)
}
