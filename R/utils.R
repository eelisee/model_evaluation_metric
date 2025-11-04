# Utility Functions for M_p Model Evaluation
# ============================================

#' Pipe operator for convenience
#' @export
`%>%` <- function(lhs, rhs) {
  eval(substitute(rhs), envir = list(. = lhs), enclos = parent.frame())
}


#' Format Model Subset as String
#'
#' Converts a vector of predictor indices to a readable string
#'
#' @param subset Integer vector. Predictor indices
#' @param prefix Character. Prefix for variable names (default: "X")
#' @return Character string
#' @export
format_subset <- function(subset, prefix = "X") {
  paste0(prefix, subset, collapse = ", ")
}


#' Extract Predictor Indices from String
#'
#' Parses comma-separated string of indices back to integer vector
#'
#' @param subset_str Character. Comma-separated predictor indices
#' @return Integer vector
#' @export
parse_subset_string <- function(subset_str) {
  as.integer(strsplit(subset_str, ",")[[1]])
}


#' Create Summary Table
#'
#' Generates a nicely formatted summary table
#'
#' @param best_models Data frame. Best models by p
#' @param digits Integer. Number of decimal places
#' @return Data frame
#' @export
create_summary_table <- function(best_models, digits = 4) {
  
  summary_table <- best_models[, c("p", "R2", "Mp", "subset_str")]
  summary_table$R2 <- round(summary_table$R2, digits)
  summary_table$Mp <- round(summary_table$Mp, digits)
  
  colnames(summary_table) <- c("p", "R²", "M_p", "Predictors")
  
  return(summary_table)
}


#' Check Package Installation
#'
#' Checks if required packages are installed and installs if missing
#'
#' @param packages Character vector. Package names
#' @export
check_packages <- function(packages = c("ggplot2")) {
  
  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
  
  if (length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
    install.packages(missing_packages)
    cat("Installation complete!\n")
  } else {
    cat("All required packages are installed.\n")
  }
}


#' Validate Data Inputs
#'
#' Checks that data inputs are valid for analysis
#'
#' @param X Matrix. Design matrix
#' @param y Vector. Response variable
#' @export
validate_inputs <- function(X, y) {
  
  # Check dimensions
  if (!is.matrix(X)) {
    stop("X must be a matrix")
  }
  
  if (!is.numeric(y)) {
    stop("y must be numeric")
  }
  
  if (nrow(X) != length(y)) {
    stop("Number of rows in X must match length of y")
  }
  
  if (ncol(X) > 12) {
    warning("p_max > 12 may result in long computation time (2^p models)")
  }
  
  if (nrow(X) < ncol(X)) {
    warning("Number of observations < number of predictors (n < p)")
  }
  
  # Check for missing values
  if (any(is.na(X))) {
    stop("X contains missing values")
  }
  
  if (any(is.na(y))) {
    stop("y contains missing values")
  }
  
  # Check for constant columns
  constant_cols <- apply(X, 2, function(col) length(unique(col)) == 1)
  if (any(constant_cols)) {
    warning(sprintf("X contains %d constant column(s)", sum(constant_cols)))
  }
  
  cat("Data validation passed!\n")
}


#' Compute Adjusted R²
#'
#' Calculates adjusted R² for a given model
#'
#' @param R2 Numeric. R-squared value
#' @param n Integer. Number of observations
#' @param p Integer. Number of predictors
#' @return Numeric. Adjusted R²
#' @export
compute_adjusted_r2 <- function(R2, n, p) {
  1 - (1 - R2) * (n - 1) / (n - p - 1)
}


#' Export Results to LaTeX Table
#'
#' Creates a LaTeX-formatted table of results
#'
#' @param best_models Data frame. Best models by p
#' @param file Character. Output file path (optional)
#' @export
export_latex_table <- function(best_models, file = NULL) {
  
  # Format data
  latex_table <- "\\begin{table}[h]\n"
  latex_table <- paste0(latex_table, "\\centering\n")
  latex_table <- paste0(latex_table, "\\caption{Best Models by Cardinality}\n")
  latex_table <- paste0(latex_table, "\\begin{tabular}{cccc}\n")
  latex_table <- paste0(latex_table, "\\hline\n")
  latex_table <- paste0(latex_table, "$p$ & $R^2$ & $M_p$ & Predictors \\\\\n")
  latex_table <- paste0(latex_table, "\\hline\n")
  
  for (i in 1:nrow(best_models)) {
    latex_table <- paste0(
      latex_table,
      sprintf("%d & %.4f & %.4f & %s \\\\\n",
              best_models$p[i],
              best_models$R2[i],
              best_models$Mp[i],
              best_models$subset_str[i])
    )
  }
  
  latex_table <- paste0(latex_table, "\\hline\n")
  latex_table <- paste0(latex_table, "\\end{tabular}\n")
  latex_table <- paste0(latex_table, "\\end{table}\n")
  
  if (!is.null(file)) {
    writeLines(latex_table, file)
    cat(sprintf("LaTeX table saved to: %s\n", file))
  }
  
  return(latex_table)
}


#' Quick Analysis Function
#'
#' Runs a complete M_p analysis with minimal setup
#'
#' @param n Integer. Number of observations
#' @param p_max Integer. Maximum number of predictors
#' @param true_beta Numeric vector. True coefficients
#' @param sigma Numeric. Noise level
#' @param seed Integer. Random seed
#' @return List containing all results
#' @export
quick_analysis <- function(n = 100, 
                          p_max = 10, 
                          true_beta = c(1, 0.8, 0.5, rep(0, 7)),
                          sigma = 0.2,
                          seed = 123) {
  
  set.seed(seed)
  
  # Generate data
  data <- generate_data(n, p_max, true_beta, sigma)
  
  # Evaluate models
  results <- evaluate_all_models(data$X, data$y, verbose = TRUE)
  
  # Find optimal
  optimal <- find_optimal_model(results)
  
  # Print summary
  print_summary(optimal, sum(true_beta != 0))
  
  # Plot
  p <- plot_mp_curves(results, optimal$p_star)
  print(p)
  
  return(list(
    data = data,
    results = results,
    optimal = optimal
  ))
}
