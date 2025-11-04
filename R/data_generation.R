# Advanced Data Generation Module
# =================================
# Supports various predictor distributions, correlation structures,
# heteroscedasticity, measurement error, and other complexities

#' Generate Covariance Matrix with Correlation Structure
#'
#' @param p Integer. Dimension
#' @param structure Character. Type: "identity", "ar1", "block", "compound"
#' @param rho Numeric. Correlation parameter
#' @param block_size Integer. For block structure
#' @return Matrix. Covariance matrix
generate_covariance_matrix <- function(p, structure = "identity", rho = 0, block_size = 5) {
  
  if (structure == "identity") {
    return(diag(p))
  }
  
  if (structure == "ar1") {
    # AR(1): Σ[i,j] = rho^|i-j|
    Sigma <- matrix(0, p, p)
    for (i in 1:p) {
      for (j in 1:p) {
        Sigma[i, j] <- rho^abs(i - j)
      }
    }
    return(Sigma)
  }
  
  if (structure == "compound") {
    # Compound symmetry: diagonal 1, off-diagonal rho
    Sigma <- matrix(rho, p, p)
    diag(Sigma) <- 1
    return(Sigma)
  }
  
  if (structure == "block") {
    # Block diagonal with within-block correlation rho
    Sigma <- diag(p)
    n_blocks <- ceiling(p / block_size)
    for (b in 1:n_blocks) {
      start_idx <- (b - 1) * block_size + 1
      end_idx <- min(b * block_size, p)
      block_indices <- start_idx:end_idx
      Sigma[block_indices, block_indices] <- rho
      diag(Sigma)[block_indices] <- 1
    }
    return(Sigma)
  }
  
  stop("Unknown correlation structure")
}


#' Generate Multivariate Normal Data
#'
#' @param n Integer. Sample size
#' @param p Integer. Number of variables
#' @param mu Vector. Means (length p)
#' @param Sigma Matrix. Covariance matrix (p x p)
#' @return Matrix. n x p matrix of data
generate_mvnormal <- function(n, p, mu = rep(0, p), Sigma = diag(p)) {
  
  # Cholesky decomposition
  L <- chol(Sigma)
  
  # Generate standard normal
  Z <- matrix(rnorm(n * p), nrow = n, ncol = p)
  
  # Transform to desired distribution
  X <- Z %*% L + matrix(mu, nrow = n, ncol = p, byrow = TRUE)
  
  return(X)
}


#' Generate Non-Gaussian Predictors
#'
#' @param n Integer. Sample size
#' @param p Integer. Number of variables
#' @param dist Character. Distribution: "normal", "t", "uniform", "lognormal"
#' @param df Numeric. Degrees of freedom for t-distribution
#' @return Matrix. n x p matrix
generate_nongaussian <- function(n, p, dist = "normal", df = 3) {
  
  if (dist == "normal") {
    return(matrix(rnorm(n * p), n, p))
  }
  
  if (dist == "t") {
    return(matrix(rt(n * p, df = df), n, p))
  }
  
  if (dist == "uniform") {
    return(matrix(runif(n * p, -sqrt(3), sqrt(3)), n, p))  # Unit variance
  }
  
  if (dist == "lognormal") {
    # Log-normal with mean 0, sd 1 (after centering/scaling)
    X <- matrix(rlnorm(n * p), n, p)
    # Standardize
    X <- scale(X)
    return(X)
  }
  
  stop("Unknown distribution")
}


#' Generate Data with Full Configuration
#'
#' @param config List with parameters:
#'   - n: sample size
#'   - p_max: number of predictors
#'   - beta_spec: vector of true coefficients or specification
#'   - sigma_eps: noise standard deviation
#'   - X_dist: predictor distribution ("normal", "t", "uniform", "lognormal")
#'   - correlation_structure: ("identity", "ar1", "block", "compound")
#'   - rho: correlation parameter
#'   - feature_means: vector of predictor means (length p_max)
#'   - feature_sigmas: vector of predictor sds (length p_max)
#'   - measurement_error_sd: sd of measurement error (0 = none)
#'   - heteroscedastic: logical, if TRUE variance depends on X
#'   - add_interactions: logical, if TRUE add interaction terms
#'   - nonlinear_terms: logical, if TRUE add squared/nonlinear terms
#' @return List with X, y, true_beta, meta
#' @export
generate_data_advanced <- function(config) {
  
  # Set defaults
  config <- modifyList(list(
    n = 100,
    p_max = 10,
    beta_spec = c(1, 0.8, 0.5, rep(0, 7)),
    sigma_eps = 0.2,
    X_dist = "normal",
    correlation_structure = "identity",
    rho = 0,
    block_size = 5,
    feature_means = NULL,
    feature_sigmas = NULL,
    measurement_error_sd = 0,
    heteroscedastic = FALSE,
    add_interactions = FALSE,
    nonlinear_terms = FALSE,
    t_df = 3,
    seed = NULL
  ), config)
  
  # Set seed if provided
  if (!is.null(config$seed)) {
    set.seed(config$seed)
  }
  
  n <- config$n
  p_max <- config$p_max
  
  # Parse beta_spec
  if (is.character(config$beta_spec)) {
    if (config$beta_spec == "constant") {
      # Constant incremental variance (S1)
      true_beta <- rep(0.1, p_max)
    } else if (config$beta_spec == "descending") {
      # Descending signal strength (S2)
      p_true <- 3
      true_beta <- c(1.0, 0.8, 0.5, rep(0, p_max - p_true))
    } else if (config$beta_spec == "random") {
      # Random permutation
      p_true <- 3
      beta_vals <- c(1.0, 0.8, 0.5, rep(0, p_max - p_true))
      true_beta <- sample(beta_vals)
    } else {
      stop("Unknown beta_spec string")
    }
  } else {
    true_beta <- config$beta_spec
    if (length(true_beta) != p_max) {
      stop("beta_spec length must equal p_max")
    }
  }
  
  # Generate covariance structure
  if (config$correlation_structure != "identity" || !is.null(config$feature_sigmas)) {
    Sigma <- generate_covariance_matrix(
      p_max, 
      config$correlation_structure, 
      config$rho,
      config$block_size
    )
    
    # Scale by feature_sigmas if provided
    if (!is.null(config$feature_sigmas)) {
      D <- diag(config$feature_sigmas)
      Sigma <- D %*% Sigma %*% D
    }
  } else {
    Sigma <- diag(p_max)
  }
  
  # Set means
  if (is.null(config$feature_means)) {
    mu <- rep(0, p_max)
  } else {
    mu <- config$feature_means
  }
  
  # Generate true predictors
  if (config$X_dist == "normal") {
    X_true <- generate_mvnormal(n, p_max, mu, Sigma)
  } else {
    # Non-Gaussian: generate iid then induce correlation structure
    X_true <- generate_nongaussian(n, p_max, config$X_dist, config$t_df)
    
    # If correlation needed, transform
    if (config$correlation_structure != "identity") {
      # Induce correlation via Cholesky
      L <- chol(Sigma)
      X_true <- X_true %*% L
    }
    
    # Add means
    if (!all(mu == 0)) {
      X_true <- X_true + matrix(mu, n, p_max, byrow = TRUE)
    }
  }
  
  # Add measurement error if specified
  if (config$measurement_error_sd > 0) {
    measurement_error <- matrix(
      rnorm(n * p_max, 0, config$measurement_error_sd), 
      n, p_max
    )
    X_obs <- X_true + measurement_error
  } else {
    X_obs <- X_true
  }
  
  # Generate base linear predictor
  linear_predictor <- X_obs %*% true_beta
  
  # Add nonlinear terms if specified
  if (config$nonlinear_terms && sum(true_beta != 0) >= 2) {
    # Add squared term for first nonzero predictor
    active_idx <- which(true_beta != 0)
    if (length(active_idx) >= 1) {
      # y = ... + 0.3 * x1^2
      linear_predictor <- linear_predictor + 0.3 * X_obs[, active_idx[1]]^2
    }
  }
  
  # Generate errors
  if (config$heteroscedastic) {
    # Variance proportional to |linear_predictor|
    error_sd <- config$sigma_eps * (1 + abs(linear_predictor) / max(abs(linear_predictor)))
    epsilon <- rnorm(n, 0, error_sd)
  } else {
    epsilon <- rnorm(n, 0, config$sigma_eps)
  }
  
  # Generate response
  y <- as.vector(linear_predictor) + epsilon
  
  # Add interaction terms to design matrix if requested
  if (config$add_interactions) {
    active_idx <- which(true_beta != 0)
    if (length(active_idx) >= 2) {
      # Add interaction between first two active predictors
      interaction <- X_obs[, active_idx[1]] * X_obs[, active_idx[2]]
      X_obs <- cbind(X_obs, interaction)
      true_beta <- c(true_beta, 0.5)  # Interaction coefficient
      p_max <- p_max + 1
    }
  }
  
  # Add column names
  colnames(X_obs) <- paste0("X", 1:ncol(X_obs))
  
  # Store metadata
  meta <- config
  meta$p_true <- sum(true_beta != 0)
  meta$true_indices <- which(true_beta != 0)
  meta$timestamp <- Sys.time()
  
  return(list(
    X = X_obs,
    y = y,
    true_beta = true_beta,
    meta = meta
  ))
}


#' Simple wrapper for backward compatibility
#'
#' @export
generate_data <- function(n = 100, 
                         p_max = 10, 
                         true_beta = c(1, 0.8, 0.5, rep(0, 7)),
                         sigma = 0.2) {
  
  config <- list(
    n = n,
    p_max = p_max,
    beta_spec = true_beta,
    sigma_eps = sigma
  )
  
  generate_data_advanced(config)
}
