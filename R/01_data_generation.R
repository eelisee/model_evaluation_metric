# Data Generation Module
# =======================
# Generates X ~ N(0, Σ), β (sparse), ε ~ N(0, σ²), y = Xβ + ε

#' Generate Covariance Matrix
#'
#' @param p Integer. Number of predictors
#' @param structure String. "identity", "ar1", "compound", "block"
#' @param rho Numeric. Correlation parameter
#' @param block_size Integer. For block structure
#' @return Matrix. p x p covariance matrix
generate_sigma <- function(p, structure = "identity", rho = 0, block_size = 5) {
  
  if (structure == "identity") {
    return(diag(p))
  }
  
  if (structure == "ar1") {
    # AR(1): Σ[i,j] = ρ^|i-j|
    Sigma <- matrix(0, p, p)
    for (i in 1:p) {
      for (j in 1:p) {
        Sigma[i, j] <- rho^abs(i - j)
      }
    }
    return(Sigma)
  }
  
  if (structure == "compound") {
    # Compound symmetry: all off-diagonals = ρ
    Sigma <- matrix(rho, p, p)
    diag(Sigma) <- 1
    return(Sigma)
  }
  
  if (structure == "block") {
    # Block diagonal with within-block correlation
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
  
  stop("Unknown structure: ", structure)
}


#' Generate β Support and Values
#'
#' @param p Integer. Total number of predictors
#' @param support_spec String or Integer. 
#'   - "single": only 1 active variable
#'   - "full": all variables active
#'   - Integer: number of active variables
#' @param signal_strength String. "strong", "weak", "mixed"
#' @return Vector. β of length p
generate_beta <- function(p, support_spec = 3, signal_strength = "strong") {
  
  # Determine support size
  if (is.character(support_spec)) {
    if (support_spec == "single") {
      p_true <- 1
    } else if (support_spec == "full") {
      p_true <- p
    } else {
      stop("Unknown support_spec: ", support_spec)
    }
  } else {
    p_true <- support_spec
  }
  
  # Random support positions
  support <- sample(1:p, p_true, replace = FALSE)
  
  # Generate coefficient values
  if (signal_strength == "strong") {
    # Strong signals: uniform [0.5, 1.5] with random sign
    magnitudes <- runif(p_true, 0.5, 1.5)
    values <- magnitudes * sample(c(-1, 1), p_true, replace = TRUE)
    
  } else if (signal_strength == "weak") {
    # Weak signals: N(0, 0.1²)
    values <- rnorm(p_true, 0, 0.1)
    
  } else if (signal_strength == "mixed") {
    # Mixed: first 3 strong, rest weak
    n_strong <- min(3, p_true)
    n_weak <- p_true - n_strong
    
    strong_vals <- runif(n_strong, 0.5, 1.5) * sample(c(-1, 1), n_strong, replace = TRUE)
    weak_vals <- rnorm(n_weak, 0, 0.1)
    
    values <- c(strong_vals, weak_vals)
    
  } else {
    stop("Unknown signal_strength: ", signal_strength)
  }
  
  # Construct β
  beta <- rep(0, p)
  beta[support] <- values
  
  return(beta)
}


#' Generate Design Matrix X
#'
#' @param n Integer. Sample size
#' @param Sigma Matrix. p x p covariance matrix
#' @return Matrix. n x p design matrix
generate_X <- function(n, Sigma) {
  
  p <- nrow(Sigma)
  
  # Cholesky decomposition
  L <- chol(Sigma)
  
  # Generate X ~ N(0, Σ)
  Z <- matrix(rnorm(n * p), nrow = n, ncol = p)
  X <- Z %*% L
  
  return(X)
}


#' Generate Complete Dataset
#'
#' @param scenario List with:
#'   - n: sample size
#'   - p: number of predictors
#'   - sigma_structure: covariance structure
#'   - rho: correlation parameter
#'   - support_spec: support specification
#'   - signal_strength: signal strength
#'   - sigma_eps: noise standard deviation
#'   - seed: random seed (optional)
#' @return List with X, y, beta_true, support_true, Sigma
#' @export
generate_data <- function(scenario) {
  
  # Set seed if provided
  if (!is.null(scenario$seed)) {
    set.seed(scenario$seed)
  }
  
  # Generate covariance matrix
  Sigma <- generate_sigma(
    p = scenario$p,
    structure = scenario$sigma_structure,
    rho = scenario$rho,
    block_size = scenario$block_size %||% 5
  )
  
  # Generate X ~ N(0, Σ)
  X <- generate_X(scenario$n, Sigma)
  
  # Generate β (support and values)
  beta_true <- generate_beta(
    p = scenario$p,
    support_spec = scenario$support_spec,
    signal_strength = scenario$signal_strength
  )
  
  # Generate noise ε ~ N(0, σ²)
  epsilon <- rnorm(scenario$n, 0, scenario$sigma_eps)
  
  # Compute y = Xβ + ε
  y <- X %*% beta_true + epsilon
  y <- as.vector(y)
  
  # Extract true support
  support_true <- which(beta_true != 0)
  p_true <- length(support_true)
  
  return(list(
    X = X,
    y = y,
    beta_true = beta_true,
    support_true = support_true,
    p_true = p_true,
    Sigma = Sigma,
    scenario = scenario
  ))
}


# Helper: NULL coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a
