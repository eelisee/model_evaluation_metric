# Scenario Definitions
# =====================
# Configuration generators for all 15 edge case and stress test scenarios

#' S1: Constant Incremental Variance (Degenerate Case)
#'
#' R²(p) = α*p where α = 0.1, resulting in constant M_p = α
#' Tests degeneracy where M_p cannot discriminate between cardinalities
#'
#' @param n Sample size (default 100)
#' @param p_max Number of predictors (default 10)
#' @param sigma_eps Noise level (default 0.2)
#' @param seed Random seed
#' @return Configuration list
#' @export
scenario_s1_constant <- function(n = 100, p_max = 10, sigma_eps = 0.2, seed = NULL) {
  
  # With equal beta values, each predictor contributes equally
  # This creates approximately constant M_p
  beta_val <- sqrt(0.1)  # Tuned to give R² ≈ 0.1*p
  
  list(
    scenario_name = "S1_constant_mp",
    scenario_description = "Constant incremental variance (degenerate M_p)",
    n = n,
    p_max = p_max,
    beta_spec = rep(beta_val, p_max),
    sigma_eps = 0.0, #sigma_eps,
    X_dist = "normal",
    correlation_structure = "identity",
    rho = 0,
    seed = seed,
    expected_behavior = "M_p constant; Rule A ties; Rule B convention-based",
    p_true = p_max
  )
}


#' S2: Descending Signal Strength (Baseline)
#'
#' Standard case with first 3 predictors active: β = [1.0, 0.8, 0.5, 0, ...]
#' Tests typical scenario where M_p should recover true model
#'
#' @export
scenario_s2_baseline <- function(n = 100, p_max = 10, sigma_eps = 0.2, seed = NULL) {
  list(
    scenario_name = "S2_baseline",
    scenario_description = "Descending signal strength (typical case)",
    n = n,
    p_max = p_max,
    beta_spec = c(1.0, 0.8, 0.5, rep(0, p_max - 3)),
    sigma_eps = sigma_eps,
    X_dist = "normal",
    correlation_structure = "identity",
    rho = 0,
    seed = seed,
    expected_behavior = "M_p should select p ≈ 3",
    p_true = 3
  )
}


#' S3: Random Ordered Signals (Order Invariance Test)
#'
#' Same as S2 but predictors randomly permuted
#' Tests that algorithm is order-invariant
#'
#' @export
scenario_s3_random_order <- function(n = 100, p_max = 10, sigma_eps = 0.2, seed = NULL) {
  list(
    scenario_name = "S3_random_order",
    scenario_description = "Random ordering of active predictors",
    n = n,
    p_max = p_max,
    beta_spec = "random",  # Special string handled by generate_data_advanced
    sigma_eps = sigma_eps,
    X_dist = "normal",
    correlation_structure = "identity",
    rho = 0,
    seed = seed,
    expected_behavior = "Same recovery as S2; order shouldn't matter",
    p_true = 3
  )
}


#' S2_ZERO: Descending Signal Strength (Zero Noise)
#'
#' Same as S2 but with zero noise (sigma_eps = 0.0)
#' Tests whether deterministic case recovers true model with exhaustive search
#'
#' @export
scenario_s2_baseline_zero <- function(n = 100, p_max = 10, seed = NULL) {
  list(
    scenario_name = "S2_baseline_zero",
    scenario_description = "Descending signal strength (zero noise)",
    n = n,
    p_max = p_max,
    beta_spec = c(1.0, 0.8, 0.5, rep(0, p_max - 3)),
    sigma_eps = 0.0,
    X_dist = "normal",
    correlation_structure = "identity",
    rho = 0,
    seed = seed,
    expected_behavior = "M_p should select p = 3 (deterministic)",
    p_true = 3
  )
}


#' S3_ZERO: Random Ordered Signals (Zero Noise)
#'
#' Same as S3 but with zero noise (sigma_eps = 0.0)
#' Tests whether deterministic case is truly order-invariant
#'
#' @export
scenario_s3_random_order_zero <- function(n = 100, p_max = 10, seed = NULL) {
  list(
    scenario_name = "S3_random_order_zero",
    scenario_description = "Random ordering of active predictors (zero noise)",
    n = n,
    p_max = p_max,
    beta_spec = "random",  # Special string handled by generate_data_advanced
    sigma_eps = 0.0,
    X_dist = "normal",
    correlation_structure = "identity",
    rho = 0,
    seed = seed,
    expected_behavior = "Should match S2_zero if truly order-invariant",
    p_true = 3
  )
}


#' S4: Heteroskedastic Predictors (Different Variances)
#'
#' Predictors have different variances
#' Tests robustness to predictor scaling
#'
#' @export
scenario_s4_hetero_predictors <- function(n = 100, p_max = 10, sigma_eps = 0.2, seed = NULL) {
  
  # Different scales for predictors
  feature_sigmas <- runif(p_max, 0.5, 2.0)
  
  list(
    scenario_name = "S4_heteroskedastic_predictors",
    scenario_description = "Predictors with different variances",
    n = n,
    p_max = p_max,
    beta_spec = c(1.0, 0.8, 0.5, rep(0, p_max - 3)),
    sigma_eps = sigma_eps,
    X_dist = "normal",
    correlation_structure = "identity",
    rho = 0,
    feature_sigmas = feature_sigmas,
    seed = seed,
    expected_behavior = "M_p should be robust to scaling",
    p_true = 3
  )
}


#' S5: Different Predictor Means
#'
#' Predictors have non-zero means
#' Tests invariance to location shifts (with intercept)
#'
#' @export
scenario_s5_nonzero_means <- function(n = 100, p_max = 10, sigma_eps = 0.2, seed = NULL) {
  
  # Random means
  feature_means <- runif(p_max, -2, 2)
  
  list(
    scenario_name = "S5_nonzero_means",
    scenario_description = "Predictors with non-zero means",
    n = n,
    p_max = p_max,
    beta_spec = c(1.0, 0.8, 0.5, rep(0, p_max - 3)),
    sigma_eps = sigma_eps,
    X_dist = "normal",
    correlation_structure = "identity",
    rho = 0,
    feature_means = feature_means,
    seed = seed,
    expected_behavior = "Same selection as S2 (with intercept in model)",
    p_true = 3
  )
}


#' S6: Collinearity (Correlated Predictors)
#'
#' @param correlation_structure "ar1", "block", or "compound"
#' @param rho Correlation parameter (default 0.8)
#' @export
scenario_s6_collinearity <- function(n = 100, p_max = 10, sigma_eps = 0.2, 
                                      correlation_structure = "ar1", 
                                      rho = 0.8, seed = NULL) {
  list(
    scenario_name = sprintf("S6_collinearity_%s_rho%.2f", correlation_structure, rho),
    scenario_description = sprintf("Correlated predictors (%s, rho=%.2f)", 
                                   correlation_structure, rho),
    n = n,
    p_max = p_max,
    beta_spec = c(1.0, 0.8, 0.5, rep(0, p_max - 3)),
    sigma_eps = sigma_eps,
    X_dist = "normal",
    correlation_structure = correlation_structure,
    rho = rho,
    block_size = 5,
    seed = seed,
    expected_behavior = "M_p may prefer fewer variables; selection less stable",
    p_true = 3
  )
}


#' S7: Weak Signals (Low SNR)
#'
#' @param sigma_eps High noise level (default 1.0)
#' @export
scenario_s7_weak_signal <- function(n = 100, p_max = 10, sigma_eps = 1.0, seed = NULL) {
  list(
    scenario_name = sprintf("S7_weak_signal_sigma%.2f", sigma_eps),
    scenario_description = sprintf("Low SNR (sigma=%.2f)", sigma_eps),
    n = n,
    p_max = p_max,
    beta_spec = c(1.0, 0.8, 0.5, rep(0, p_max - 3)),
    sigma_eps = sigma_eps,
    X_dist = "normal",
    correlation_structure = "identity",
    rho = 0,
    seed = seed,
    expected_behavior = "Lower R²; M_p may favor smaller models",
    p_true = 3
  )
}


#' S8: High-Dimensional (p close to n)
#' S8: Nonlinear True Model (Misspecification) [Renumbered from S9]
#'
#' @export
scenario_s8_nonlinear <- function(n = 100, p_max = 10, sigma_eps = 0.2, seed = NULL) {
  list(
    scenario_name = "S8_nonlinear",
    scenario_description = "Nonlinear true model (OLS misspecification)",
    n = n,
    p_max = p_max,
    beta_spec = c(1.0, 0.8, 0.5, rep(0, p_max - 3)),
    sigma_eps = sigma_eps,
    X_dist = "normal",
    correlation_structure = "identity",
    rho = 0,
    nonlinear_terms = TRUE,  # Adds squared term
    seed = seed,
    expected_behavior = "Linear OLS misspecified; R² may not reach 1",
    p_true = 3
  )
}


#' S9: Interactions Between Predictors [Renumbered from S10]
#'
#' @export
scenario_s9_interactions <- function(n = 100, p_max = 10, sigma_eps = 0.2, seed = NULL) {
  list(
    scenario_name = "S9_interactions",
    scenario_description = "Model with interaction terms",
    n = n,
    p_max = p_max,
    beta_spec = c(1.0, 0.8, 0.5, rep(0, p_max - 3)),
    sigma_eps = sigma_eps,
    X_dist = "normal",
    correlation_structure = "identity",
    rho = 0,
    add_interactions = TRUE,  # Adds interaction column
    seed = seed,
    expected_behavior = "Should select interaction if included in candidate set",
    p_true = 4  # 3 main effects + 1 interaction
  )
}


#' S11: Redundant Variables (Near-Duplicates)
#'
#' Creates duplicate columns by adding small noise
#'
#' @export
scenario_s10_redundant <- function(n = 100, p_max = 10, sigma_eps = 0.2, seed = NULL) {
  
  # Note: This requires special handling in data generation
  # We'll add logic to create X1_copy = X1 + small noise
  
  list(
    scenario_name = "S10_redundant",
    scenario_description = "Redundant/duplicate predictors",
    n = n,
    p_max = p_max,
    beta_spec = c(1.0, 0.8, 0.5, rep(0, p_max - 3)),
    sigma_eps = sigma_eps,
    X_dist = "normal",
    correlation_structure = "compound",  # High correlation
    rho = 0.95,  # Near-duplicates
    seed = seed,
    expected_behavior = "Many equivalent models; equivalence classes",
    p_true = 3
  )
}


#' S11: Measurement Error in Predictors [Renumbered from S12]
#'
#' @param error_sd Standard deviation of measurement error
#' @export
scenario_s11_measurement_error <- function(n = 100, p_max = 10, sigma_eps = 0.2, 
                                           error_sd = 0.2, seed = NULL) {
  list(
    scenario_name = "S11_measurement_error",
    scenario_description = "Measurement error in predictors",
    n = n,
    p_max = p_max,
    beta_spec = c(1.0, 0.8, 0.5, rep(0, p_max - 3)),
    sigma_eps = sigma_eps,
    X_dist = "normal",
    correlation_structure = "identity",
    rho = 0,
    measurement_error = TRUE,
    measurement_error_sd = error_sd,
    seed = seed,
    expected_behavior = "Attenuation bias; reduced R²",
    p_true = 3
  )
}


#' S12: Non-Gaussian Errors or Predictors [Renumbered from S13]
#'
#' @param dist_type Distribution type ("t", "lognormal")
#' @export
scenario_s12_nongaussian <- function(n = 100, p_max = 10, sigma_eps = 0.2, 
                                     dist_type = "t", seed = NULL) {
  list(
    scenario_name = sprintf("S12_nongaussian_%s", dist_type),
    scenario_description = sprintf("Non-Gaussian (%s distribution)", dist_type),
    n = n,
    p_max = p_max,
    beta_spec = c(1.0, 0.8, 0.5, rep(0, p_max - 3)),
    sigma_eps = sigma_eps,
    X_dist = dist_type,
    correlation_structure = "identity",
    rho = 0,
    seed = seed,
    expected_behavior = "Test distribution robustness",
    p_true = 3
  )
}


#' S13: Heteroscedastic Errors (Non-Constant Variance) [Renumbered from S14]
#'
#' Error variance depends on predictors
#'
#' @export
scenario_s13_heteroscedastic_errors <- function(n = 100, p_max = 10, 
                                                sigma_eps = 0.2, seed = NULL) {
  list(
    scenario_name = "S13_heteroscedastic_errors",
    scenario_description = "Non-constant error variance",
    n = n,
    p_max = p_max,
    beta_spec = c(1.0, 0.8, 0.5, rep(0, p_max - 3)),
    sigma_eps = sigma_eps,
    X_dist = "normal",
    correlation_structure = "identity",
    rho = 0,
    heteroscedastic = TRUE,
    seed = seed,
    expected_behavior = "OLS variance assumptions violated",
    p_true = 3
  )
}


#' S14: Group Sparsity (Grouped Variables) [Renumbered from S15]
#'
#' Variables in groups with block correlation
#'
#' @export
scenario_s14_group_sparsity <- function(n = 100, p_max = 10, sigma_eps = 0.2, 
                                        seed = NULL) {
  
  # First 3 variables active, next 2 in same group
  # β = [1.0, 0.8, 0.5, 0.3, 0.3, 0, ...]
  beta_vals <- c(1.0, 0.8, 0.5, 0.3, 0.3, rep(0, p_max - 5))
  
  list(
    scenario_name = "S14_group_sparsity",
    scenario_description = "Grouped active variables",
    n = n,
    p_max = p_max,
    beta_spec = beta_vals,
    sigma_eps = sigma_eps,
    X_dist = "normal",
    correlation_structure = "block",
    rho = 0.7,
    block_size = 5,
    seed = seed,
    expected_behavior = "Test group selection performance",
    p_true = 5
  )
}


#' Get All Scenario Configurations
#'
#' @param include_sweeps Logical. Include parameter sweep variants
#' @return List of scenario configurations
#' @export
get_all_scenarios <- function(include_sweeps = FALSE) {
  
  # Use the same base seed for all scenarios to make results comparable
  base_seed <- 123
  
  scenarios <- list(
    s1 = scenario_s1_constant(seed = base_seed),
    s2 = scenario_s2_baseline(seed = base_seed),
    s3 = scenario_s3_random_order(seed = base_seed),
    s2_zero = scenario_s2_baseline_zero(seed = base_seed),
    s3_zero = scenario_s3_random_order_zero(seed = base_seed),
    s4 = scenario_s4_hetero_predictors(seed = base_seed),
    s5 = scenario_s5_nonzero_means(seed = base_seed),
    s6_ar1 = scenario_s6_collinearity(correlation_structure = "ar1", rho = 0.8, seed = base_seed),
    s7_snr = scenario_s7_weak_signal(sigma_eps = 1.0, seed = base_seed),
    s8_nonlin = scenario_s8_nonlinear(seed = base_seed),
    s9_interact = scenario_s9_interactions(seed = base_seed),
    s10_redund = scenario_s10_redundant(seed = base_seed),
    s11_meas = scenario_s11_measurement_error(error_sd = 0.3, seed = base_seed),
    s12_tdist = scenario_s12_nongaussian(dist_type = "t", seed = base_seed),
    s13_hetero = scenario_s13_heteroscedastic_errors(seed = base_seed),
    s14_group = scenario_s14_group_sparsity(seed = base_seed)
  )
  
  if (include_sweeps) {
    # Add parameter sweeps (using same base seed for comparability)
    
    # SNR sweep
    for (sigma in c(0.1, 0.5, 2.0)) {
      name <- sprintf("s7_snr_sigma%.1f", sigma)
      scenarios[[name]] <- scenario_s7_weak_signal(sigma_eps = sigma, seed = base_seed)
    }
    
    # Collinearity sweep
    for (rho in c(0.3, 0.6, 0.9)) {
      name <- sprintf("s6_ar1_rho%.1f", rho)
      scenarios[[name]] <- scenario_s6_collinearity(rho = rho, seed = base_seed)
    }
    
    # Non-Gaussian variants
    for (dist in c("t", "lognormal")) {
      name <- sprintf("s12_%s", dist)
      scenarios[[name]] <- scenario_s12_nongaussian(dist_type = dist, seed = base_seed)
    }
  }
  
  return(scenarios)
}


#' Get Scenario Description
#'
#' @param scenario_name Character. Scenario identifier
#' @return Character. Human-readable description
#' @export
get_scenario_description <- function(scenario_name) {
  descriptions <- list(
    s1 = "Constant M_p (degenerate)",
    s2 = "Baseline descending signal",
    s3 = "Random order (invariance test)",
    s4 = "Heteroskedastic predictors",
    s5 = "Non-zero predictor means",
    s6 = "Collinearity",
    s7 = "Weak signals (low SNR)",
    s8 = "Nonlinear model",
    s9 = "Interactions",
    s10 = "Redundant variables",
    s11 = "Measurement error",
    s12 = "Non-Gaussian predictors",
    s13 = "Heteroscedastic errors",
    s14 = "Group sparsity",
    s15 = "Decoupled (AR1 + random effects)",
    s16 = "Decoupled (Block + random effects)",
    s17 = "Decoupled (Compound + random effects)"
  )
  
  # Try to match prefix
  for (key in names(descriptions)) {
    if (grepl(key, scenario_name)) {
      return(descriptions[[key]])
    }
  }
  
  return("Unknown scenario")
}


# ============================================================================
# NEUE ENTKOPPELTE SZENARIEN
# ============================================================================
# Diese Szenarien implementieren die Entkopplung zwischen X-Struktur und β

#' S15: Decoupled AR(1) Structure with Random Effects
#'
#' ✅ ENTKOPPELT: X hat AR(1)-Struktur, β-Support und Größe sind unabhängig
#'
#' @param n Sample size (default 100)
#' @param p_max Number of predictors (default 10)
#' @param p_true Number of active predictors (default 5)
#' @param sigma_eps Noise level (default 0.2)
#' @param rho AR(1) correlation (default 0.7)
#' @param beta_sd SD for beta coefficient distribution (default 1.0)
#' @param seed Random seed
#' @export
scenario_s15_decoupled_ar1 <- function(n = 100, p_max = 10, p_true = 5, 
                                       sigma_eps = 0.2, rho = 0.7, 
                                       beta_sd = 1.0, seed = NULL) {
  list(
    scenario_name = sprintf("S15_decoupled_ar1_rho%.2f", rho),
    scenario_description = sprintf("Decoupled: AR(1) structure (rho=%.2f) + random beta", rho),
    n = n,
    p_max = p_max,
    p_true = p_true,
    beta_spec = "decoupled_random",  # ← NEU
    beta_sd = beta_sd,                # ← β ~ N(0, beta_sd^2)
    sigma_eps = sigma_eps,
    X_dist = "normal",
    correlation_structure = "ar1",   # ← Struktur kommt von X
    rho = rho,
    seed = seed,
    expected_behavior = "Realistic: X structure independent of beta",
    notes = "Support positions random, effect sizes from N(0,1)"
  )
}


#' S16: Decoupled Block Structure with Fixed Magnitude Effects
#'
#' ✅ ENTKOPPELT: X hat Block-Struktur, β hat fixe Magnitude mit random signs
#'
#' @export
scenario_s16_decoupled_block <- function(n = 100, p_max = 10, p_true = 5,
                                         sigma_eps = 0.2, rho = 0.6,
                                         block_size = 3, beta_magnitude = 1.0,
                                         seed = NULL) {
  list(
    scenario_name = sprintf("S16_decoupled_block_rho%.2f", rho),
    scenario_description = sprintf("Decoupled: Block structure (rho=%.2f) + fixed magnitude", rho),
    n = n,
    p_max = p_max,
    p_true = p_true,
    beta_spec = "decoupled_random",
    beta_magnitude = beta_magnitude,  # ← β = ±magnitude
    sigma_eps = sigma_eps,
    X_dist = "normal",
    correlation_structure = "block",
    rho = rho,
    block_size = block_size,
    seed = seed,
    expected_behavior = "Block correlations don't bias toward specific indices",
    notes = "All effects ±1.0, positions randomized"
  )
}


#' S17: Decoupled Compound Symmetry with Controlled Effects
#'
#' ✅ ENTKOPPELT: X hat Compound-Struktur, β-Werte aus vorgegebener Liste
#'
#' @export
scenario_s17_decoupled_compound <- function(n = 100, p_max = 10, p_true = 3,
                                            sigma_eps = 0.2, rho = 0.5,
                                            beta_values = c(1.0, 0.8, 0.5),
                                            seed = NULL) {
  list(
    scenario_name = sprintf("S17_decoupled_compound_rho%.2f", rho),
    scenario_description = sprintf("Decoupled: Compound symmetry (rho=%.2f) + controlled effects", rho),
    n = n,
    p_max = p_max,
    p_true = p_true,
    beta_spec = "decoupled_fixed",
    beta_values = beta_values,        # ← Spezifische Werte, aber zufällige Positionen
    sigma_eps = sigma_eps,
    X_dist = "normal",
    correlation_structure = "compound",
    rho = rho,
    seed = seed,
    expected_behavior = "Compound correlation structure independent of effect positions",
    notes = "Effects [1.0, 0.8, 0.5] assigned to random positions"
  )
}
