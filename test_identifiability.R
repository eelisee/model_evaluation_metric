# Test: Minimum Detectable Signal Constraint
# ==========================================

source("R/01_data_generation.R")

cat("\n")
cat("=================================================================\n")
cat("  TEST: IDENTIFIABILITY-CONSTRAINED β GENERATION\n")
cat("=================================================================\n\n")

# Setup parameters
n <- 500
p <- 10
p_true <- 3
sigma_eps <- 0.2
alpha <- 0.05

cat("PARAMETER:\n")
cat(sprintf("  n = %d (sample size)\n", n))
cat(sprintf("  p = %d (predictors)\n", p))
cat(sprintf("  p* = %d (support size)\n", p_true))
cat(sprintf("  σ_ε = %.2f (noise std dev)\n", sigma_eps))
cat(sprintf("  α = %.2f (significance level)\n\n", alpha))

# Critical value
z_alpha <- qnorm(1 - alpha/2)
cat(sprintf("Critical value z_{1-α/2} = %.3f\n\n", z_alpha))

# =================================================================
# SCENARIO 1: Identity Covariance (uncorrelated)
# =================================================================
cat("=================================================================\n")
cat("SCENARIO 1: Identity Covariance (uncorrelated)\n")
cat("=================================================================\n\n")

Sigma_identity <- diag(p)
Sigma_inv_identity <- solve(Sigma_identity)

cat("Σ = I (identity matrix)\n")
cat("  → All predictors independent\n")
cat("  → No multicollinearity\n\n")

# Minimum detectable signal
min_signal_identity <- z_alpha * (sigma_eps / sqrt(n)) * sqrt(diag(Sigma_inv_identity))

cat("Minimum detectable signal per coefficient:\n")
cat(sprintf("  |β_j| ≥ %.4f  (constant for all j)\n\n", min_signal_identity[1]))

# Generate beta WITHOUT constraint
set.seed(123)
beta_no_constraint <- generate_beta(
  p = p,
  support_spec = p_true,
  signal_strength = "weak"
)

cat("β WITHOUT identifiability constraint (weak signal):\n")
active_idx <- which(beta_no_constraint != 0)
for (j in active_idx) {
  detectable <- ifelse(abs(beta_no_constraint[j]) >= min_signal_identity[j], "✓", "✗")
  cat(sprintf("  β[%d] = %+.4f  [min: %.4f] %s\n", 
              j, beta_no_constraint[j], min_signal_identity[j], detectable))
}

# Generate beta WITH constraint
set.seed(123)
beta_with_constraint <- generate_beta(
  p = p,
  support_spec = p_true,
  signal_strength = "weak",
  Sigma = Sigma_identity,
  n = n,
  sigma_eps = sigma_eps,
  alpha = alpha
)

cat("\nβ WITH identifiability constraint (weak signal):\n")
active_idx <- which(beta_with_constraint != 0)
for (j in active_idx) {
  detectable <- ifelse(abs(beta_with_constraint[j]) >= min_signal_identity[j], "✓", "✗")
  cat(sprintf("  β[%d] = %+.4f  [min: %.4f] %s\n", 
              j, beta_with_constraint[j], min_signal_identity[j], detectable))
}

# =================================================================
# SCENARIO 2: AR(1) with high correlation
# =================================================================
cat("\n\n=================================================================\n")
cat("SCENARIO 2: AR(1) Covariance (ρ = 0.8, high correlation)\n")
cat("=================================================================\n\n")

rho <- 0.8
Sigma_ar1 <- matrix(0, p, p)
for (i in 1:p) {
  for (j in 1:p) {
    Sigma_ar1[i, j] <- rho^abs(i - j)
  }
}

Sigma_inv_ar1 <- solve(Sigma_ar1)

cat(sprintf("Σ = AR(1) with ρ = %.1f\n", rho))
cat("  → Predictors correlated\n")
cat("  → Multicollinearity present\n\n")

# Minimum detectable signal
min_signal_ar1 <- z_alpha * (sigma_eps / sqrt(n)) * sqrt(diag(Sigma_inv_ar1))

cat("Minimum detectable signal per coefficient:\n")
cat("  Variable:  1      2      3      4      5      6      7      8      9     10\n")
cat("  Min |β_j|:")
for (j in 1:p) {
  cat(sprintf(" %.3f", min_signal_ar1[j]))
}
cat("\n\n")

cat("NOTE: Minimum signal varies by position due to correlation structure!\n")
cat(sprintf("  Min signal range: [%.4f, %.4f]\n", 
            min(min_signal_ar1), max(min_signal_ar1)))
cat(sprintf("  Inflation factor: %.2fx\n\n", 
            max(min_signal_ar1) / min(min_signal_ar1)))

# Generate beta WITHOUT constraint
set.seed(456)
beta_no_constraint_ar1 <- generate_beta(
  p = p,
  support_spec = p_true,
  signal_strength = "weak"
)

cat("β WITHOUT identifiability constraint (weak signal):\n")
active_idx <- which(beta_no_constraint_ar1 != 0)
for (j in active_idx) {
  detectable <- ifelse(abs(beta_no_constraint_ar1[j]) >= min_signal_ar1[j], "✓", "✗")
  cat(sprintf("  β[%d] = %+.4f  [min: %.4f] %s\n", 
              j, beta_no_constraint_ar1[j], min_signal_ar1[j], detectable))
}

# Generate beta WITH constraint
set.seed(456)
beta_with_constraint_ar1 <- generate_beta(
  p = p,
  support_spec = p_true,
  signal_strength = "weak",
  Sigma = Sigma_ar1,
  n = n,
  sigma_eps = sigma_eps,
  alpha = alpha
)

cat("\nβ WITH identifiability constraint (weak signal):\n")
active_idx <- which(beta_with_constraint_ar1 != 0)
for (j in active_idx) {
  detectable <- ifelse(abs(beta_with_constraint_ar1[j]) >= min_signal_ar1[j], "✓", "✗")
  cat(sprintf("  β[%d] = %+.4f  [min: %.4f] %s\n", 
              j, beta_with_constraint_ar1[j], min_signal_ar1[j], detectable))
}

# =================================================================
# SCENARIO 3: Strong signals (should not be affected much)
# =================================================================
cat("\n\n=================================================================\n")
cat("SCENARIO 3: Strong signals with AR(1) correlation\n")
cat("=================================================================\n\n")

# Generate beta WITHOUT constraint
set.seed(789)
beta_strong_no <- generate_beta(
  p = p,
  support_spec = p_true,
  signal_strength = "strong"
)

cat("β WITHOUT constraint (strong signal, AR1):\n")
active_idx <- which(beta_strong_no != 0)
for (j in active_idx) {
  detectable <- ifelse(abs(beta_strong_no[j]) >= min_signal_ar1[j], "✓", "✗")
  cat(sprintf("  β[%d] = %+.4f  [min: %.4f] %s\n", 
              j, beta_strong_no[j], min_signal_ar1[j], detectable))
}

# Generate beta WITH constraint
set.seed(789)
beta_strong_with <- generate_beta(
  p = p,
  support_spec = p_true,
  signal_strength = "strong",
  Sigma = Sigma_ar1,
  n = n,
  sigma_eps = sigma_eps,
  alpha = alpha
)

cat("\nβ WITH constraint (strong signal, AR1):\n")
active_idx <- which(beta_strong_with != 0)
for (j in active_idx) {
  detectable <- ifelse(abs(beta_strong_with[j]) >= min_signal_ar1[j], "✓", "✗")
  cat(sprintf("  β[%d] = %+.4f  [min: %.4f] %s\n", 
              j, beta_strong_with[j], min_signal_ar1[j], detectable))
}

cat("\nNOTE: Strong signals typically already exceed minimum threshold.\n")
cat("      The constraint mainly affects weak signals.\n")

# =================================================================
# Visualisierung
# =================================================================
cat("\n\n=================================================================\n")
cat("  VISUALISIERUNG\n")
cat("=================================================================\n\n")

png("identifiability_constraint_test.png", width = 12, height = 8, units = "in", res = 300)
par(mfrow = c(2, 2), mar = c(4, 4.5, 3, 2))

# Panel 1: Identity - Without constraint
plot(1:p, abs(beta_no_constraint), type = "h", lwd = 8, col = "#2E86AB",
     main = "Identity Σ: Without Constraint",
     xlab = "Variable", ylab = "|β|", ylim = c(0, max(abs(beta_no_constraint)) + 0.05))
points(1:p, abs(beta_no_constraint), pch = 19, cex = 1.5, col = "#2E86AB")
abline(h = min_signal_identity[1], col = "#E63946", lwd = 2, lty = 2)
text(8, min_signal_identity[1] + 0.01, "Min detectable", col = "#E63946", cex = 0.9)
active <- which(beta_no_constraint != 0)
failed <- active[abs(beta_no_constraint[active]) < min_signal_identity[active]]
if (length(failed) > 0) {
  points(failed, abs(beta_no_constraint[failed]), pch = 4, cex = 2, col = "#E63946", lwd = 2)
}

# Panel 2: Identity - With constraint
plot(1:p, abs(beta_with_constraint), type = "h", lwd = 8, col = "#28A745",
     main = "Identity Σ: With Constraint",
     xlab = "Variable", ylab = "|β|", ylim = c(0, max(abs(beta_with_constraint)) + 0.05))
points(1:p, abs(beta_with_constraint), pch = 19, cex = 1.5, col = "#28A745")
abline(h = min_signal_identity[1], col = "#E63946", lwd = 2, lty = 2)
text(8, min_signal_identity[1] + 0.01, "Min detectable", col = "#E63946", cex = 0.9)

# Panel 3: AR(1) - Without constraint
plot(1:p, abs(beta_no_constraint_ar1), type = "h", lwd = 8, col = "#2E86AB",
     main = "AR(1) Σ (ρ=0.8): Without Constraint",
     xlab = "Variable", ylab = "|β|", ylim = c(0, max(c(abs(beta_no_constraint_ar1), min_signal_ar1)) + 0.05))
points(1:p, abs(beta_no_constraint_ar1), pch = 19, cex = 1.5, col = "#2E86AB")
lines(1:p, min_signal_ar1, col = "#E63946", lwd = 2, lty = 2)
text(8, max(min_signal_ar1) + 0.01, "Min detectable", col = "#E63946", cex = 0.9)
active <- which(beta_no_constraint_ar1 != 0)
failed <- active[abs(beta_no_constraint_ar1[active]) < min_signal_ar1[active]]
if (length(failed) > 0) {
  points(failed, abs(beta_no_constraint_ar1[failed]), pch = 4, cex = 2, col = "#E63946", lwd = 2)
}

# Panel 4: AR(1) - With constraint
plot(1:p, abs(beta_with_constraint_ar1), type = "h", lwd = 8, col = "#28A745",
     main = "AR(1) Σ (ρ=0.8): With Constraint",
     xlab = "Variable", ylab = "|β|", ylim = c(0, max(c(abs(beta_with_constraint_ar1), min_signal_ar1)) + 0.05))
points(1:p, abs(beta_with_constraint_ar1), pch = 19, cex = 1.5, col = "#28A745")
lines(1:p, min_signal_ar1, col = "#E63946", lwd = 2, lty = 2)
text(8, max(min_signal_ar1) + 0.01, "Min detectable", col = "#E63946", cex = 0.9)

dev.off()

cat("✓ Plot gespeichert: identifiability_constraint_test.png\n\n")

cat("=================================================================\n")
cat("  ZUSAMMENFASSUNG\n")
cat("=================================================================\n\n")

cat("Die neue Funktion generate_beta() prüft nun:\n\n")
cat("1. Berechnung der minimalen detektierbaren Signalstärke:\n")
cat("   |β_j| ≥ z_{α} × (σ_ε/√n) × √(Σ^{-1})_jj\n\n")

cat("2. Bei WEAK signals:\n")
cat("   - Ohne Constraint: Können unter Schwellwert fallen\n")
cat("   - Mit Constraint: Werden auf Mindestwert angehoben\n\n")

cat("3. Bei STRONG signals:\n")
cat("   - Meist bereits über Schwellwert\n")
cat("   - Constraint hat wenig Effekt\n\n")

cat("4. Effekt der Korrelation:\n")
cat("   - Identity Σ: Gleicher Schwellwert für alle Variablen\n")
cat("   - AR(1) Σ: Unterschiedliche Schwellwerte je nach Position\n")
cat("   - Höhere Korrelation → höhere Schwellwerte nötig\n\n")

cat("=================================================================\n\n")
