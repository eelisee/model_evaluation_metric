# Visual Diagnostic for M_p Formulations
# ========================================
# Create plots to visualize why different formulations work/fail

library(ggplot2)

# Load a specific scenario for visualization
scenario_name <- "A1_Baseline_Uncorrelated"
csv_path <- file.path("results", scenario_name, "detailed_results.csv")
data <- read.csv(csv_path, stringsAsFactors = FALSE)

# Average R² across iterations
avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)

R2_vals <- avg_data$R2
p_vals <- avg_data$p
n <- length(p_vals)

# Compute M_p and derivatives
M_p <- R2_vals / p_vals

# Second derivative (central difference)
delta2_central <- numeric(n)
for (i in 2:(n-1)) {
  delta2_central[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
}
delta2_central[1] <- NA
delta2_central[n] <- NA

# Third derivative (central difference)
delta3_central <- diff(delta2_central)

# Create comprehensive plot
png("diagnostic_mp_curves_A1.png", width = 12, height = 10, units = "in", res = 300)
par(mfrow = c(3, 2), mar = c(4, 4.5, 3, 2))

# Plot 1: R² curve
plot(p_vals, R2_vals, type = "b", pch = 19, col = "blue", lwd = 2,
     xlab = "Model Size (p)", ylab = "R²",
     main = "R² Curve")
abline(v = 3, col = "red", lty = 2, lwd = 2)
legend("bottomright", "True p* = 3", col = "red", lty = 2, lwd = 2)
grid()

# Plot 2: M_p curve
plot(p_vals, M_p, type = "b", pch = 19, col = "darkgreen", lwd = 2,
     xlab = "Model Size (p)", ylab = "M_p = R²/p",
     main = "M_p Curve (Efficiency)")
abline(v = 3, col = "red", lty = 2, lwd = 2)
grid()

# Plot 3: First derivative (forward difference)
delta1 <- diff(M_p)
plot(p_vals[-n], delta1, type = "b", pch = 19, col = "orange", lwd = 2,
     xlab = "Model Size (p)", ylab = "Δ₁M_p",
     main = "First Derivative (Rate of Change)")
abline(v = 3, col = "red", lty = 2, lwd = 2)
abline(h = 0, col = "gray", lty = 2)
grid()

# Plot 4: Second derivative (central)
plot(p_vals, delta2_central, type = "b", pch = 19, col = "purple", lwd = 2,
     xlab = "Model Size (p)", ylab = "Δ₂M_p",
     main = "Second Derivative (Curvature)")
abline(v = 3, col = "red", lty = 2, lwd = 2)
abline(h = 0, col = "gray", lty = 2)
grid()

# Highlight min/max of delta2
valid_idx <- which(!is.na(delta2_central))
if (length(valid_idx) > 0) {
  min_idx <- valid_idx[which.min(delta2_central[valid_idx])]
  max_idx <- valid_idx[which.max(delta2_central[valid_idx])]
  points(p_vals[min_idx], delta2_central[min_idx], pch = 17, col = "blue", cex = 2)
  points(p_vals[max_idx], delta2_central[max_idx], pch = 17, col = "darkgreen", cex = 2)
  legend("topright", c("min", "max"), pch = 17, col = c("blue", "darkgreen"), cex = 1.2)
}

# Plot 5: Third derivative (central)
plot(p_vals[-n], delta3_central, type = "b", pch = 19, col = "red", lwd = 2,
     xlab = "Model Size (p)", ylab = "Δ₃M_p",
     main = "Third Derivative (Change in Curvature)")
abline(v = 3, col = "red", lty = 2, lwd = 2)
abline(h = 0, col = "gray", lty = 2)
grid()

# Highlight min and zero crossing
valid_idx3 <- which(!is.na(delta3_central))
if (length(valid_idx3) > 0) {
  min_idx3 <- valid_idx3[which.min(delta3_central[valid_idx3])]
  points(p_vals[min_idx3], delta3_central[min_idx3], pch = 17, col = "blue", cex = 2)
  
  # Find zero crossing
  for (i in 1:(length(delta3_central)-1)) {
    if (!is.na(delta3_central[i]) && !is.na(delta3_central[i+1])) {
      if (delta3_central[i] * delta3_central[i+1] < 0) {
        # Interpolate
        x_cross <- p_vals[i] - delta3_central[i] * (p_vals[i+1] - p_vals[i]) / 
                   (delta3_central[i+1] - delta3_central[i])
        abline(v = ceiling(x_cross), col = "darkgreen", lty = 3, lwd = 3)
        break
      }
    }
  }
  
  legend("topright", c("argmin (current)", "zero crossing ceil (best)"), 
         col = c("blue", "darkgreen"), lty = c(NA, 3), pch = c(17, NA), lwd = 2, cex = 1)
}

# Plot 6: Summary with all selection points
plot(p_vals, M_p, type = "b", pch = 19, col = "black", lwd = 2,
     xlab = "Model Size (p)", ylab = "M_p",
     main = "M_p with Selection Points")
grid()

# True p*
abline(v = 3, col = "red", lty = 1, lwd = 3)

# argmin(delta3) = always 2
abline(v = 2, col = "blue", lty = 2, lwd = 2)

# zero crossing ceil
if (exists("x_cross")) {
  abline(v = ceiling(x_cross), col = "darkgreen", lty = 3, lwd = 2)
}

legend("topright", 
       c("True p* = 3", 
         sprintf("argmin(Δ₃) = %d (WRONG)", p_vals[min_idx3]),
         sprintf("zero_crossing_ceil = %d (CORRECT)", ceiling(x_cross))),
       col = c("red", "blue", "darkgreen"),
       lty = c(1, 2, 3),
       lwd = 2,
       cex = 1.1)

dev.off()

cat("\n✓ Diagnostic plot saved to: diagnostic_mp_curves_A1.png\n\n")

# Print numerical summary
cat("=== NUMERICAL SUMMARY (A1_Baseline_Uncorrelated) ===\n\n")
cat("True p* = 3\n\n")

cat("M_p values:\n")
for (i in 1:min(10, n)) {
  cat(sprintf("  p=%2d: M_p=%.6f\n", p_vals[i], M_p[i]))
}

cat("\nSecond derivative (Δ₂):\n")
for (i in 1:min(10, n)) {
  if (!is.na(delta2_central[i])) {
    cat(sprintf("  p=%2d: Δ₂=%.8f\n", p_vals[i], delta2_central[i]))
  }
}

cat("\nThird derivative (Δ₃):\n")
for (i in 1:min(9, length(delta3_central))) {
  if (!is.na(delta3_central[i])) {
    cat(sprintf("  p=%2d: Δ₃=%.8f\n", p_vals[i], delta3_central[i]))
  }
}

cat("\nSelection results:\n")
cat(sprintf("  argmin(Δ₃) = %d (current implementation - WRONG)\n", p_vals[min_idx3]))
if (exists("x_cross")) {
  cat(sprintf("  Δ₃ zero crossing (ceil) = %d (best formulation - CORRECT)\n", ceiling(x_cross)))
}
cat("\n")
