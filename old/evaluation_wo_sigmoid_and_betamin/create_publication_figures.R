#!/usr/bin/env Rscript

# Generate publication-quality plots for LaTeX document
# Creates 3 main figures demonstrating M_p metric limitations

# Load scenario data
load_scenario <- function(scenario_name) {
  file_path <- sprintf("results/%s/detailed_results.csv", scenario_name)
  data <- read.csv(file_path)
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  names(avg_data)[2] <- "R2_mean"
  avg_data <- avg_data[order(avg_data$p), ]
  return(avg_data$R2_mean)
}

# Compute derivatives with proper boundary conditions
compute_derivatives <- function(M_p) {
  n <- length(M_p)
  delta1 <- numeric(n)
  delta2 <- numeric(n)
  delta3 <- numeric(n)
  
  # First derivative
  delta1[1] <- M_p[2] - M_p[1]
  delta1[n] <- M_p[n] - M_p[n-1]
  for (i in 2:(n-1)) {
    delta1[i] <- (M_p[i+1] - M_p[i-1]) / 2
  }
  
  # Second derivative
  delta2[1] <- delta1[2] - delta1[1]
  delta2[n] <- delta1[n] - delta1[n-1]
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
  }
  
  # Third derivative
  delta3[1] <- delta2[2] - delta2[1]
  delta3[n] <- delta2[n] - delta2[n-1]
  for (i in 2:(n-1)) {
    delta3[i] <- (delta2[i+1] - delta2[i-1]) / 2
  }
  
  return(list(delta1=delta1, delta2=delta2, delta3=delta3))
}

# Compute with bad boundary conditions (original code)
compute_bad_boundaries <- function(M_p) {
  n <- length(M_p)
  delta1 <- numeric(n)
  delta2 <- numeric(n)
  delta3 <- numeric(n)
  
  # First derivative (same as proper)
  delta1[1] <- M_p[2] - M_p[1]
  delta1[n] <- M_p[n] - M_p[n-1]
  for (i in 2:(n-1)) {
    delta1[i] <- (M_p[i+1] - M_p[i-1]) / 2
  }
  
  # Second derivative with BAD boundaries
  delta2[1] <- -2*M_p[1] + M_p[2]  # Assumes M_p[0] = 0
  delta2[n] <- M_p[n-1] - 2*M_p[n]  # Assumes M_p[n+1] = M_p[n]
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
  }
  
  # Third derivative
  delta3[1] <- delta2[2] - delta2[1]
  delta3[n] <- delta2[n] - delta2[n-1]
  for (i in 2:(n-1)) {
    delta3[i] <- (delta2[i+1] - delta2[i-1]) / 2
  }
  
  return(list(delta1=delta1, delta2=delta2, delta3=delta3))
}

cat("================================================================================\n")
cat("GENERATING PUBLICATION PLOTS FOR LATEX DOCUMENT\n")
cat("================================================================================\n\n")

# ==============================================================================
# FIGURE 1: Three critical scenarios showing M_p behavior
# ==============================================================================
cat("Creating Figure 1: Critical scenarios A1, A2, A3...\n")

scenarios_fig1 <- c("A1_Baseline_Uncorrelated", "A2_Single_Predictor", "A3_Full_Support")
titles_fig1 <- c("A1: Baseline (p* = 3)", "A2: Single Predictor (p* = 1)", "A3: Full Support (p* = 20)")
true_p_fig1 <- c(3, 1, 20)

pdf("figure1_critical_scenarios.pdf", width=12, height=4)
par(mfrow=c(1,3), mar=c(4.5, 4.5, 3, 1), cex.lab=1.2, cex.axis=1.1, cex.main=1.3)

for (i in 1:3) {
  R2 <- load_scenario(scenarios_fig1[i])
  p_vals <- 1:length(R2)
  M_p <- R2 / p_vals
  
  plot(p_vals, M_p, type="l", lwd=3, col="blue",
       xlab="Number of components (p)", ylab=expression(M[p] == R^2/p),
       main=titles_fig1[i],
       ylim=c(0, max(M_p)*1.1))
  
  # Mark true p*
  abline(v=true_p_fig1[i], col="red", lty=2, lwd=2)
  points(true_p_fig1[i], M_p[true_p_fig1[i]], pch=19, col="red", cex=1.5)
  
  # Add grid
  grid(col="gray90", lty=1)
  
  # Add legend
  legend("topright", 
         legend=c(expression(M[p]), paste("True p* =", true_p_fig1[i])),
         col=c("blue", "red"), lty=c(1, 2), lwd=c(3, 2),
         bty="n", cex=1.1)
}

dev.off()
cat("  Saved: figure1_critical_scenarios.pdf\n\n")

# ==============================================================================
# FIGURE 2: Derivative structure for successful vs failed case
# ==============================================================================
cat("Creating Figure 2: Derivative structure comparison...\n")

pdf("figure2_derivative_structure.pdf", width=12, height=8)
par(mfrow=c(2,4), mar=c(4, 4.5, 3, 1), cex.lab=1.1, cex.axis=1.0, cex.main=1.2)

# Successful case: A1 (p* = 3)
R2_a1 <- load_scenario("A1_Baseline_Uncorrelated")
M_p_a1 <- R2_a1 / (1:length(R2_a1))
derivs_a1 <- compute_derivatives(M_p_a1)
p_vals <- 1:length(R2_a1)

plot(p_vals, M_p_a1, type="l", lwd=2, col="darkgreen",
     main="A1: Successful (p* = 3)", xlab="p", ylab=expression(M[p]))
abline(v=3, col="red", lty=2, lwd=2)
grid(col="gray90", lty=1)

plot(p_vals, derivs_a1$delta1, type="l", lwd=2, col="darkgreen",
     main=expression(Delta[1]), xlab="p", ylab="First Derivative")
abline(h=0, col="gray50", lty=1)
abline(v=3, col="red", lty=2, lwd=2)
grid(col="gray90", lty=1)

plot(p_vals, derivs_a1$delta2, type="l", lwd=2, col="darkgreen",
     main=expression(Delta[2]), xlab="p", ylab="Second Derivative")
abline(h=0, col="gray50", lty=1)
abline(v=3, col="red", lty=2, lwd=2)
grid(col="gray90", lty=1)

plot(p_vals, derivs_a1$delta3, type="l", lwd=2, col="darkgreen",
     main=expression(Delta[3]), xlab="p", ylab="Third Derivative")
abline(h=0, col="gray50", lty=1)
abline(v=3, col="red", lty=2, lwd=2)
abline(v=which.min(derivs_a1$delta3), col="blue", lty=3, lwd=2)
grid(col="gray90", lty=1)
legend("topright", c("True p*", "argmin"), col=c("red", "blue"), 
       lty=c(2, 3), lwd=2, bty="n", cex=0.9)

# Failed case: A2 (p* = 1)
R2_a2 <- load_scenario("A2_Single_Predictor")
M_p_a2 <- R2_a2 / (1:length(R2_a2))
derivs_a2 <- compute_derivatives(M_p_a2)

plot(p_vals, M_p_a2, type="l", lwd=2, col="darkred",
     main="A2: Failed (p* = 1)", xlab="p", ylab=expression(M[p]))
abline(v=1, col="red", lty=2, lwd=2)
grid(col="gray90", lty=1)

plot(p_vals, derivs_a2$delta1, type="l", lwd=2, col="darkred",
     main=expression(Delta[1]), xlab="p", ylab="First Derivative")
abline(h=0, col="gray50", lty=1)
abline(v=1, col="red", lty=2, lwd=2)
grid(col="gray90", lty=1)

plot(p_vals, derivs_a2$delta2, type="l", lwd=2, col="darkred",
     main=expression(Delta[2]), xlab="p", ylab="Second Derivative")
abline(h=0, col="gray50", lty=1)
abline(v=1, col="red", lty=2, lwd=2)
text(1, derivs_a2$delta2[1], sprintf("%.3f", derivs_a2$delta2[1]), 
     pos=3, col="darkred", cex=1.1, font=2)
grid(col="gray90", lty=1)

plot(p_vals, derivs_a2$delta3, type="l", lwd=2, col="darkred",
     main=expression(Delta[3]), xlab="p", ylab="Third Derivative")
abline(h=0, col="gray50", lty=1)
abline(v=1, col="red", lty=2, lwd=2)
abline(v=which.min(derivs_a2$delta3), col="blue", lty=3, lwd=2)
grid(col="gray90", lty=1)
legend("topright", c("True p*", "argmin"), col=c("red", "blue"), 
       lty=c(2, 3), lwd=2, bty="n", cex=0.9)

dev.off()
cat("  Saved: figure2_derivative_structure.pdf\n\n")

# ==============================================================================
# FIGURE 3: Impact of boundary condition choices (A2 scenario)
# ==============================================================================
cat("Creating Figure 3: Boundary condition artifacts...\n")

R2_a2 <- load_scenario("A2_Single_Predictor")
M_p_a2 <- R2_a2 / (1:length(R2_a2))

derivs_proper <- compute_derivatives(M_p_a2)
derivs_bad <- compute_bad_boundaries(M_p_a2)

pdf("figure3_boundary_artifacts.pdf", width=12, height=8)
par(mfrow=c(2,3), mar=c(4.5, 4.5, 3.5, 1), cex.lab=1.2, cex.axis=1.1, cex.main=1.3)

p_vals <- 1:length(R2_a2)

# Row 1: Proper boundaries
plot(p_vals, derivs_proper$delta1, type="l", lwd=2.5, col="darkgreen",
     main=expression("Proper Boundaries: " * Delta[1]),
     xlab="Number of components (p)", ylab="First Derivative")
abline(h=0, col="gray50", lty=1, lwd=1.5)
abline(v=1, col="red", lty=2, lwd=2)
grid(col="gray90", lty=1)

plot(p_vals, derivs_proper$delta2, type="l", lwd=2.5, col="darkgreen",
     main=expression("Proper Boundaries: " * Delta[2]),
     xlab="Number of components (p)", ylab="Second Derivative")
abline(h=0, col="gray50", lty=1, lwd=1.5)
abline(v=1, col="red", lty=2, lwd=2)
# Highlight boundary value
points(1, derivs_proper$delta2[1], pch=19, col="darkgreen", cex=1.8)
text(1, derivs_proper$delta2[1], sprintf("%.3f", derivs_proper$delta2[1]), 
     pos=3, col="darkgreen", cex=1.1, font=2)
grid(col="gray90", lty=1)

plot(p_vals, derivs_proper$delta3, type="l", lwd=2.5, col="darkgreen",
     main=expression("Proper Boundaries: " * Delta[3]),
     xlab="Number of components (p)", ylab="Third Derivative")
abline(h=0, col="gray50", lty=1, lwd=1.5)
abline(v=1, col="red", lty=2, lwd=2)
abline(v=which.min(derivs_proper$delta3), col="blue", lty=3, lwd=2)
grid(col="gray90", lty=1)
legend("topright", 
       legend=c("True p* = 1", paste("argmin =", which.min(derivs_proper$delta3))),
       col=c("red", "blue"), lty=c(2, 3), lwd=2, bty="n", cex=1.0)

# Row 2: Bad boundaries (assumes M_p[0] = 0)
plot(p_vals, derivs_bad$delta1, type="l", lwd=2.5, col="darkred",
     main=expression("Bad Boundaries: " * Delta[1]),
     xlab="Number of components (p)", ylab="First Derivative")
abline(h=0, col="gray50", lty=1, lwd=1.5)
abline(v=1, col="red", lty=2, lwd=2)
grid(col="gray90", lty=1)

plot(p_vals, derivs_bad$delta2, type="l", lwd=2.5, col="darkred",
     main=expression("Bad Boundaries (assumes " * M[p] * "[0]=0): " * Delta[2]),
     xlab="Number of components (p)", ylab="Second Derivative")
abline(h=0, col="gray50", lty=1, lwd=1.5)
abline(v=1, col="red", lty=2, lwd=2)
# Highlight WRONG boundary value
points(1, derivs_bad$delta2[1], pch=19, col="darkred", cex=1.8)
text(1, derivs_bad$delta2[1], sprintf("%.3f", derivs_bad$delta2[1]), 
     pos=1, col="darkred", cex=1.1, font=2)
# Show the artifact at p=19
if (which.min(derivs_bad$delta3) > 15) {
  points(which.min(derivs_bad$delta3), derivs_bad$delta2[which.min(derivs_bad$delta3)], 
         pch=4, col="purple", cex=2, lwd=3)
}
grid(col="gray90", lty=1)

plot(p_vals, derivs_bad$delta3, type="l", lwd=2.5, col="darkred",
     main=expression("Bad Boundaries: " * Delta[3]),
     xlab="Number of components (p)", ylab="Third Derivative")
abline(h=0, col="gray50", lty=1, lwd=1.5)
abline(v=1, col="red", lty=2, lwd=2)
abline(v=which.min(derivs_bad$delta3), col="blue", lty=3, lwd=2)
grid(col="gray90", lty=1)
legend("topright", 
       legend=c("True p* = 1", paste("argmin =", which.min(derivs_bad$delta3))),
       col=c("red", "blue"), lty=c(2, 3), lwd=2, bty="n", cex=1.0)

dev.off()
cat("  Saved: figure3_boundary_artifacts.pdf\n\n")

# ==============================================================================
# FIGURE 4: Success rate summary across all scenarios
# ==============================================================================
cat("Creating Figure 4: Success rate summary...\n")

scenarios <- c("A1", "A2", "A3", "B1W", "B1S", "B2", "B3", "C1", "C2", "C3")
true_p_vals <- c(3, 1, 20, 3, 3, 3, 3, 5, 10, 8)

# Results from Table 1
variant1 <- c(2, 19, 2, 2, 2, 2, 2, 2, 2, 2)
variant2 <- rep(2, 10)
variant3 <- rep(3, 10)

correct1 <- variant1 == true_p_vals
correct2 <- variant2 == true_p_vals
correct3 <- variant3 == true_p_vals

pdf("figure4_success_rates.pdf", width=10, height=6)
par(mar=c(5, 5, 4, 2), cex.lab=1.3, cex.axis=1.2, cex.main=1.4)

barplot_data <- rbind(
  as.numeric(correct1),
  as.numeric(correct2),
  as.numeric(correct3)
)

bp <- barplot(barplot_data, beside=TRUE, 
        names.arg=scenarios,
        col=c("coral", "skyblue", "lightgreen"),
        ylim=c(0, 1.2),
        ylab="Success (1 = correct, 0 = wrong)",
        xlab="Scenario",
        main="Model Selection Success Across Scenarios",
        border="black",
        las=1)

# Add success rate text
success_rates <- c(
  sprintf("0/10 (0%%)"),
  sprintf("0/10 (0%%)"),
  sprintf("5/10 (50%%)")
)

legend("topright", 
       legend=c("Variant 1: Current Code (bad boundaries)",
                "Variant 2: Proper Boundaries",
                paste("Variant 3: Proper + Index Correction")),
       fill=c("coral", "skyblue", "lightgreen"),
       border="black",
       bty="n",
       cex=1.1)

# Add horizontal line at 1.0
abline(h=1, col="darkgreen", lty=2, lwd=2)

# Add text annotations for success rates
text(mean(bp[1,]), 1.15, success_rates[1], col="coral", font=2, cex=1.1)
text(mean(bp[2,]), 1.15, success_rates[2], col="skyblue4", font=2, cex=1.1)
text(mean(bp[3,]), 1.15, success_rates[3], col="darkgreen", font=2, cex=1.1)

dev.off()
cat("  Saved: figure4_success_rates.pdf\n\n")

cat("================================================================================\n")
cat("All publication figures created successfully!\n")
cat("================================================================================\n")
cat("\nGenerated files:\n")
cat("  - figure1_critical_scenarios.pdf (3-panel: A1, A2, A3)\n")
cat("  - figure2_derivative_structure.pdf (2×4: successful vs failed)\n")
cat("  - figure3_boundary_artifacts.pdf (2×3: proper vs bad boundaries)\n")
cat("  - figure4_success_rates.pdf (success rate comparison)\n")
cat("\n")
