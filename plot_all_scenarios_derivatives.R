# Plot derivative structure for all 10 scenarios
# Each scenario gets one PNG file with 4 panels in a row
# Panels: M_p, Δ₁, Δ₂, Δ₃

# Helper function to load scenario data
load_scenario <- function(scenario_name) {
  csv_path <- file.path("results", scenario_name, "detailed_results.csv")
  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  return(avg_data$R2)
}

# Compute derivatives using Approach 1 (via first derivative)
compute_derivatives <- function(M_p) {
  n <- length(M_p)
  
  # First derivative
  delta1 <- numeric(n)
  delta1[1] <- M_p[2] - M_p[1]  # forward
  for (i in 2:(n-1)) {
    delta1[i] <- (M_p[i+1] - M_p[i-1]) / 2  # central
  }
  delta1[n] <- M_p[n] - M_p[n-1]  # backward
  
  # Second derivative
  delta2 <- numeric(n)
  delta2[1] <- delta1[2] - delta1[1]
  for (i in 2:(n-1)) {
    delta2[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
  }
  delta2[n] <- delta1[n] - delta1[n-1]
  
  # Third derivative
  delta3 <- numeric(n)
  delta3[1] <- delta2[2] - delta2[1]
  for (i in 2:(n-1)) {
    delta3[i] <- (delta2[i+1] - delta2[i-1]) / 2
  }
  delta3[n] <- delta2[n] - delta2[n-1]
  
  return(list(delta1=delta1, delta2=delta2, delta3=delta3))
}

# Scenario definitions
scenarios <- data.frame(
  name = c("A1_Baseline_Uncorrelated", "A2_Single_Predictor", "A3_Full_Support",
           "B1_AR1_Weak", "B1_AR1_Strong", "B2_Compound_Symmetry", "B3_Block_Structure",
           "C1_Weak_Signals", "C2_Many_Weak_Signals", "C3_Mixed_Signals"),
  short_name = c("A1", "A2", "A3", "B1-Weak", "B1-Strong", "B2", "B3", "C1", "C2", "C3"),
  file_prefix = c("deriv_A1", "deriv_A2", "deriv_A3", 
                  "deriv_B1_Weak", "deriv_B1_Strong", "deriv_B2", "deriv_B3",
                  "deriv_C1", "deriv_C2", "deriv_C3"),
  p_true = c(3, 1, 20, 3, 3, 3, 3, 5, 10, 8),
  p_classical = c(3, 1, 1, 3, 1, 1, 1, 1, 1, 3),
  p_argmin = c(5, 3, 3, 2, 3, 3, 3, 3, 3, 5),
  classical_correct = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
  argmin_correct = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

# Create directory for derivative plots
dir.create("derivative_plots", showWarnings = FALSE)

cat("Creating derivative structure plots for all scenarios...\n\n")

for (i in seq_len(nrow(scenarios))) {
  scenario_name <- scenarios$name[i]
  short_name <- scenarios$short_name[i]
  file_prefix <- scenarios$file_prefix[i]
  p_true <- scenarios$p_true[i]
  p_classical <- scenarios$p_classical[i]
  p_argmin <- scenarios$p_argmin[i]
  classical_correct <- scenarios$classical_correct[i]
  argmin_correct <- scenarios$argmin_correct[i]
  
  cat(sprintf("Processing %s (p*=%d)...\n", short_name, p_true))
  
  # Load data
  R2 <- load_scenario(scenario_name)
  M_p <- R2 / seq_along(R2)
  derivs <- compute_derivatives(M_p)
  p_vals <- seq_along(R2)
  
  # Create PNG with 1x4 layout (one row, four columns)
  png(file.path("derivative_plots", paste0(file_prefix, ".png")), 
      width=16, height=4, units="in", res=300)
  
  par(mfrow=c(1, 4), mar=c(4.5, 4.5, 3.5, 1.5), cex.lab=1.2, cex.axis=1.1, cex.main=1.3)
  
  # Color for this scenario
  scenario_color <- if (classical_correct || argmin_correct) "darkgreen" else "darkred"
  
  # Panel 1: M_p curve
  plot(p_vals, M_p, type="l", lwd=3, col=scenario_color,
       main=expression(M[p] == R^2/p),
       xlab="p", 
       ylab=expression(M[p]))
  abline(v=p_true, col="red", lty=2, lwd=2.5)
  grid(col="gray90", lty=1)
  legend("topright", 
         legend=c(paste("p* =", p_true)),
         col="red", lty=2, lwd=2.5, bty="n", cex=1.1)
  
  # Panel 2: First derivative Δ₁
  plot(p_vals, derivs$delta1, type="l", lwd=3, col=scenario_color,
       main=expression(Delta[1]),
       xlab="p", 
       ylab=expression(Delta[1]))
  abline(h=0, col="gray50", lty=1, lwd=1)
  abline(v=p_true, col="red", lty=2, lwd=2.5)
  grid(col="gray90", lty=1)
  
  # Panel 3: Second derivative Δ₂
  plot(p_vals, derivs$delta2, type="l", lwd=3, col=scenario_color,
       main=expression(Delta[2]),
       xlab="p", 
       ylab=expression(Delta[2]))
  abline(h=0, col="gray50", lty=1, lwd=1.5)
  abline(v=p_true, col="red", lty=2, lwd=2.5)
  
  # Mark classical detection
  if (classical_correct) {
    points(p_classical, derivs$delta2[p_classical], pch=19, col="green3", cex=2)
  }
  
  # Check for sign changes
  sign_changes <- which(diff(sign(derivs$delta2)) != 0)
  if (length(sign_changes) > 0) {
    for (sc in sign_changes) {
      points(sc, derivs$delta2[sc], pch=21, bg="orange", col="black", cex=1.5, lwd=1.5)
    }
  }
  
  # Show boundary value at p=1
  text(1, derivs$delta2[1], sprintf("%.4f", derivs$delta2[1]), 
       pos=if(derivs$delta2[1] > 0) 3 else 1, col=scenario_color, cex=1.0, font=2)
  
  grid(col="gray90", lty=1)
  
  # Panel 4: Third derivative Δ₃
  plot(p_vals, derivs$delta3, type="l", lwd=3, col=scenario_color,
       main=expression(Delta[3]),
       xlab="p", 
       ylab=expression(Delta[3]))
  abline(h=0, col="gray50", lty=1, lwd=1)
  abline(v=p_true, col="red", lty=2, lwd=2.5)
  
  # Mark argmin(Δ₃)
  argmin_idx <- which.min(derivs$delta3)
  abline(v=argmin_idx, col="purple", lty=3, lwd=2.5)
  
  if (argmin_correct) {
    points(argmin_idx, derivs$delta3[argmin_idx], pch=19, col="purple", cex=2)
    text(argmin_idx, derivs$delta3[argmin_idx], 
         sprintf("argmin=%d", argmin_idx), 
         pos=1, col="purple", cex=1.0, font=2)
  } else {
    points(argmin_idx, derivs$delta3[argmin_idx], pch=4, col="purple", cex=1.8, lwd=2.5)
    text(argmin_idx, derivs$delta3[argmin_idx], 
         sprintf("argmin=%d", argmin_idx), 
         pos=1, col="purple", cex=0.9)
  }
  
  grid(col="gray90", lty=1)
  
  dev.off()
  cat(sprintf("  -> Saved: derivative_plots/%s.png\n", file_prefix))
}

cat("\n")
cat("====================================================================\n")
cat("SUCCESS: Created PNG files for all scenarios\n")
cat("====================================================================\n")
cat("Location: derivative_plots/\n")
cat("Files created:\n")
for (i in seq_len(nrow(scenarios))) {
  cat(sprintf("  - %s.png (Scenario %s, p*=%d)\n", 
              scenarios$file_prefix[i], scenarios$short_name[i], scenarios$p_true[i]))
}
cat("\n")
cat("Each PNG shows 4 panels in one row:\n")
cat("  [M_p] [Δ₁] [Δ₂] [Δ₃]\n")
cat("\n")
cat("Color coding:\n")
cat("  - Green lines: At least one method succeeds\n")
cat("  - Red lines: Both methods fail\n")
cat("  - Red dashed vertical: True p*\n")
cat("  - Purple markers: argmin(Δ₃)\n")
cat("  - Orange circles: Sign changes in Δ₂\n")
cat("  - Green dots: Successful classical detection\n")
cat("====================================================================\n")
