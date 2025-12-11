# Visualize the two best-performing metrics
# efficiency_ratio and info_gain both achieve 70% success

# Function to load and average scenario data
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

# Scenario names
scenarios <- c(
  "A1_Baseline_Uncorrelated",
  "A2_Single_Predictor", 
  "A3_Full_Support",
  "B1_AR1_Weak",
  "B1_AR1_Strong",
  "B2_Compound_Symmetry",
  "B3_Block_Structure",
  "C1_Weak_Signals",
  "C2_Many_Weak_Signals",
  "C3_Mixed_Signals"
)

true_p <- c(3, 1, 20, 3, 3, 3, 3, 5, 10, 8)

# Create plots for all scenarios
cat("================================================================================\n")
cat("VISUALIZING BEST METRICS: efficiency_ratio and info_gain\n")
cat("================================================================================\n\n")

for (idx in seq_along(scenarios)) {
  scenario <- scenarios[idx]
  p_star <- true_p[idx]
  
  cat("Processing:", scenario, "(p*=", p_star, ")\n")
  
  R2 <- load_scenario(scenario)
  n <- length(R2)
  p_vals <- 1:n
  
  # Compute three metrics
  M_p_original <- R2 / p_vals
  M_p_efficiency <- R2 / (p_vals * (1 - R2))
  M_p_info <- log(1 / (1 - R2)) / p_vals
  
  # Get derivatives for all three
  derivs_original <- compute_derivatives(M_p_original)
  derivs_efficiency <- compute_derivatives(M_p_efficiency)
  derivs_info <- compute_derivatives(M_p_info)
  
  # Selection results
  sel_original <- which.max(M_p_original)
  sel_efficiency <- which.max(M_p_efficiency)
  sel_info <- which.max(M_p_info)
  
  # Create PDF
  pdf(paste0("metric_comparison_", scenario, ".pdf"), width=14, height=10)
  par(mfrow=c(3, 4), mar=c(4, 4, 3, 1))
  
  # Row 1: Original (R²/p)
  plot(p_vals, M_p_original, type="l", lwd=2, col="blue",
       main=paste0("Original: R²/p\n", scenario, " (p*=", p_star, ")"),
       xlab="p", ylab="M_p")
  abline(v=p_star, col="red", lty=2, lwd=2)
  abline(v=sel_original, col="green", lty=3, lwd=2)
  legend("topright", c("True p*", paste0("Selected: ", sel_original)), 
         col=c("red", "green"), lty=c(2, 3), lwd=2, cex=0.8)
  
  plot(p_vals, derivs_original$delta1, type="l", lwd=2, col="blue",
       main="Δ₁", xlab="p", ylab="First Derivative")
  abline(h=0, col="gray", lty=2)
  abline(v=p_star, col="red", lty=2, lwd=2)
  
  plot(p_vals, derivs_original$delta2, type="l", lwd=2, col="blue",
       main="Δ₂", xlab="p", ylab="Second Derivative")
  abline(h=0, col="gray", lty=2)
  abline(v=p_star, col="red", lty=2, lwd=2)
  
  plot(p_vals, derivs_original$delta3, type="l", lwd=2, col="blue",
       main="Δ₃", xlab="p", ylab="Third Derivative")
  abline(h=0, col="gray", lty=2)
  abline(v=p_star, col="red", lty=2, lwd=2)
  
  # Row 2: Efficiency Ratio
  plot(p_vals, M_p_efficiency, type="l", lwd=2, col="purple",
       main=paste0("Efficiency Ratio: R²/(p*(1-R²))\nSelected: ", sel_efficiency, 
                   ifelse(sel_efficiency == p_star, " ✓", " ✗")),
       xlab="p", ylab="Metric")
  abline(v=p_star, col="red", lty=2, lwd=2)
  abline(v=sel_efficiency, col="green", lty=3, lwd=2)
  legend("topright", c("True p*", paste0("Selected: ", sel_efficiency)), 
         col=c("red", "green"), lty=c(2, 3), lwd=2, cex=0.8)
  
  plot(p_vals, derivs_efficiency$delta1, type="l", lwd=2, col="purple",
       main="Δ₁", xlab="p", ylab="First Derivative")
  abline(h=0, col="gray", lty=2)
  abline(v=p_star, col="red", lty=2, lwd=2)
  
  plot(p_vals, derivs_efficiency$delta2, type="l", lwd=2, col="purple",
       main="Δ₂", xlab="p", ylab="Second Derivative")
  abline(h=0, col="gray", lty=2)
  abline(v=p_star, col="red", lty=2, lwd=2)
  
  plot(p_vals, derivs_efficiency$delta3, type="l", lwd=2, col="purple",
       main="Δ₃", xlab="p", ylab="Third Derivative")
  abline(h=0, col="gray", lty=2)
  abline(v=p_star, col="red", lty=2, lwd=2)
  
  # Row 3: Info Gain
  plot(p_vals, M_p_info, type="l", lwd=2, col="darkgreen",
       main=paste0("Info Gain: log(1/(1-R²))/p\nSelected: ", sel_info,
                   ifelse(sel_info == p_star, " ✓", " ✗")),
       xlab="p", ylab="Metric")
  abline(v=p_star, col="red", lty=2, lwd=2)
  abline(v=sel_info, col="green", lty=3, lwd=2)
  legend("topright", c("True p*", paste0("Selected: ", sel_info)), 
         col=c("red", "green"), lty=c(2, 3), lwd=2, cex=0.8)
  
  plot(p_vals, derivs_info$delta1, type="l", lwd=2, col="darkgreen",
       main="Δ₁", xlab="p", ylab="First Derivative")
  abline(h=0, col="gray", lty=2)
  abline(v=p_star, col="red", lty=2, lwd=2)
  
  plot(p_vals, derivs_info$delta2, type="l", lwd=2, col="darkgreen",
       main="Δ₂", xlab="p", ylab="Second Derivative")
  abline(h=0, col="gray", lty=2)
  abline(v=p_star, col="red", lty=2, lwd=2)
  
  plot(p_vals, derivs_info$delta3, type="l", lwd=2, col="darkgreen",
       main="Δ₃", xlab="p", ylab="Third Derivative")
  abline(h=0, col="gray", lty=2)
  abline(v=p_star, col="red", lty=2, lwd=2)
  
  dev.off()
  cat("  Created: metric_comparison_", scenario, ".pdf\n\n", sep="")
}

cat("================================================================================\n")
cat("All visualization PDFs created successfully!\n")
cat("================================================================================\n")
