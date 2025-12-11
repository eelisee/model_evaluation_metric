# Deep Analysis: Finding the Exact Turning Point Formulation
# =============================================================
# The M_p curve DOES have a clear turning point visually.
# We need to find the EXACT mathematical formulation and indexing.

library(ggplot2)

# Load scenario data
scenario_name <- "A1_Baseline_Uncorrelated"
csv_path <- file.path("results", scenario_name, "detailed_results.csv")
data <- read.csv(csv_path, stringsAsFactors = FALSE)

# Average R² across iterations
avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
R2_vals <- avg_data$R2
p_vals <- avg_data$p
n <- length(p_vals)

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("DEEP ANALYSIS: M_p TURNING POINT\n")
cat(sprintf("Scenario: %s (true p* = 3)\n", scenario_name))
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Compute M_p
M_p <- R2_vals / p_vals

cat("Step 1: M_p values\n")
cat("==================\n\n")
for (i in 1:min(10, n)) {
  marker <- if (i == 3) " <<< TRUE p*" else ""
  cat(sprintf("p=%2d: M_p = %.8f%s\n", p_vals[i], M_p[i], marker))
}

cat("\n\nStep 2: First derivative (Forward Difference)\n")
cat("==============================================\n")
cat("Δ₁(p) = M(p+1) - M(p)\n\n")

delta1_forward <- diff(M_p)
for (i in 1:min(9, length(delta1_forward))) {
  marker <- if (i == 3 || i == 2) " <<<" else ""
  cat(sprintf("p=%2d→%2d: Δ₁ = %+.8f%s\n", p_vals[i], p_vals[i+1], delta1_forward[i], marker))
}

cat("\n\nStep 3: Second derivative (Central Difference)\n")
cat("===============================================\n")
cat("Δ₂(p) = M(p-1) - 2·M(p) + M(p+1)\n\n")

delta2_central <- numeric(n)
for (i in 2:(n-1)) {
  delta2_central[i] <- M_p[i-1] - 2*M_p[i] + M_p[i+1]
}
delta2_central[1] <- NA
delta2_central[n] <- NA

for (i in 1:min(10, n)) {
  if (!is.na(delta2_central[i])) {
    marker <- if (i == 3 || i == 2 || i == 4) " <<<" else ""
    sign_str <- if (delta2_central[i] < 0) "NEGATIVE (concave)" else "POSITIVE (convex)"
    cat(sprintf("p=%2d: Δ₂ = %+.8f  [%s]%s\n", p_vals[i], delta2_central[i], sign_str, marker))
  }
}

cat("\n\nStep 4: Third derivative\n")
cat("========================\n")
cat("Δ₃(p) = Δ₂(p+1) - Δ₂(p)\n\n")

delta3 <- diff(delta2_central)
for (i in 1:min(9, length(delta3))) {
  if (!is.na(delta3[i])) {
    marker <- if (i == 2 || i == 3) " <<<" else ""
    sign_str <- if (delta3[i] < 0) "DECREASING curvature" else "INCREASING curvature"
    cat(sprintf("p=%2d→%2d: Δ₃ = %+.8f  [%s]%s\n", p_vals[i], p_vals[i+1], delta3[i], sign_str, marker))
  }
}

# Analyze all possible selection rules
cat("\n\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("TESTING ALL POSSIBLE FORMULATIONS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

formulations <- list()

# Based on M_p directly
formulations[["max(M_p)"]] <- which.max(M_p)
formulations[["argmax before drop"]] <- which.max(M_p[-n]) # max before last

# Based on Δ₁ (first derivative)
formulations[["min(Δ₁)"]] <- which.min(delta1_forward)
formulations[["max(Δ₁)"]] <- which.max(delta1_forward)
formulations[["Δ₁ zero crossing"]] <- {
  idx <- NA
  for (i in 1:(length(delta1_forward)-1)) {
    if (delta1_forward[i] > 0 && delta1_forward[i+1] < 0) {
      idx <- i
      break
    }
  }
  idx
}
formulations[["max(|Δ₁|)"]] <- which.max(abs(delta1_forward))

# Based on Δ₂ (second derivative)
valid_delta2 <- which(!is.na(delta2_central))
formulations[["min(Δ₂)"]] <- valid_delta2[which.min(delta2_central[valid_delta2])]
formulations[["max(Δ₂)"]] <- valid_delta2[which.max(delta2_central[valid_delta2])]
formulations[["max(|Δ₂|)"]] <- valid_delta2[which.max(abs(delta2_central[valid_delta2]))]
formulations[["Δ₂ zero crossing -→+"]] <- {
  idx <- NA
  for (i in 1:(length(delta2_central)-1)) {
    if (!is.na(delta2_central[i]) && !is.na(delta2_central[i+1])) {
      if (delta2_central[i] < 0 && delta2_central[i+1] > 0) {
        idx <- i
        break
      }
    }
  }
  idx
}
formulations[["Δ₂ zero crossing +→-"]] <- {
  idx <- NA
  for (i in 1:(length(delta2_central)-1)) {
    if (!is.na(delta2_central[i]) && !is.na(delta2_central[i+1])) {
      if (delta2_central[i] > 0 && delta2_central[i+1] < 0) {
        idx <- i
        break
      }
    }
  }
  idx
}

# Based on Δ₃ (third derivative)
valid_delta3 <- which(!is.na(delta3))
formulations[["min(Δ₃)"]] <- valid_delta3[which.min(delta3[valid_delta3])]
formulations[["max(Δ₃)"]] <- valid_delta3[which.max(delta3[valid_delta3])]
formulations[["max(|Δ₃|)"]] <- valid_delta3[which.max(abs(delta3[valid_delta3]))]
formulations[["Δ₃ zero crossing -→+"]] <- {
  idx <- NA
  for (i in 1:(length(delta3)-1)) {
    if (!is.na(delta3[i]) && !is.na(delta3[i+1])) {
      if (delta3[i] < 0 && delta3[i+1] > 0) {
        idx <- i
        break
      }
    }
  }
  idx
}
formulations[["Δ₃ zero crossing +→-"]] <- {
  idx <- NA
  for (i in 1:(length(delta3)-1)) {
    if (!is.na(delta3[i]) && !is.na(delta3[i+1])) {
      if (delta3[i] > 0 && delta3[i+1] < 0) {
        idx <- i
        break
      }
    }
  }
  idx
}

# Index-shifted versions (the ±1 problem you mentioned!)
formulations[["min(Δ₃) + 1"]] <- valid_delta3[which.min(delta3[valid_delta3])] + 1
formulations[["min(Δ₃) - 1"]] <- valid_delta3[which.min(delta3[valid_delta3])] - 1
formulations[["max(Δ₂) + 1"]] <- valid_delta2[which.max(delta2_central[valid_delta2])] + 1
formulations[["max(Δ₂) - 1"]] <- valid_delta2[which.max(delta2_central[valid_delta2])] - 1

# Print results
cat(sprintf("%-30s | Selected p | Correct?\n", "Formulation"))
cat(paste(rep("-", 60), collapse = ""), "\n")

results <- data.frame(
  formulation = character(),
  p_selected = integer(),
  is_correct = logical(),
  stringsAsFactors = FALSE
)

for (name in names(formulations)) {
  idx <- formulations[[name]]
  if (is.na(idx) || idx < 1 || idx > n) {
    p_selected <- NA
    is_correct <- FALSE
    status <- "NA/Invalid"
  } else {
    p_selected <- p_vals[idx]
    is_correct <- (p_selected == 3)
    status <- if (is_correct) "✓ CORRECT" else sprintf("✗ Wrong (%d)", p_selected)
  }
  
  cat(sprintf("%-30s | %10s | %s\n", name, 
              ifelse(is.na(p_selected), "NA", as.character(p_selected)), 
              status))
  
  results <- rbind(results, data.frame(
    formulation = name,
    p_selected = ifelse(is.na(p_selected), -1, p_selected),
    is_correct = is_correct,
    stringsAsFactors = FALSE
  ))
}

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

correct_formulations <- results[results$is_correct, ]
if (nrow(correct_formulations) > 0) {
  cat("✓ Formulations that select p* = 3 correctly:\n\n")
  for (i in 1:nrow(correct_formulations)) {
    cat(sprintf("  %d. %s\n", i, correct_formulations$formulation[i]))
  }
} else {
  cat("✗ No formulation selected p* = 3 correctly!\n")
}

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Visual analysis: Create detailed plots
png("deep_analysis_A1_turning_point.png", width = 16, height = 10, units = "in", res = 300)
par(mfrow = c(2, 3), mar = c(4, 4.5, 3, 2))

# Plot 1: M_p with markers for all selection points
plot(p_vals, M_p, type = "b", pch = 19, col = "black", lwd = 2,
     xlab = "Model Size (p)", ylab = "M_p = R²/p",
     main = "M_p Curve with Selection Points", cex = 1.2)
abline(v = 3, col = "red", lwd = 3, lty = 1)
points(1, M_p[1], pch = 15, col = "blue", cex = 2)
points(2, M_p[2], pch = 15, col = "purple", cex = 2)
points(4, M_p[4], pch = 15, col = "orange", cex = 2)
legend("topright", 
       c("True p*=3", "p=1 (max M_p)", "p=2 (min Δ₃)", "p=4 (max Δ₂)"),
       col = c("red", "blue", "purple", "orange"),
       lty = c(1, NA, NA, NA),
       pch = c(NA, 15, 15, 15),
       lwd = c(3, NA, NA, NA))
grid()

# Plot 2: Δ₁
plot(p_vals[-n], delta1_forward, type = "b", pch = 19, col = "orange", lwd = 2,
     xlab = "p", ylab = "Δ₁(p) = M(p+1) - M(p)",
     main = "First Derivative")
abline(v = 3, col = "red", lwd = 2, lty = 2)
abline(h = 0, col = "gray", lty = 2)
grid()

# Plot 3: Δ₂
plot(p_vals, delta2_central, type = "b", pch = 19, col = "purple", lwd = 2,
     xlab = "p", ylab = "Δ₂(p)",
     main = "Second Derivative (Curvature)")
abline(v = 3, col = "red", lwd = 2, lty = 2)
abline(h = 0, col = "gray", lty = 2)
# Mark max
if (!is.na(formulations[["max(Δ₂)"]])) {
  points(formulations[["max(Δ₂)"]], delta2_central[formulations[["max(Δ₂)"]]], 
         pch = 17, col = "darkgreen", cex = 2)
}
grid()

# Plot 4: Δ₃
plot(p_vals[-n], delta3, type = "b", pch = 19, col = "red", lwd = 2,
     xlab = "p", ylab = "Δ₃(p)",
     main = "Third Derivative")
abline(v = 3, col = "red", lwd = 2, lty = 2)
abline(h = 0, col = "gray", lty = 2)
# Mark min
if (!is.na(formulations[["min(Δ₃)"]])) {
  points(formulations[["min(Δ₃)"]], delta3[formulations[["min(Δ₃)"]]], 
         pch = 17, col = "blue", cex = 2)
}
grid()

# Plot 5: Zoomed M_p (p=1 to 7)
zoom_range <- 1:7
plot(p_vals[zoom_range], M_p[zoom_range], type = "b", pch = 19, col = "black", lwd = 3,
     xlab = "p", ylab = "M_p",
     main = "M_p Curve (Zoomed: p=1 to 7)", cex = 1.5)
abline(v = 3, col = "red", lwd = 3)
# Add tangent lines to visualize curvature
segments(2, M_p[2], 3, M_p[3], col = "blue", lwd = 2, lty = 2)
segments(3, M_p[3], 4, M_p[4], col = "darkgreen", lwd = 2, lty = 2)
text(2.5, mean(c(M_p[2], M_p[3])), "slope 1", col = "blue", pos = 3)
text(3.5, mean(c(M_p[3], M_p[4])), "slope 2", col = "darkgreen", pos = 1)
grid()

# Plot 6: Rate of change visualization
plot(p_vals[-n], abs(delta1_forward), type = "b", pch = 19, col = "purple", lwd = 2,
     xlab = "p", ylab = "|Δ₁(p)|",
     main = "Absolute Rate of Change", log = "y")
abline(v = 3, col = "red", lwd = 2, lty = 2)
grid()

dev.off()

cat("✓ Detailed plots saved to: deep_analysis_A1_turning_point.png\n\n")
