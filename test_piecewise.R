#!/usr/bin/env Rscript
# Test piecewise linear model

source("R/02_metrics_piecewise.R")

# A2 data (p*=1)
mp_a2 <- c(0.9488, 0.4746, 0.3165, 0.2375, 0.1900, 0.1584, 0.1358, 0.1188, 0.1056, 0.0951,
           0.0872, 0.0807, 0.0754, 0.0710, 0.0672, 0.0640, 0.0611, 0.0586, 0.0563, 0.0543)
p_vals <- 1:20
r2_a2 <- mp_a2 * p_vals

# A1 data (p*=3)
mp_a1 <- c(0.4999, 0.4370, 0.3301, 0.2476, 0.1981, 0.1651, 0.1415, 0.1238, 0.1101, 0.0991,
           0.0901, 0.0826, 0.0763, 0.0708, 0.0661, 0.0620, 0.0583, 0.0551, 0.0522, 0.0496)
r2_a1 <- mp_a1 * p_vals

cat("=== A2 SCENARIO (true p*=1) ===\n\n")
r2_curve_a2 <- data.frame(p = p_vals, R2 = r2_a2, subset_Mp = NA)
result_a2 <- metric_piecewise_mp(r2_curve_a2)

cat(sprintf("Selected p*: %d\n", result_a2$p_star))
cat(sprintf("RMSE: %.6f\n", result_a2$rmse))
cat(sprintf("Slope before p*: %.4f\n", result_a2$params["b1"]))
cat(sprintf("Slope after p*: %.4f\n", result_a2$params["b2"]))
cat(sprintf("\nError: MAE = %d\n", abs(result_a2$p_star - 1)))

cat("\n=== A1 SCENARIO (true p*=3) ===\n\n")
r2_curve_a1 <- data.frame(p = p_vals, R2 = r2_a1, subset_Mp = NA)
result_a1 <- metric_piecewise_mp(r2_curve_a1)

cat(sprintf("Selected p*: %d\n", result_a1$p_star))
cat(sprintf("RMSE: %.6f\n", result_a1$rmse))
cat(sprintf("Slope before p*: %.4f\n", result_a1$params["b1"]))
cat(sprintf("Slope after p*: %.4f\n", result_a1$params["b2"]))
cat(sprintf("\nError: MAE = %d\n", abs(result_a1$p_star - 3)))

cat("\n=== Comparison ===\n")
cat(sprintf("A2: Piecewise selects p*=%d (MAE=%d) vs Sigmoid p*=2 (MAE=1)\n", 
            result_a2$p_star, abs(result_a2$p_star - 1)))
cat(sprintf("A1: Piecewise selects p*=%d (MAE=%d) vs Sigmoid p*=4 (MAE=1)\n", 
            result_a1$p_star, abs(result_a1$p_star - 3)))
