# Simple Example Script
# ======================
# A minimal example demonstrating the M_p metric

# Load functions
source("R/mp_functions.R")
source("R/visualization.R")
source("R/utils.R")

# Set seed for reproducibility
set.seed(123)

# Configuration
n <- 100          # Sample size
p_max <- 8        # Maximum predictors (reduced for faster execution)
sigma <- 0.3      # Noise level

# True model: only first 3 predictors are active
true_beta <- c(1.0, 0.8, 0.5, 0, 0, 0, 0, 0)
p_true <- 3

cat("\n=== M_p Model Selection Example ===\n\n")
cat("Generating data with:\n")
cat(sprintf("  - n = %d observations\n", n))
cat(sprintf("  - p_max = %d potential predictors\n", p_max))
cat(sprintf("  - p_true = %d active predictors\n", p_true))
cat(sprintf("  - sigma = %.2f noise level\n\n", sigma))

# Step 1: Generate data
cat("Step 1: Generating data...\n")
data <- generate_data(n, p_max, true_beta, sigma)
cat("  Done!\n\n")

# Step 2: Evaluate all models
cat(sprintf("Step 2: Evaluating all %d possible models...\n", 2^p_max - 1))
results <- evaluate_all_models(data$X, data$y, verbose = FALSE)
cat("  Done!\n\n")

# Step 3: Find optimal model
cat("Step 3: Finding optimal model...\n")
optimal <- find_optimal_model(results)
cat("  Done!\n\n")

# Display results
cat("=== RESULTS ===\n\n")
cat(sprintf("Optimal model complexity: p* = %d\n", optimal$p_star))
cat(sprintf("True model complexity:    p_true = %d\n", p_true))
cat(sprintf("Difference:               %+d\n\n", optimal$p_star - p_true))

cat("Best model details:\n")
cat(sprintf("  - Predictors: %s\n", optimal$optimal_model$subset_str))
cat(sprintf("  - R²:         %.4f\n", optimal$optimal_model$R2))
cat(sprintf("  - M_p:        %.4f\n\n", optimal$optimal_model$Mp))

# Show best models for each p
cat("Best models by cardinality:\n")
cat("-----------------------------\n")
best_models <- find_best_models_by_p(results)
print(best_models[, c("p", "R2", "Mp", "subset_str")])

cat("\n\n=== VISUALIZATION ===\n")

# Create and display plot
plot <- plot_mp_curves(results, optimal$p_star)
print(plot)

cat("\nAnalysis complete!\n")
cat("\nTo run the full analysis with all features, use:\n")
cat("  source('main_analysis.R')\n\n")
