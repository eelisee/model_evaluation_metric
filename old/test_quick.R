# Quick Test: A1 Baseline with SMALL p and N for validation
# ===========================================================

source("run_experiment.R")

scenarios <- define_scenarios()

# Override with SMALL p for fast testing
test_scenario <- scenarios$A1
test_scenario$p <- 8  # Small p for quick test
test_scenario$name <- "A1_Test_Small"

cat("\n")
cat("======================================================================\n")
cat("QUICK TEST: Running with p=8, N=3 for validation\n")
cat("======================================================================\n\n")

# Run with 3 iterations
result <- run_scenario(test_scenario, N_iterations = 3, output_dir = "results_test")

cat("\n")
cat("======================================================================\n")
cat("Quick test complete! Check results_test/A1_Test_Small/\n")
cat("======================================================================\n")
