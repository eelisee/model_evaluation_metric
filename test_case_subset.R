# Quick Test Case for Subset Evaluation
# ========================================
# Minimal test with N=3 iterations to verify plots are created

# Load modules
source("R/01_data_generation.R")
source("R/02_metrics.R")
source("R/03_evaluation.R")
source("R/04_visualization.R")

# Load functions from run_experiment.R without executing
env <- new.env()
suppressWarnings(sys.source("run_experiment.R", envir = env))

# Test with A1 scenario, N=3 iterations, sequential
cat("\n=== Testing Subset Evaluation with N=3 iterations ===\n\n")

scenarios <- env$define_scenarios()
result <- env$run_scenario(
  scenario = scenarios$A1,
  N_iterations = 3,
  output_dir = "test_case_subset",
  parallel_iterations = FALSE
)

cat("\n=== TEST COMPLETE ===\n")
cat("\nCheck output in: test_case_subset/A1_Baseline_Uncorrelated/\n")
cat("Plots should include:\n")
cat("  - plots/09_subset_metrics.png (NEW)\n")
cat("  - plots/10_subset_confusion_matrix.png (NEW)\n\n")

# List the plots created
plots_dir <- "test_case_subset/A1_Baseline_Uncorrelated/plots"
if (dir.exists(plots_dir)) {
  plots <- list.files(plots_dir, pattern = "\\.png$")
  cat("Created plots:\n")
  for (p in plots) cat("  -", p, "\n")
  cat("\nTotal:", length(plots), "plots created\n")
}
