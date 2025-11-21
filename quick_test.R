#!/usr/bin/env Rscript
# Quick Test of Subset Evaluation (N=3 iterations)

# Prevent run_experiment.R from auto-executing by setting a flag
.test_mode <- TRUE

# Source all files
source("R/01_data_generation.R")
source("R/02_metrics.R")
source("R/03_evaluation.R")
source("R/04_visualization.R")

# Manually load just the functions we need
source_lines <- readLines("run_experiment.R")
# Remove the auto-execution part (last ~20 lines)
source_lines <- source_lines[1:(length(source_lines) - 20)]
eval(parse(text = source_lines))

# Run test
cat("\n=== Testing Subset Evaluation (N=3 iterations) ===\n\n")

scenarios <- define_scenarios()
result <- run_scenario(
  scenario = scenarios$A1,
  N_iterations = 3,
  output_dir = "test_case_subset",
  parallel_iterations = FALSE
)

cat("\n=== TEST COMPLETE ===\n")
cat("\nOutput directory: test_case_subset/A1_Baseline_Uncorrelated/\n\n")

# List created plots
plots_dir <- "test_case_subset/A1_Baseline_Uncorrelated/plots"
if (dir.exists(plots_dir)) {
  plots <- sort(list.files(plots_dir, pattern = "\\.png$"))
  cat("Created", length(plots), "plots:\n")
  for (p in plots) {
    cat("  ", p, "\n")
  }
}
