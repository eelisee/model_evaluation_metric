#!/usr/bin/env Rscript
# Test Subset Evaluation with Small N
# =====================================

# Set flag to prevent auto-execution when sourcing
.skip_auto_run <- TRUE

# Source all modules
source("R/01_data_generation.R")
source("R/02_metrics.R")
source("R/03_evaluation.R")
source("R/04_visualization.R")
source("run_experiment.R")

# Run small test
cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SUBSET EVALUATION TEST (N=3 iterations)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

scenarios <- define_scenarios()
result <- run_scenario(
  scenario = scenarios$A1,
  N_iterations = 3,
  output_dir = "test_small",
  parallel_iterations = FALSE
)

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("TEST COMPLETE!\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# List created files
output_dir <- "test_small/A1_Baseline_Uncorrelated"
cat("Output directory:", output_dir, "\n\n")

cat("Files created:\n")
files <- list.files(output_dir, recursive = FALSE)
for (f in files) cat("  ", f, "\n")

cat("\nPlots created:\n")
plots_dir <- file.path(output_dir, "plots")
if (dir.exists(plots_dir)) {
  plots <- sort(list.files(plots_dir, pattern = "\\.png$"))
  for (p in plots) {
    cat("  ", p)
    if (grepl("^09_|^10_", p)) cat(" [NEW SUBSET PLOT]")
    cat("\n")
  }
  cat("\nTotal:", length(plots), "plots\n")
}

cat("\nExpected new plots:\n")
cat("  09_subset_metrics.png\n")
cat("  10_subset_confusion_matrix.png\n\n")
