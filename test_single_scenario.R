#!/usr/bin/env Rscript
# Test Single Scenario
# ====================
# Usage: Rscript test_single_scenario.R <scenario_name> [N_iterations] [methods]

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat("\nUsage: Rscript test_single_scenario.R <scenario_name> [N_iterations] [methods]\n\n")
  cat("Available scenarios:\n")
  cat("  A1         - Baseline Uncorrelated\n")
  cat("  A2         - Single Predictor\n")
  cat("  A3         - Full Support\n")
  cat("  B1_weak    - AR1 Weak (ρ=0.5)\n")
  cat("  B1_strong  - AR1 Strong (ρ=0.8)\n")
  cat("  B2         - Compound Symmetry\n")
  cat("  B3         - Block Structure\n")
  cat("  C1         - Weak Signals\n")
  cat("  C2         - Many Weak Signals\n")
  cat("  C3         - Mixed Signals\n\n")
  cat("Examples:\n")
  cat("  Rscript test_single_scenario.R A1\n")
  cat("  Rscript test_single_scenario.R A1 100\n")
  cat("  Rscript test_single_scenario.R A1 50 sigmoid\n")
  cat("  Rscript test_single_scenario.R A1 50 derivative\n\n")
  quit(status = 1)
}

scenario_name <- args[1]
N_iterations <- ifelse(length(args) >= 2, as.integer(args[2]), 50)
methods <- ifelse(length(args) >= 3, args[3], "sigmoid")

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("TESTING SINGLE SCENARIO\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat(sprintf("Scenario:    %s\n", scenario_name))
cat(sprintf("Iterations:  %d\n", N_iterations))
cat(sprintf("Methods:     %s\n", methods))
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Load the experiment framework
source("run_experiment.R")

# Get all scenarios
scenarios <- define_scenarios()

# Check if scenario exists
if (!scenario_name %in% names(scenarios)) {
  cat(sprintf("ERROR: Scenario '%s' not found!\n\n", scenario_name))
  cat("Available scenarios:\n")
  cat(paste("  -", names(scenarios), collapse = "\n"))
  cat("\n\n")
  quit(status = 1)
}

# Get the scenario
scenario <- scenarios[[scenario_name]]

cat("Scenario details:\n")
cat(sprintf("  Name:         %s\n", scenario$name))
cat(sprintf("  Description:  %s\n", scenario$description))
cat(sprintf("  n:            %d\n", scenario$n))
cat(sprintf("  p:            %d\n", scenario$p))
cat(sprintf("  Support:      %s\n", as.character(scenario$support_spec)))
cat(sprintf("  Signal:       %s\n", scenario$signal_strength))
cat(sprintf("  Structure:    %s (ρ=%.1f)\n", scenario$sigma_structure, scenario$rho))
cat("\n")

# Run the scenario
result <- run_scenario(
  scenario = scenario,
  N_iterations = N_iterations,
  output_dir = "results",
  parallel_iterations = TRUE,
  n_cores = NULL,  # auto-detect
  methods = methods
)

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("RESULTS SUMMARY\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Print summary statistics
print(result$summary_stats)

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat(sprintf("✓ Results saved to: results/%s/\n", scenario$name))
cat(paste(rep("=", 70), collapse = ""), "\n\n")
