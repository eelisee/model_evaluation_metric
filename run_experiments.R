# Main Script for Running M_p Experiments
# =========================================
# This script demonstrates how to use the modular experiment framework

# Source all required modules
source("R/data_generation.R")
source("R/model_evaluation.R")
source("R/selection_rules.R")
source("R/recovery_metrics.R")
source("R/scenarios.R")
source("R/io_utilities.R")
source("R/visualization.R")
source("R/experiment_runner.R")

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("M_p EXPERIMENT FRAMEWORK - MAIN SCRIPT\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# =============================================================================
# OPTION 1: Run a single scenario
# =============================================================================

run_single_scenario <- function(scenario_id = "S2", n_reps = 1, seed = 123) {
  cat(sprintf("Running scenario %s...\n\n", scenario_id))
  
  # Get scenario configuration based on ID
  scenarios <- get_all_scenarios()
  
  # Map scenario_id to scenario function
  scenario_map <- list(
    S1 = "s1",
    S2 = "s2", 
    S3 = "s3",
    S4 = "s4",
    S5 = "s5",
    S6 = "s6_ar1",
    S7 = "s7_snr",
    S8 = "s8_nonlin",
    S9 = "s9_interact",
    S10 = "s10_redund",
    S11 = "s11_meas",
    S12 = "s12_tdist",
    S13 = "s13_hetero",
    S14 = "s14_group"
  )
  
  scenario_key <- scenario_map[[scenario_id]]
  if (is.null(scenario_key)) {
    stop(sprintf("Unknown scenario: %s. Valid scenarios: S1-S14", scenario_id))
  }
  
  config <- scenarios[[scenario_key]]
  config$seed <- seed
  
  if (n_reps == 1) {
    # Run single experiment
    result <- run_experiment(
      config = config,
      output_dir = "results",
      save_data = TRUE,
      save_plots = TRUE,
      verbose = TRUE
    )
    
    cat("\nResults saved to:", result$exp_dir, "\n")
    return(result)
    
  } else {
    # Run multiple repetitions
    scenarios_list <- list()
    scenarios_list[[scenario_key]] <- config
    
    results <- run_batch_experiments(
      scenarios = scenarios_list,
      n_reps = n_reps,
      output_dir = "results",
      verbose = TRUE
    )
    
    return(results)
  }
}

# =============================================================================
# OPTION 2: Run multiple scenarios (batch mode)
# =============================================================================

run_batch_scenarios <- function(n_reps = 1) {
  cat("Running batch experiments...\n\n")
  
  # Select scenarios to run
  scenarios <- list(
    s1 = scenario_s1_constant(seed = 100),
    s2 = scenario_s2_baseline(seed = 200),
    s3 = scenario_s3_random_order(seed = 300)
  )
  
  # Run batch
  results <- run_batch_experiments(
    scenarios = scenarios,
    n_reps = n_reps,
    output_dir = "results",
    verbose = TRUE
  )
  
  return(results)
}

# =============================================================================
# OPTION 3: Run all 14 scenarios
# =============================================================================

run_all_scenarios <- function(n_reps = 1) {
  cat("Running all 14 scenarios...\n\n")
  
  # Get all scenario configurations
  scenarios <- get_all_scenarios()
  
  # Run batch
  results <- run_batch_experiments(
    scenarios = scenarios,
    n_reps = n_reps,
    output_dir = "results",
    verbose = TRUE
  )
  
  return(results)
}

# =============================================================================
# OPTION 4: Quick test
# =============================================================================

run_quick_test <- function() {
  cat("Running quick test...\n\n")
  result <- quick_test("s2")
  return(result)
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

if (interactive()) {
  cat("Interactive mode detected.\n")
  cat("To run experiments, use one of these commands:\n\n")
  cat("  result <- run_single_scenario()              # Single scenario (S2, default)\n")
  cat("  result <- run_single_scenario('S1', n_reps=5) # Scenario S1, 5 repetitions\n")
  cat("  results <- run_batch_scenarios(3)            # S1-S3, 3 reps each\n")
  cat("  results <- run_all_scenarios(5)              # All 14 scenarios, 5 reps\n")
  cat("  result <- run_quick_test()                   # Quick test\n\n")
  
} else {
  # Non-interactive: run from command line with arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    # Default: show usage message
    cat("No command-line arguments provided.\n")
    cat("Use one of these commands:\n\n")
    cat("  Rscript run_experiments.R single       # Single scenario (S2)\n")
    cat("  Rscript run_experiments.R batch [N]    # S1-S3, N reps each\n")
    cat("  Rscript run_experiments.R all [N]      # All 14 scenarios, N reps\n")
    cat("  Rscript run_experiments.R test         # Quick test\n\n")
    cat("Or source the script in R and call functions directly:\n")
    cat("  source('run_experiments.R')\n")
    cat("  result <- run_all_scenarios(n_reps = 1)\n\n")
    
  } else if (args[1] == "single") {
    result <- run_single_scenario()
    
  } else if (args[1] == "batch") {
    n_reps <- if (length(args) > 1) as.integer(args[2]) else 1
    results <- run_batch_scenarios(n_reps)
    
  } else if (args[1] == "all") {
    n_reps <- if (length(args) > 1) as.integer(args[2]) else 1
    results <- run_all_scenarios(n_reps)
    
  } else if (args[1] == "test") {
    result <- run_quick_test()
    
  } else {
    cat("Unknown command:", args[1], "\n")
    cat("Valid commands: single, batch, all, test\n")
  }
}

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("Script complete. Check results/ directory for outputs.\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
