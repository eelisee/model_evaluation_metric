# M_p Model Selection Framework - User Guide

## Overview

This is a comprehensive, modular framework for evaluating the **M_p** model selection metric:

$$M_p = \frac{R^2}{p}$$

The framework systematically tests M_p across 15 stress-test scenarios, compares multiple selection rules, and provides detailed performance metrics.

## Quick Start

### 1. Run a Quick Test

```r
# Source the main script
source("run_experiments.R")

# Run a quick test on baseline scenario
result <- run_quick_test()
```

### 2. Run Single Scenario

```r
# Run the baseline scenario (S2)
result <- run_single_scenario()

# Check where results were saved
print(result$exp_dir)
```

### 3. Run Multiple Scenarios

```r
# Run scenarios S1-S3 with 3 repetitions each
results <- run_batch_scenarios(n_reps = 3)
```

### 4. Run All 15 Scenarios

```r
# Run complete test battery (5 reps per scenario = 75 experiments)
results <- run_all_scenarios(n_reps = 5)
```

## Architecture

### Module Structure

```
R/
├── data_generation.R      # Advanced data generation
├── model_evaluation.R     # Model enumeration and evaluation
├── selection_rules.R      # Multiple selection rules
├── recovery_metrics.R     # Performance evaluation
├── scenarios.R            # 15 scenario configurations
├── io_utilities.R         # I/O and file management
├── visualization.R        # Plotting functions
└── experiment_runner.R    # Experiment orchestration
```

### Data Flow

```
Config → Generate Data → Enumerate Models → Evaluate Models
  ↓           ↓              ↓                   ↓
Save     Save to RData   Model list         All metrics
meta.json                                   (R², M_p, AIC, BIC)
                              ↓                   ↓
                        Apply Rules         Compute Recovery
                        (A, B, C)          (TP/FP/FN, F1)
                              ↓                   ↓
                        Save results       Generate plots
                        to CSV             Save to plots/
                              ↓
                        Summary report
                        summary.txt
```

## The 15 Scenarios

| ID | Scenario | Description | Expected Behavior |
|----|----------|-------------|-------------------|
| S1 | Constant M_p | All M_p values identical | Rule A should fail (degeneracy) |
| S2 | Baseline | Standard conditions | Should recover true p* |
| S3 | Random Order | Shuffled coefficients | Order-invariance test |
| S4 | Heteroscedastic Predictors | Varying variances | Robustness to scale |
| S5 | Non-zero Means | Shifted predictors | Centering test |
| S6 | Collinearity | Correlated predictors | Instability expected |
| S7 | Weak Signal | Low SNR | Under-selection risk |
| S8 | High-Dimensional | p_max = 50, p_true = 5 | Computational stress |
| S9 | Nonlinear | Squared terms | Model misspecification |
| S10 | Interactions | X1*X2 terms | Feature engineering |
| S11 | Redundant Variables | Duplicate predictors | Over-selection risk |
| S12 | Measurement Error | Noisy predictors | Attenuation bias |
| S13 | Non-Gaussian | Heavy-tailed errors | Distribution robustness |
| S14 | Heteroscedastic Errors | Varying noise | Efficiency loss |
| S15 | Group Sparsity | Correlated blocks | Group selection |

## Selection Rules

### Rule A: Maximum M_p
- **Method:** Select p that maximizes M_p
- **Strength:** Simple, interpretable
- **Weakness:** Can fail with constant M_p

### Rule B: Steep Drop
- **Method:** Detect largest drop in M_p curve
- **Strength:** Robust to noise
- **Weakness:** May over-select if no clear drop

### Rule C: Elbow Detection
- **Method:** Find "elbow" in M_p vs p curve
- **Strength:** Balances parsimony and fit
- **Weakness:** Subjective definition of "elbow"

### Comparisons
- **AIC:** Akaike Information Criterion
- **BIC:** Bayesian Information Criterion  
- **Adj R²:** Adjusted R-squared

## Output Structure

Each experiment creates a timestamped directory:

```
results/
└── s2__baseline__2024-01-15_14-30-45/
    ├── meta.json                    # Configuration metadata
    ├── data.RData                   # Generated data (X, y, true_beta)
    ├── models_full.csv              # All evaluated models
    ├── best_models_by_p.csv         # Best model per complexity
    ├── recovery_stats.csv           # TP/FP/FN/Precision/Recall/F1
    ├── selection_comparison.csv     # All rules compared
    ├── ranking_correlations.csv     # M_p vs R² correlation
    ├── aggregate_by_p.csv           # Statistics by complexity
    ├── summary.txt                  # Human-readable summary
    ├── plots/
    │   ├── mp_and_r2_curves.png
    │   ├── mp_efficiency_curve.png
    │   ├── r2_curve.png
    │   ├── all_models_scatter.png
    │   └── criterion_comparison.png
    └── diagnostics/
```

## Key Metrics

### Model Evaluation
- **R²:** Coefficient of determination
- **Adjusted R²:** Penalized for model complexity
- **M_p:** Efficiency metric (R²/p)
- **AIC:** Akaike Information Criterion
- **BIC:** Bayesian Information Criterion

### Variable Selection Performance
- **TP/FP/FN:** True/False Positives/Negatives
- **Precision:** TP / (TP + FP)
- **Recall:** TP / (TP + FN)
- **F1 Score:** Harmonic mean of precision and recall
- **Hamming Distance:** Number of variable mismatches
- **Exact Match:** Boolean for perfect recovery

### Stability Analysis
- **Selection Stability:** Consistency across repetitions
- **Ranking Correlations:** Agreement between metrics

## Advanced Usage

### Custom Scenario

```r
# Create custom configuration
custom_config <- list(
  scenario_name = "my_scenario",
  scenario_description = "Custom test case",
  n = 200,                    # Sample size
  p_max = 30,                 # Number of predictors
  p_true = 5,                 # True active predictors
  true_indices = 1:5,         # Which predictors are active
  beta_true = c(2, -1.5, 1, -0.5, 0.8),  # True coefficients
  sigma_eps = 1.0,            # Noise level
  X_dist = "normal",          # Predictor distribution
  correlation_structure = "identity",
  rho = 0,
  enumeration_strategy = "full",
  seed = 999,
  expected_behavior = "Should work well"
)

# Run experiment
result <- run_experiment(custom_config)
```

### Modify Existing Scenario

```r
# Start with baseline
config <- scenario_s2_baseline()

# Modify parameters
config$n <- 500              # Increase sample size
config$sigma_eps <- 0.5      # Reduce noise
config$seed <- 456

# Run modified scenario
result <- run_experiment(config)
```

### Parameter Sweeps

```r
# Sweep over SNR values
snr_values <- c(0.5, 1.0, 2.0, 5.0)
results <- list()

for (snr in snr_values) {
  config <- scenario_s2_baseline()
  config$sigma_eps <- 1.0 / sqrt(snr)  # Adjust noise for desired SNR
  config$scenario_name <- paste0("s2_snr_", snr)
  
  results[[as.character(snr)]] <- run_experiment(config, verbose = FALSE)
}

# Compare F1 scores
f1_scores <- sapply(results, function(r) {
  r$recovery[r$recovery$rule == "max_mp", "F1"]
})

plot(snr_values, f1_scores, type = "b",
     xlab = "SNR", ylab = "F1 Score",
     main = "Rule A Performance vs SNR")
```

## Command-Line Usage

```bash
# Run single scenario
Rscript run_experiments.R single

# Run batch of 3 scenarios, 5 reps each
Rscript run_experiments.R batch 5

# Run all 15 scenarios, 10 reps each  
Rscript run_experiments.R all 10

# Quick test
Rscript run_experiments.R test
```

## Loading Results

```r
# Load results from saved experiment
exp_dir <- "results/s2__baseline__2024-01-15_14-30-45"
saved_results <- load_experiment_results(exp_dir)

# Access components
print(saved_results$meta)           # Configuration
print(saved_results$recovery)       # Recovery stats
print(saved_results$models)         # All models

# Re-generate plots
plot_mp_curves(saved_results$models)
```

## Interpreting Results

### Good Performance Indicators
- ✓ F1 score > 0.8
- ✓ Exact match = TRUE
- ✓ |p* - p_true| ≤ 1
- ✓ Low Hamming distance
- ✓ High precision and recall

### Warning Signs
- ⚠ M_p curve is degenerate
- ⚠ Large gap between p* and p_true
- ⚠ Low F1 score (< 0.5)
- ⚠ Many false positives or false negatives
- ⚠ Unstable selections across repetitions

### Summary Report
Always check `summary.txt` for:
1. Configuration parameters
2. Selection results (all rules)
3. Recovery metrics
4. Expected vs actual behavior
5. Interpretation notes

## Troubleshooting

### Error: "package 'jsonlite' not available"
```r
install.packages("jsonlite")
```

### Error: "cannot open file 'R/xxx.R'"
Make sure you're in the project root directory:
```r
setwd("/path/to/metric")
source("run_experiments.R")
```

### Plots not generating
Check if ggplot2 is installed (optional, falls back to base R):
```r
install.packages("ggplot2")
```

### Out of memory with high-dimensional scenarios
Use limited enumeration:
```r
config <- scenario_s8_high_dimensional()
config$enumeration_strategy <- "cardinality_limited"
config$max_p <- 10  # Limit model complexity
result <- run_experiment(config)
```

## Best Practices

1. **Start small:** Test with S2 (baseline) before running all scenarios
2. **Use repetitions:** Run n_reps ≥ 5 for stability analysis
3. **Check diagnostics:** Always review summary.txt and plots
4. **Save configurations:** Keep meta.json for reproducibility
5. **Compare rules:** Don't rely on single selection rule
6. **Validate assumptions:** Check if scenario assumptions hold

## Performance Notes

- **Full enumeration:** Feasible up to p_max ≈ 20 (2^20 ≈ 1M models)
- **Cardinality-limited:** Use for p_max > 20, set reasonable max_p
- **Random sampling:** For very high dimensions (p_max > 50)
- **Parallel processing:** Not yet implemented (future enhancement)

## References

For methodology details, see:
- `IMPLEMENTATION_STATUS.md` - Technical specifications
- `README.md` - Original project overview
- `QUICKSTART.md` - Basic usage examples

## Contact

For issues, questions, or contributions, please open an issue on GitHub.
