# M_p Model Selection Framework - User Guide

## Overview

This framework provides a comprehensive empirical evaluation of the **M_p** model selection metric:

$$M_p = \frac{R^2_p}{p}$$

The M_p criterion uses an **inflection point method** based on discrete second derivatives to identify optimal model complexity, balancing goodness-of-fit with parsimony.

## Quick Start

### 1. Run a Single Scenario

```r
# Source the main script
source("run_experiment.R")

# Get scenario definitions
scenarios <- define_scenarios()

# Run baseline scenario with 100 iterations
result <- run_scenario(scenarios$A1, N_iterations = 100)
```

### 2. Run All Scenarios

```r
# Source the main script
source("run_experiment.R")

# Run all 11 scenarios with 100 iterations each
results <- run_all_scenarios(N_iterations = 100)
```

### 3. Check Results

```r
# Results are saved to:
# results/<scenario_name>/
#   ├── summary.txt
#   ├── summary_stats.csv
#   ├── detailed_results.csv
#   └── plots/
```

## Architecture

### Module Structure

```
metric/
├── run_experiment.R           # Main experiment orchestration
└── R/
    ├── 01_data_generation.R   # Data generation module
    ├── 02_metrics.R           # R² curve & M_p metric computation
    ├── 03_evaluation.R        # Performance evaluation & statistics
    └── 04_visualization.R     # Plotting functions
```

### Data Flow

```
Scenario Config
    ↓
Generate Data (X, y, β_true)
    ↓
Compute R² Curve (exhaustive best subset search for each p)
    ↓
Apply Metrics (M_p, AIC, BIC)
    ↓
Evaluate Performance (MAE, Hit Rate, TP/FP/FN, F1, etc.)
    ↓
Aggregate Statistics (across iterations)
    ↓
Create Plots & Save Results
```

## The 11 Scenarios

The framework tests M_p across three scenario categories:

### A) Baseline Scenarios

| Scenario | Description | Key Parameters |
|----------|-------------|----------------|
| **A1** | Baseline Uncorrelated | n=500, p=20, ρ=0, 3 active, strong signals |
| **A2** | Single Predictor | n=500, p=20, ρ=0, 1 active, strong signal |
| **A3** | Full Support | n=500, p=20, ρ=0, all 20 active, strong signals |

### B) Structural Scenarios (Correlation)

| Scenario | Description | Key Parameters |
|----------|-------------|----------------|
| **B1_weak** | Weak AR(1) Correlation | n=500, p=20, AR(1) ρ=0.5, 3 active |
| **B1_strong** | Strong AR(1) Correlation | n=500, p=20, AR(1) ρ=0.8, 3 active |
| **B2** | Compound Symmetry | n=500, p=20, exchangeable ρ=0.5, 3 active |
| **B3** | Block Structure | n=500, p=20, block ρ=0.7 (groups of 5), 3 active |

### C) Support Scenarios (Signal Strength)

| Scenario | Description | Key Parameters |
|----------|-------------|----------------|
| **C1** | Weak Signals | n=500, p=20, ρ=0, 5 weak signals |
| **C2** | Many Weak Signals | n=500, p=20, ρ=0, 10 weak signals |
| **C3** | Mixed Signals | n=500, p=20, ρ=0, 8 mixed strength signals |

## Output Structure

Each scenario creates a dedicated directory:

```
results/<scenario_name>/
├── summary.txt                  # Human-readable summary
├── summary_stats.csv            # Aggregated statistics
├── detailed_results.csv         # Full iteration-level results
└── plots/
    ├── r2_and_mp_curves.png     # R² and M_p progression
    ├── criterion_comparison.png # M_p vs AIC vs BIC
    ├── mp_inflection.png        # Inflection point detection
    ├── delta2_plot.png          # Second derivative visualization
    └── boxplot_errors.png       # Error distribution
```

### Key Output Files

**summary.txt** contains:
- Scenario configuration (n, p, correlation structure, support, signal strength)
- Selection performance (MAE, Bias, Variance, Hit Rate)
- Subset recovery metrics (mean TP, FP, FN, Precision, Recall, F1, Jaccard)
- Classification breakdown (% correct, underfit, overfit)
- Most frequent p* selections for each metric

**summary_stats.csv** aggregates:
- Selection accuracy per metric (M_p, AIC, BIC)
- Mean performance metrics across all iterations
- Average R², M_p values by model complexity

**detailed_results.csv** includes:
- `iteration`: Monte Carlo iteration number (1 to N)
- `p`: Model complexity (1 to p_max)
- `metric`: Selection criterion (M_p, AIC, BIC)
- `subset_p`: True active variables ordered by |β| magnitude
- `subset_Mp`: Variables in best M_p model at this complexity
- `subset_AIC`: Variables in best AIC model at this complexity
- `subset_BIC`: Variables in best BIC model at this complexity
- All performance metrics for each iteration

## Key Metrics

### Selection Accuracy
- **MAE** (Mean Absolute Error): Average |p* - p_true|
- **Bias**: Average (p* - p_true)
- **Variance**: Variance of p* across iterations
- **Hit Rate**: Proportion of iterations where p* = p_true

### Subset Recovery
- **TP** (True Positives): Correctly identified active variables
- **FP** (False Positives): Incorrectly identified inactive variables
- **FN** (False Negatives): Missed active variables
- **Precision**: TP / (TP + FP)
- **Recall**: TP / (TP + FN)
- **F1 Score**: Harmonic mean of precision and recall
- **Jaccard Index**: |intersection| / |union| of true and selected sets

### Classification
- **Correct**: p* = p_true
- **Underfit**: p* < p_true
- **Overfit**: p* > p_true

## Advanced Usage

### Custom Scenario

```r
# Define custom scenario
custom_scenario <- list(
  name = "Custom_Test",
  description = "Custom scenario for specific testing",
  n = 300,                      # Sample size
  p = 15,                       # Number of predictors
  sigma_structure = "ar1",      # "identity", "ar1", "compound", "block"
  rho = 0.6,                    # Correlation parameter
  support_spec = 5,             # Number of active variables (or "single"/"full")
  signal_strength = "strong",   # "strong", "weak", "mixed"
  sigma_eps = 0.3,              # Noise level
  seed = 123                    # Random seed (optional)
)

# Run with custom iterations
result <- run_scenario(custom_scenario, N_iterations = 50)
```

### Modify Existing Scenario

```r
# Start with baseline
scenarios <- define_scenarios()
my_scenario <- scenarios$A1

# Modify parameters
my_scenario$n <- 1000           # Increase sample size
my_scenario$sigma_eps <- 0.1    # Reduce noise
my_scenario$support_spec <- 5   # Change active variables

# Run modified scenario
result <- run_scenario(my_scenario, N_iterations = 100)
```

### Parallel vs Sequential Execution

```r
# Default: Parallel execution across iterations (faster)
result <- run_scenario(scenarios$A1, N_iterations = 100, 
                       parallel_iterations = TRUE)

# Sequential execution (useful for debugging)
result <- run_scenario(scenarios$A1, N_iterations = 100, 
                       parallel_iterations = FALSE)
```

### Custom Output Directory

```r
# Specify custom output location
result <- run_scenario(scenarios$A1, 
                       N_iterations = 100,
                       output_dir = "my_results")
```

## Selection Methods

### M_p Inflection Point Method

The M_p metric uses discrete second derivatives:

1. Compute M_p curve: $M_p = R^2_p / p$
2. First difference: $\Delta_1(p) = M_{p+1} - M_p$
3. Second difference: $\Delta_2(p) = \Delta_1(p+1) - \Delta_1(p)$
4. Select: $p^* = \arg\min_p \Delta_2(p)$

**Interpretation**: Identifies where M_p curve shows steepest decline (inflection point).

### AIC and BIC

For comparison, the framework also applies:
- **AIC**: $\text{AIC}_p = n \log(\text{RSS}_p / n) + 2p$
- **BIC**: $\text{BIC}_p = n \log(\text{RSS}_p / n) + p \log(n)$

Both select $p^* = \arg\min_p \text{Criterion}_p$

## Interpreting Results

### summary.txt Example

```
SCENARIO: A1_Baseline_Uncorrelated
Configuration:
  Sample size (n):           500
  Predictors (p):            20
  Correlation structure:     identity
  Support specification:     3
  Signal strength:           strong

SELECTION PERFORMANCE (N = 100 iterations)
────────────────────────────────────────────
Metric  MAE   Bias   Var   Hit Rate
M_p     0.85  -0.23  1.12  0.52
AIC     0.42   0.15  0.65  0.73
BIC     0.28  -0.08  0.31  0.85

SUBSET RECOVERY METRICS
────────────────────────────────────────────
Metric  Precision  Recall  F1     Jaccard
M_p     0.82      0.76    0.79   0.65
AIC     0.91      0.88    0.89   0.81
BIC     0.95      0.92    0.93   0.88
```

**Interpretation**:
- BIC has highest hit rate (85%) and best subset recovery (F1=0.93)
- M_p shows more variability (Var=1.12) but still performs reasonably
- All methods show slight negative bias (tend to underfit slightly)

## Computational Considerations

### Parallel Processing

The framework uses two levels of parallelization:

1. **Iteration-level** (default): Multiple Monte Carlo runs in parallel
   - Controlled by `parallel_iterations` parameter
   - Uses `parallel::mclapply()` on Mac/Linux

2. **Subset-level**: Best subset search parallelized across model sizes
   - Automatically used in `compute_r2_curve()`
   - Adjustable via `n_cores` parameter

### Runtime Estimates

For p=20 predictors (1,048,576 total models):
- **Single iteration**: ~5-10 seconds (with parallelization)
- **100 iterations** (parallel): ~2-5 minutes
- **All 11 scenarios** (100 iterations each): ~30-60 minutes

### Memory Requirements

- **p=20**: Moderate (~1-2 GB RAM)
- **p=25**: High (~10-15 GB RAM) - not recommended
- **p>25**: Not feasible with exhaustive search

## Troubleshooting

### Error: "Cannot allocate vector of size..."
**Issue**: Insufficient memory for exhaustive search  
**Solution**: Reduce p or increase system RAM

### Long execution time
**Issue**: Exhaustive search is computationally intensive  
**Solutions**:
- Use fewer iterations for testing
- Enable parallel processing (default)
- Reduce p (number of predictors)

### Plots not generating
**Issue**: Directory permissions or graphics device error  
**Solution**: Check write permissions in output directory

### Different results across runs
**Issue**: Random variation in data generation  
**Solutions**:
- Set `seed` parameter in scenario for reproducibility
- Increase `N_iterations` for more stable estimates

## Best Practices

1. **Start small**: Test with N_iterations=10 before full run
2. **Use parallelization**: Keep `parallel_iterations=TRUE` (default)
3. **Organize output**: Use descriptive `output_dir` names
4. **Document modifications**: Add notes to scenario descriptions
5. **Check convergence**: Review variance in summary statistics

## License

MIT License

## Author

Created: November 2025