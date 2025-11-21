# M_p Model Selection Framework

## Overview

This project implements and empirically investigates the **M_p** model selection metric, which balances model fit and complexity through an inflection point detection method. The metric is defined as:

$$M_p = \frac{R^2_p}{p}$$

where:
- $R^2_p$ is the coefficient of determination for the best model with $p$ predictors
- $p$ is the number of active predictors in the model

The M_p metric expresses the **explained variance per parameter**, quantifying how efficiently each predictor contributes to explanatory power.

## Theoretical Background

### Model Setting

We assume a linear regression model:

$$y = X\beta + \varepsilon$$

where:
- $y \in \mathbb{R}^n$: response vector
- $X \in \mathbb{R}^{n \times p}$: design matrix with $p$ potential predictors
- $\beta \in \mathbb{R}^{p}$: coefficient vector
- $\varepsilon \sim \mathcal{N}(0, \sigma^2 I_n)$: normally distributed noise

### R² Definition

For any model with predictor subset of size $p$:

$$R^2_p = 1 - \frac{RSS_p}{TSS}$$

where:
- $RSS_p = \sum_{i=1}^{n} (y_i - \hat{y}_{p,i})^2$ (residual sum of squares)
- $TSS = \sum_{i=1}^{n} (y_i - \bar{y})^2$ (total sum of squares)

### Selection Strategy

The framework implements an **M_p inflection point method** using discrete second derivatives:

$$\Delta_1(p) = M_{p+1} - M_p \quad \text{(first difference)}$$
$$\Delta_2(p) = \Delta_1(p+1) - \Delta_1(p) \quad \text{(second difference)}$$
$$p^* = \arg\min_p \Delta_2(p) \quad \text{(inflection point)}$$

This identifies the model complexity $p^*$ where the efficiency curve shows the steepest decline, representing the optimal trade-off between explanatory power and parsimony.

For comparison, the framework also evaluates AIC and BIC selection criteria.

## Project Structure

```
metric/
├── README.md                    # This file
├── USER_GUIDE.md                # Detailed usage guide
├── run_experiment.R             # Main experiment runner
├── R/                           # Core modules
│   ├── 01_data_generation.R     # Data generation with various scenarios
│   ├── 02_metrics.R             # R² curve computation and M_p metric
│   ├── 03_evaluation.R          # Performance evaluation and statistics
│   └── 04_visualization.R       # Plotting functions
├── results/                     # Output directory (created automatically)
└── old/                         # Legacy code (archived)
```

## Quick Start

### Requirements

- R ≥ 4.0.0
- Required package: `parallel` (included in base R)

### Run a Single Scenario

```r
source("run_experiment.R")

# Run baseline scenario with 100 iterations
scenarios <- define_scenarios()
result <- run_scenario(scenarios$A1, N_iterations = 100)
```

### Run All Scenarios

```r
source("run_experiment.R")

# Run all scenarios with 100 iterations each
results <- run_all_scenarios(N_iterations = 100)
```

Results are automatically saved to `results/<scenario_name>/` with detailed outputs.

## Test Scenarios

The framework includes **11 comprehensive test scenarios** organized into three categories:

### A) Baseline Scenarios

**A1: Baseline Uncorrelated**
- Uncorrelated predictors (ρ = 0), sparse β (3 active), strong signal
- Standard reference scenario for comparison

**A2: Single Predictor**
- Only one relevant variable among 20 candidates
- Tests ability to identify minimal models

**A3: Full Support**
- All variables are relevant (complete model)
- Tests behavior when no sparsity exists

### B) Structural Scenarios (Correlation)

**B1_weak: Weak AR(1) Correlation**
- Autoregressive correlation structure with ρ = 0.5
- Tests robustness to moderate predictor correlation

**B1_strong: Strong AR(1) Correlation**
- Autoregressive correlation structure with ρ = 0.8
- Tests performance under strong multicollinearity

**B2: Compound Symmetry**
- Exchangeable correlation structure (constant ρ = 0.5)
- Tests uniform correlation patterns

**B3: Block Structure**
- Block-diagonal correlation (groups of 5 with ρ = 0.7)
- Tests group-wise correlation patterns

### C) Support Scenarios (Signal Strength)

**C1: Weak Signals**
- Five weak signals among noise variables
- Tests sensitivity to low signal strength

**C2: Many Weak Signals**
- Ten weak signals (half the variables active)
- Tests performance with high-dimensional active sets

**C3: Mixed Signals**
- Eight variables with heterogeneous signal strengths
- Tests ability to handle varying coefficient magnitudes

## Output Structure

Each scenario creates a dedicated output directory:

```
results/<scenario_name>/
├── summary.txt                  # Human-readable summary report
├── summary_stats.csv            # Aggregated statistics across iterations
├── detailed_results.csv         # Full results (all iterations × p × metrics)
└── plots/
    ├── r2_and_mp_curves.png     # R² and M_p progression (averaged)
    ├── criterion_comparison.png # M_p vs AIC vs BIC (normalized)
    ├── mp_inflection.png        # M_p inflection point detection
    ├── delta2_plot.png          # Second derivative visualization
    └── boxplot_errors.png       # Error distribution across metrics
```

### Key Output Files

**summary.txt** includes:
- Scenario configuration (n, p, correlation structure, etc.)
- Selection performance (MAE, Bias, Variance, Hit Rate)
- Subset recovery metrics (TP, FP, FN, Precision, Recall, F1, Jaccard)
- Classification breakdown (correct/underfit/overfit percentages)

**detailed_results.csv** contains:
- `iteration`: Monte Carlo iteration number
- `p`: Model complexity (1 to p_max)
- `metric`: Selection criterion (M_p, AIC, BIC)
- `subset_p`: True variables ordered by coefficient magnitude
- `subset_Mp`: Best subset selected by M_p at complexity p
- `subset_AIC`: Best subset selected by AIC at complexity p
- `subset_BIC`: Best subset selected by BIC at complexity p
- Performance metrics for each iteration

**summary_stats.csv** aggregates:
- Selection accuracy (MAE, Bias, Variance, Hit Rate) per metric
- Subset recovery performance (mean Precision, Recall, F1, Jaccard)
- Average R², M_p, Delta2 values by model complexity

## Core Functions

### Data Generation (`R/01_data_generation.R`)

```r
generate_data(scenario)
```

Generates synthetic datasets with configurable properties:
- **Covariance structures**: identity, AR(1), compound symmetry, block
- **Support specifications**: single variable, sparse, full support
- **Signal strengths**: strong, weak, mixed

### R² Curve Computation (`R/02_metrics.R`)

```r
compute_r2_curve(X, y, n_cores = detectCores() - 1)
```

Performs exhaustive best subset search:
- For each $p = 1, \ldots, p_{\max}$, finds the subset maximizing R²
- Computes R², AIC, BIC for each cardinality
- Parallelized for computational efficiency
- Returns optimal subsets for each criterion

### Selection Metrics (`R/02_metrics.R`)

```r
apply_all_metrics(r2_curve)
```

Applies three selection criteria:
- **M_p**: Inflection point method (discrete second derivative)
- **AIC**: Akaike Information Criterion (minimum)
- **BIC**: Bayesian Information Criterion (minimum)

Returns selected model size and subset for each criterion.

### Evaluation (`R/03_evaluation.R`)

```r
evaluate_iteration(metric_results, p_true, support_true)
compute_summary_statistics(all_iterations, all_r2_curves, p_true, all_metric_results)
```

Computes performance metrics:
- **Size accuracy**: MAE, Bias, Variance, Hit Rate
- **Subset recovery**: TP, FP, FN, Precision, Recall, F1, Jaccard Index
- **Classification**: Correct, Underfit, Overfit percentages

### Visualization (`R/04_visualization.R`)

```r
create_all_plots(all_iterations, all_r2_curves, all_metric_results, 
                 summary_stats, p_true, output_dir)
```

Generates comprehensive visualization suite:
- R² and M_p curves (averaged across iterations)
- M_p inflection point detection
- Second derivative (Δ₂) visualization
- Criterion comparison (M_p vs AIC vs BIC)
- Error distribution boxplots

## Parallelization

The framework supports two levels of parallelization:

1. **Parallel iterations** (default): Multiple Monte Carlo runs execute simultaneously
2. **Parallel subset evaluation**: Best subset search parallelized across model sizes

To disable parallel iterations:
```r
run_scenario(scenario, N_iterations = 100, parallel_iterations = FALSE)
```

## Known Limitations

1. **Computational complexity**: Full enumeration requires evaluating $2^p$ models
   - Feasible for $p \leq 20$ (approximately 1 million models)
   - For larger $p$, use alternative selection methods (forward/backward selection, LASSO)

2. **M_p degeneracy**: When all predictors contribute equally, M_p curve may be flat
   - Detected but requires manual inspection

## Future Development

Planned enhancements:
- Greedy search algorithms for $p > 20$
- Additional selection criteria (Cp, cross-validation)
- Non-Gaussian error distributions
- More complex correlation structures

## License

MIT License

## Author

Created: November 2025

---

For detailed usage instructions, custom scenario creation, and advanced features, see [`USER_GUIDE.md`](USER_GUIDE.md).
