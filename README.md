# M_p Model Selection Framework

## Overview

This project implements and empirically investigates the **M_p** model selection metric, which balances model fit and complexity. The metric is defined as:

$$M_p(S) = \frac{R^2_S}{p}$$

where:
- $R^2_S$ is the coefficient of determination for model $S$
- $p = |S|$ is the number of active regressors in the model

The M_p metric expresses the **explained variance per parameter**, quantifying how efficiently each regressor contributes to explanatory power.

## Theoretical Background

### Model Setting

We assume a linear model:

$$y = X\beta + \varepsilon$$

where:
- $y \in \mathbb{R}^n$: response vector
- $X \in \mathbb{R}^{n \times P_{\max}}$: design matrix with $P_{\max}$ potential predictors
- $\beta \in \mathbb{R}^{P_{\max}}$: coefficient vector
- $\varepsilon \sim \mathcal{N}(0, \sigma^2 I_n)$: normally distributed noise

### R² Definition

For any model $S \subseteq \{1, \dots, P_{\max}\}$:

$$R^2_S = 1 - \frac{RSS_S}{TSS}$$

where:
- $RSS_S = \sum_{i=1}^{n} (y_i - \hat{y}_{S,i})^2$ (residual sum of squares)
- $TSS = \sum_{i=1}^{n} (y_i - \bar{y})^2$ (total sum of squares)

### Selection Strategy

The framework implements an **M_p steep drop rule**: identify the model complexity $p^*$ just before the steepest decline in M_p values. This represents the optimal trade-off between explanatory power and parsimony.

For comparison, the framework also computes AIC and BIC selections.

## Project Structure

```
metric/
├── README.md                    # This file
├── USER_GUIDE.md                # Detailed usage guide
├── run_experiments.R            # Main entry point
├── R/                           # Core modules
│   ├── data_generation.R        # Data generation with multiple distributions
│   ├── model_evaluation.R       # Model enumeration and evaluation
│   ├── selection_rules.R        # M_p selection rule
│   ├── recovery_metrics.R       # Performance metrics
│   ├── scenarios.R              # 14 test scenarios
│   ├── io_utilities.R           # I/O and reporting
│   ├── visualization.R          # Plotting functions
│   └── experiment_runner.R      # Experiment orchestration
└── results/                     # Output directory (created automatically)
```

## Quick Start

### Requirements

- R ≥ 4.0.0
- Optional packages (auto-installed if needed):
  - `jsonlite` (for JSON metadata)
  - `ggplot2` (for enhanced plots, falls back to base R)

### Run a Quick Test

```r
source("run_experiments.R")
result <- run_quick_test()
```

This runs scenario S2 (baseline) with 100 observations and 10 predictors, completes in ~1-2 seconds, and saves results to `results_test/`.

### Run a Single Scenario

```r
source("run_experiments.R")
result <- run_single_scenario(
  scenario_id = "S2",
  n_reps = 1,
  output_dir = "results"
)
```

### Run Multiple Scenarios

```r
source("run_experiments.R")
results <- run_batch_scenarios(
  scenario_ids = c("S2", "S6", "S7"),
  n_reps = 10,
  output_dir = "results"
)
```

### Run All Scenarios

```r
source("run_experiments.R")
results <- run_all_scenarios(
  n_reps = 10,
  output_dir = "results"
)
```

For detailed instructions, see [`USER_GUIDE.md`](USER_GUIDE.md).

## Test Scenarios

The framework includes 14 comprehensive test scenarios:

### Core Scenarios

**S1: Constant Signal Strength**
- Tests degeneracy detection when all predictors contribute equally
- Expected behavior: M_p remains flat, rule should detect degeneracy

**S2: Baseline (Descending Signal)**
- Standard scenario with decreasing signal strength
- Tests basic functionality and serves as reference

**S3: Random Order Signal**
- Verifies order-invariance of the metric
- Ensures M_p selection doesn't depend on predictor ordering

### Predictor Properties

**S4: Heteroscedastic Predictors**
- Predictors have different variances
- Tests robustness to predictor scaling

**S5: Non-Zero Means**
- Predictors centered at different values
- Verifies proper mean-centering in calculations

### Collinearity

**S6: Collinearity**
- Three sub-scenarios: ρ = 0.3 (weak), 0.6 (moderate), 0.9 (strong)
- Tests performance under correlated predictors

### Signal Strength

**S7: Weak Signal**
- Three sub-scenarios: SNR = 0.5 (very weak), 1.0 (weak), 2.0 (moderate)
- Tests sensitivity to noise levels

### Complex Structures

**S9: Nonlinear Terms**
- True model includes squared terms
- Tests performance when linear assumptions violated

**S10: Interaction Terms**
- True model includes interaction X₁ × X₂
- Tests detection of multiplicative effects

**S11: Redundant Variables**
- Multiple irrelevant predictors among true signals
- Tests specificity and false positive control

**S12: Measurement Error**
- Predictors observed with noise
- Tests robustness to attenuation bias

**S13: Non-Gaussian Predictors**
- Two sub-scenarios: t-distribution (heavy tails), lognormal (skewed)
- Tests robustness to distributional assumptions

**S14: Heteroscedastic Errors**
- Error variance depends on predictor values
- Tests performance under non-constant variance

**S15: Group Sparsity**
- Predictors organized in groups, some groups fully inactive
- Tests block-level selection behavior

## Output Structure

Each experiment creates a timestamped directory:

```
results/<scenario>__<timestamp>/
├── meta.json                      # Full configuration (JSON)
├── data.RData                     # Generated data (X, y, true_beta)
├── models_full.csv                # All evaluated models
├── best_models_by_p.csv           # Best model per cardinality
├── recovery_stats.csv             # TP/FP/FN/Precision/Recall/F1
├── selection_comparison.csv       # M_p vs AIC vs BIC
├── ranking_correlations.csv       # Spearman correlations
├── aggregate_by_p.csv             # Summary statistics by p
├── summary.txt                    # Human-readable summary
└── plots/
    ├── mp_and_r2_curves.png       # M_p and R² progression
    ├── mp_efficiency_curve.png    # M_p values by cardinality
    ├── r2_curve.png               # R² by cardinality
    ├── all_models_scatter.png     # All models visualized
    └── criterion_comparison.png   # M_p vs AIC/BIC selections
```

The `summary.txt` includes:
- Scenario configuration
- Selection results (M_p, AIC, BIC)
- Recovery metrics (when true model known)
- **Best Models by Cardinality** table showing top models for each complexity level
- Execution timing

## Core Functions

### Data Generation

```r
generate_data_advanced(config)
```

Supports:
- Multiple distributions: normal, t, uniform, lognormal
- Correlation structures: identity, AR(1), block, compound symmetry
- Special cases: measurement error, heteroscedasticity, nonlinear terms, interactions

### Model Evaluation

```r
enumerate_models(p_max, strategy = "full")
evaluate_model(X, y, subset)
evaluate_all_models_batch(X, y, subsets)
aggregate_by_p(results)
```

Computes for each model:
- R², adjusted R², M_p
- AIC, BIC
- Coefficients and standard errors

### Selection

```r
selection_rule_mp(best_models)
apply_selection_rule(results, rule = "mp")
```

Implements M_p steep drop rule and returns comparison with AIC/BIC.

### Performance Metrics

```r
compute_recovery_metrics(selected, true, p_max)
evaluate_selection_rules(selection_results, true_beta)
```

Computes:
- Confusion matrix (TP, FP, FN)
- Classification metrics (Precision, Recall, F1)
- Distance metrics (Hamming distance)

### Visualization

```r
plot_mp_curves(results, true_p = NULL)
plot_criterion_comparison(results, selections)
```

**Test Results:** All scenarios execute successfully. Baseline test (S2) shows:
- M_p selection: close to true model (F1 = 0.800)
- BIC comparison: perfect recovery (F1 = 1.000)
- Execution time: ~1.2 seconds

## Known Limitations

1. **Full enumeration infeasible for p_max > 20**
   - Solution: Use `enumeration_strategy = "cardinality_limited"`

2. **M_p degeneracy in constant signal scenarios**
   - Detected automatically with `is_degenerate` flag

## License

MIT License

## Author

Created: November 2025

---

For detailed usage instructions, examples, and troubleshooting, see [`USER_GUIDE.md`](USER_GUIDE.md).
