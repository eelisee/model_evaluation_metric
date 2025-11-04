# M_p Model Evaluation Metric

## Overview

This project implements and empirically investigates a new model selection metric, **M_p**, which balances model fit and model complexity. The metric is defined as:

$$M_p(S) = \frac{R^2_S}{p}$$

where:
- $R^2_S$ is the coefficient of determination for model $S$
- $p = |S|$ is the number of active regressors in the model

The M_p metric expresses the **explained variance per parameter**, quantifying how efficiently each additional regressor contributes to the model's explanatory power.

## Theoretical Background

### Model Setting

We assume a linear model:

$$y = X\beta + \varepsilon$$

where:
- $y \in \mathbb{R}^n$: response vector
- $X \in \mathbb{R}^{n \times P_{\max}}$: design matrix with $P_{\max}$ potential explanatory variables
- $\beta \in \mathbb{R}^{P_{\max}}$: coefficient vector
- $\varepsilon \sim \mathcal{N}(0, \sigma^2 I_n)$: normally distributed noise

### R² Definition

For any model $S \subseteq \{1, \dots, P_{\max}\}$:

$$R^2_S = 1 - \frac{RSS_S}{TSS}$$

where:
- $RSS_S = \sum_{i=1}^{n} (y_i - \hat{y}_{S,i})^2$
- $TSS = \sum_{i=1}^{n} (y_i - \bar{y})^2$

### Optimal Model Selection

The optimal model complexity $p^*$ is determined by identifying the point of inflection in the $M_p$ curve, representing the trade-off between explanatory power and model parsimony.

## Project Structure

```
metric/
├── README.md                 # This file
├── R/
│   ├── mp_functions.R       # Core functions for M_p calculation
│   ├── visualization.R      # Plotting functions
│   └── utils.R              # Utility functions
├── main_analysis.R          # Main execution script
├── example.R                # Example Code
├── example_output/          # Example results
│   ├── model_results.csv
│   └── plots/
└── install.R                # Required packages
```

## Installation

### Requirements

- R (≥ 4.0.0)
- Optional package (recommended for better plots):
  ```r
  install.packages("ggplot2")
  ```

**Note:** The code works with or without ggplot2. Without it, base R graphics are used automatically.

To verify installation:
```r
source("install.R")
```

## Usage

### Quick Start

**Option 1:** Simple example (fast, 8 predictors):
```r
source("example.R")
```

**Option 2:** Full analysis (complete, 10 predictors):
```r
source("main_analysis.R")
```

See `QUICKSTART.md` for detailed usage instructions.

### Custom Analysis

```r
# Load functions
source("R/mp_functions.R")
source("R/visualization.R")

# Generate data
set.seed(123)
data <- generate_data(
  n = 100,                           # Number of observations
  p_max = 10,                        # Maximum number of predictors
  true_beta = c(1, 0.8, 0.5, rep(0, 7)),  # True coefficients
  sigma = 0.2                        # Noise level
)

# Evaluate all models
results <- evaluate_all_models(data$X, data$y)

# Find optimal model
optimal <- find_optimal_model(results)

# Visualize results
plot_mp_curves(results, optimal$p_star)
```

## Key Functions

### Data Generation

- `generate_data(n, p_max, true_beta, sigma)`: Generate synthetic dataset

### Model Evaluation

- `evaluate_all_models(X, y)`: Fit all possible models and compute M_p
- `compute_mp(R2, p)`: Calculate M_p metric for given R² and p
- `find_optimal_model(results)`: Identify optimal model complexity p*

### Visualization

- `plot_mp_curves(results, p_star)`: Plot R² and M_p vs. model complexity
- `plot_best_mp_curve(results)`: Plot only the best M_p for each p

## Functionality Overview

1. **Data Generation**: Create synthetic dataset with known structure
2. **Model Enumeration**: Generate all $2^{P_{\max}}$ possible models
3. **Model Evaluation**: For each model, compute $R^2_S$ and $M_p(S)$
4. **Grouping**: Group results by model cardinality $p$
5. **Optimization**: Find $p^*$ with steepest descent
6. **Visualization**: Plot curves

## Implementation Notes

- Exhaustive search might not be computationally feasible for $P_{\max}$ large
- For larger problems, consider greedy or stochastic search algorithms
- The metric is dimensionless and directly comparable across different datasets

## Comparison with Other Metrics

The M_p metric can be compared in `main_analysis.R` with
- **AIC** (Akaike Information Criterion)
- **BIC** (Bayesian Information Criterion)
- **Adjusted R²**

## License

MIT License

## Author

Created: November 2025
