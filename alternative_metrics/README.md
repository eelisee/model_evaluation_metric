# Alternative Metrics (Experimental/Joke Implementations)

This folder contains alternative variants of the M_p metric that are **likely inferior** but interesting to test empirically for educational purposes.

## The "Joke" Metrics

### 1. M_p_minus: R² / (p-1)

**Formula:**
```
M_p_minus = R² / (p-1)    for p ≥ 2
M_p_minus = 0              for p = 1
```

**Rationale:** "The first predictor is free!" - Only penalize additional predictors beyond the first.

**Expected Behavior:** Over-selection compared to standard M_p, since the denominator is smaller.

**Why it's silly:** The first predictor isn't actually "free" - it still consumes a degree of freedom and should be penalized.

---

### 2. M_p_scaled: R² / (p/p_max)

**Formula:**
```
M_p_scaled = R² / (p/p_max) = (R² · p_max) / p
```

**Rationale:** "Complexity should be relative to the problem size"

**Expected Behavior:** **Identical to M_p** because multiplying by a constant (p_max) doesn't change the ordering.

**Why it's silly:** This is just M_p scaled by a constant factor. The steep drop rule finds the maximum difference, which is scale-invariant. So this literally does nothing different from M_p.

**Mathematical proof:**
```
Drop_i = M_scaled(p_i) - M_scaled(p_i+1)
       = p_max · M_p(p_i) - p_max · M_p(p_i+1)  
       = p_max · (M_p(p_i) - M_p(p_i+1))
       = p_max · Drop_Mp(i)

argmax Drop_i = argmax Drop_Mp(i)  ✓ Same selection!
```

## Usage

```r
# Source the comparison script
source("alternative_metrics/run_comparison.R")

# Run comparison across all 14 scenarios
results <- run_all_alternative_comparisons()
```

## Results Preview

You'll see output like:

```
Selected Complexity (p*):
  Scenario   p_true  Mp  Mp_minus  Mp_scaled  AIC  BIC
  S1         10      2   ?         2          10   10
  S2         3       1   2         1          3    3
  ...

F1 Scores:
  Scenario   F1_Mp  F1_Mp_minus  F1_Mp_scaled  F1_AIC  F1_BIC
  S1         0.333  ?            0.333         1.000   1.000
  S2         0.500  0.800        0.500         1.000   1.000
  ...
```

## Files

- `alternative_selection_rules.R` - Implementation of the two alternative metrics
- `run_comparison.R` - Script to run all 14 scenarios and compare results
- `comparison_results.csv` - Output results (generated when you run the script)

## Why This Exists

This is a **teaching tool** to demonstrate:

1. **Theoretical analysis vs. empirical testing**: M_p_scaled is provably identical, but let's verify!
2. **The importance of proper penalization**: M_p_minus might over-select or still under-select
3. **Scale invariance**: Multiplying by constants doesn't change ordinal rankings