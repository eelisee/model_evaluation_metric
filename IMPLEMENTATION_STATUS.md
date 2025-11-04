# M_p Model Selection: Advanced Implementation
## Implementation Status and Next Steps

### ✅ Completed Modules

#### 1. **data_generation.R** - Advanced Data Generation
**Location:** `R/data_generation.R`

**Features:**
- `generate_data_advanced(config)` - Comprehensive data generator supporting:
  - Multiple predictor distributions: Normal, t, uniform, lognormal
  - Correlation structures: Identity, AR(1), block, compound symmetry
  - Heteroscedastic errors (variance depends on X)
  - Measurement error in predictors
  - Non-zero predictor means and varying scales
  - Nonlinear terms (squared terms)
  - Interaction terms
  - Beta specifications: constant, descending, random permutation

**Configuration Options:**
```r
config <- list(
  n = 100,                              # Sample size
  p_max = 10,                           # Number of predictors
  beta_spec = "descending",             # or vector or "constant"/"random"
  sigma_eps = 0.2,                      # Noise SD
  X_dist = "normal",                    # "normal", "t", "uniform", "lognormal"
  correlation_structure = "identity",   # "ar1", "block", "compound"
  rho = 0,                              # Correlation parameter
  feature_means = NULL,                 # Vector of means
  feature_sigmas = NULL,                # Vector of SDs
  measurement_error_sd = 0,             # Measurement error
  heteroscedastic = FALSE,              # Heteroscedastic errors
  add_interactions = FALSE,             # Add interaction terms
  nonlinear_terms = FALSE,              # Add squared terms
  seed = 123                            # Random seed
)
```

#### 2. **model_evaluation.R** - Enhanced Model Enumeration and Evaluation
**Location:** `R/model_evaluation.R`

**Key Functions:**
- `enumerate_models(p_max, strategy, ...)` - Flexible model enumeration
  - `strategy = "full"` - Full power set (p_max ≤ 14 recommended)
  - `strategy = "cardinality_limited"` - Only subsets up to size max_p
  - `strategy = "random"` - Random sampling of n_samples subsets
  
- `evaluate_model(X, y, subset)` - Comprehensive single model evaluation
  - Returns: R², adjusted R², M_p, AIC, BIC, coefficients, SEs, predictions
  
- `evaluate_all_models_batch(X, y, subsets)` - Batch evaluation
- `aggregate_by_p(results)` - Summary statistics by cardinality

#### 3. **selection_rules.R** - Multiple Selection Rules
**Location:** `R/selection_rules.R`

**Implemented Rules:**
- **Rule A (max_mp):** Choose p* that maximizes M_p (ties → smallest p)
- **Rule B (mp):** Choose p just before steepest M_p decline
- **Rule C (elbow):** Use second derivative to find inflection point
- **AIC/BIC/adjR²:** Standard information criteria

**Key Functions:**
- `selection_rule_max_mp(best_models)` - Implements Rule A
- `selection_rule_mp(best_models)` - Implements Rule B
- `selection_rule_elbow(best_models)` - Implements Rule C
- `apply_all_selection_rules(results)` - Runs all rules and compares

#### 4. **recovery_metrics.R** - Performance Metrics
**Location:** `R/recovery_metrics.R`

**Key Functions:**
- `compute_recovery_metrics(selected, true, p_max)` 
  - Returns: TP, FP, FN, precision, recall, F1, Hamming distance
  
- `evaluate_selection_rules(selection_results, true_beta)`
  - Compares all selection rules against ground truth
  
- `compute_selection_stability(selection_history)`
  - Analyzes stability across multiple runs
  - Returns: Most common selection, frequency, average Hamming distance
  
- `compute_ranking_correlations(results, true_beta)`
  - Spearman correlations between metric rankings
  - Correlation with closeness to true model

### 🚧 In Progress / To Be Implemented

#### 5. **experiment_runner.R** - Experiment Orchestration
**Status:** Not started
**Needs:**
- `run_experiment(config)` - Main orchestration function
- Proper I/O structure (results/, scenario folders, meta.json)
- Integration of all modules
- Time tracking

#### 6. **scenarios.R** - Scenario Definitions
**Status:** Not started
**Needs:** Configuration generators for all 15 scenarios:
- S1: Constant incremental variance (degenerate)
- S2: Descending signal strength (baseline)
- S3: Random ordered signals (order invariance)
- S4: Heteroskedastic predictors
- S5: Different predictor means
- S6: Collinearity (AR(1), blocks)
- S7: Weak signals (low SNR)
- S8: High-dimensional (p ≈ n)
- S9: Nonlinear true model
- S10: Interaction terms
- S11: Redundant variables
- S12: Measurement error
- S13: Non-Gaussian predictors
- S14: Heteroscedastic errors
- S15: Group sparsity

#### 7. **batch_runner.R** - Multi-Run Experiment System
**Status:** Not started
**Needs:**
- Run multiple repetitions per scenario
- Aggregate results across runs
- Parameter grid support
- Parallel execution (optional)

#### 8. **enhanced_visualization.R** - Advanced Plotting
**Status:** Partially complete (basic plots exist)
**Needs to add:**
- Stability plots
- ROC-like curves (TPR vs FPR)
- Cross-scenario comparison plots
- Recovery rate vs SNR/collinearity
- Selection frequency heatmaps

#### 9. **io_utilities.R** - I/O and Folder Management
**Status:** Not started
**Needs:**
- Create folder structure: `results/<scenario_name>/`
- Save meta.json with full config
- Save all required CSVs and plots
- Load/resume functionality

#### 10. **reporting.R** - Automated Report Generation
**Status:** Not started
**Needs:**
- Generate summary.txt per scenario
- Aggregate results across scenarios
- Success/failure interpretation
- LaTeX table generation

### 📋 Required Output Files (Per Scenario)

```
results/
  <scenario_name>__<timestamp>/
    meta.json                    # Full configuration
    data.RData                   # X, y, true_beta
    models_full.csv              # All evaluated models
    best_models_by_p.csv         # Best per cardinality
    recovery_stats.csv           # TP/FP/FN/precision/recall/F1
    selection_comparison.csv     # All rules compared
    ranking_correlations.csv     # Spearman correlations
    summary.txt                  # Human-readable summary
    plots/
      mp_curve.png
      r2_curve.png
      criterion_comparison.png
      selection_stability.png
    diagnostics/
      aggregate_by_p.csv
      selection_stability.csv
```

### 🎯 Integration Priority

**Phase 1 (Immediate):**
1. Create `scenarios.R` with all 15 scenario configs
2. Create `experiment_runner.R` to orchestrate single runs
3. Create `io_utilities.R` for proper file management
4. Test end-to-end on S2 (baseline scenario)

**Phase 2 (Next):**
5. Create `batch_runner.R` for multiple repetitions
6. Enhance visualizations
7. Add reporting utilities
8. Run full test battery (S1-S15)

**Phase 3 (Analysis):**
9. Cross-scenario analysis
10. Rule comparison and recommendation
11. Documentation and interpretation guide

### 📝 Usage Example (Once Complete)

```r
# Load all modules
source("R/data_generation.R")
source("R/model_evaluation.R")
source("R/selection_rules.R")
source("R/recovery_metrics.R")
source("R/scenarios.R")
source("R/experiment_runner.R")

# Run single scenario
config <- scenario_s2_baseline()  # Descending signal strength
results <- run_experiment(config, n_reps = 100, output_dir = "results/")

# Run full test battery
scenarios <- list(s1_constant(), s2_baseline(), s3_random_order(), ...)
batch_results <- run_batch_experiments(scenarios, n_reps = 100)

# Compare rule performance
rule_comparison <- compare_rules_across_scenarios(batch_results)
plot_rule_performance(rule_comparison)
```

### 🔍 Testing Strategy

For each scenario:
1. **Sanity check:** Data generation produces expected structure
2. **Enumeration:** Correct number of models evaluated
3. **Selection:** Both rules return valid p*
4. **Recovery:** Metrics computed correctly when truth known
5. **Stability:** Multiple runs show expected variance
6. **Edge cases:** Degeneracies handled gracefully

### 📊 Success Criteria

A scenario "passes" if:
- No errors in execution
- M_p selects p* within ±1 of p_true (for clean scenarios)
- Rule A and Rule B yield reasonable, documented differences
- Stability metrics show expected behavior
- Computational time < 5 minutes (for p_max=10)

### 🚀 Next Steps

**Immediate actions:**
1. ✅ Complete core modules (DONE)
2. ⏳ Create scenarios.R (NEXT)
3. ⏳ Create experiment_runner.R (NEXT)
4. ⏳ Test on S2 baseline
5. ⏳ Iterate and expand

**Your input needed:**
- Confirm design approach matches requirements
- Any specific modifications to scenarios?
- Preferred parameter ranges for SNR/collinearity sweeps?
- Computational budget (max p_max, max n_reps)?

---

This implementation provides a solid, modular foundation for comprehensive M_p evaluation. The system is designed to be:
- **Extensible:** Easy to add new scenarios, rules, or metrics
- **Reproducible:** Full config saved, seeding controlled
- **Analyzable:** Rich output for deep investigation
- **Scalable:** Supports both quick tests and large-scale experiments
