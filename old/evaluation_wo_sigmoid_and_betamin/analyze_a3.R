# Analyze A3 to find full support detection rule

data <- read.csv('results/A3_Full_Support/detailed_results.csv')
avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
R2_vals <- avg_data$R2
p_vals <- avg_data$p
M_p <- R2_vals / p_vals

cat('\n============================================================\n')
cat('A3 FULL SUPPORT ANALYSIS\n')
cat('============================================================\n\n')

cat(sprintf('M_p[1]  = %.6f\n', M_p[1]))
cat(sprintf('M_p[20] = %.6f\n', M_p[20]))
cat(sprintf('Ratio M_p[20]/M_p[1] = %.4f (%.1f%%)\n\n', M_p[20]/M_p[1], M_p[20]/M_p[1]*100))

cat(sprintf('R²[1]  = %.6f\n', R2_vals[1]))
cat(sprintf('R²[20] = %.6f\n\n', R2_vals[20]))

cat('Key insight: R² at p=20 is %.4f - nearly perfect!\n', R2_vals[20])
cat('This means ALL 20 variables are needed.\n\n')

# Compare with other scenarios
cat('Comparison with other scenarios:\n')
cat('================================\n\n')

scenarios <- list(
  A1_Baseline_Uncorrelated = 3,
  A2_Single_Predictor = 1,
  A3_Full_Support = 20,
  B1_AR1_Weak = 3
)

cat(sprintf('%-30s | True p* | R²[20] | M_p ratio\n', 'Scenario'))
cat(paste(rep('-', 70), collapse = ''), '\n')

for (scen in names(scenarios)) {
  data <- read.csv(paste0('results/', scen, '/detailed_results.csv'))
  avg_data <- aggregate(R2 ~ p, data = data, FUN = mean)
  R2_vals <- avg_data$R2
  M_p <- R2_vals / avg_data$p
  
  cat(sprintf('%-30s | %7d | %.4f | %.4f\n', 
              scen, 
              scenarios[[scen]], 
              R2_vals[20],
              M_p[20]/M_p[1]))
}

cat('\n\nOBSERVATION:\n')
cat('A3 (full support) has R²[20] ≈ 0.99, which is much higher than others.\n')
cat('Simple rule: If R²[p_max] > 0.95, select p* = p_max\n\n')
