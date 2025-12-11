# Analyze stability from saved results
library(data.table)

results <- fread("results/A1_Baseline_Uncorrelated/detailed_results.csv")

cat("\n=== Sigmoid p* selections across 5 iterations ===\n")
for (iter in 1:5) {
  iter_data <- results[iteration == iter]
  selected_p <- iter_data[selected_by_Mp == TRUE, p]
  
  # Get M_p values for p=1,2,3,4,5
  Mp_vals <- iter_data[p <= 5, .(p, M_p)]
  
  # Calculate differences
  diffs <- diff(Mp_vals$M_p)
  
  cat(sprintf("\nIteration %d: selected p*=%d\n", iter, selected_p))
  cat("  M_p values and differences:\n")
  for (i in 1:5) {
    if (i < 5) {
      cat(sprintf("    p=%d: M_p=%.4f, Δ=%.4f (%.1f%% decrease)\n", 
                  i, Mp_vals$M_p[i], diffs[i], 
                  abs(diffs[i]/Mp_vals$M_p[i])*100))
    } else {
      cat(sprintf("    p=%d: M_p=%.4f\n", i, Mp_vals$M_p[i]))
    }
  }
}
