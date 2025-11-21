# Visualization Module
# =====================
# Creates all plots for analysis (inspired by old visualization.R)

#' Plot R² and M_p Curves
#'
#' Two-panel plot showing R² increase and M_p efficiency
#' Uses averaged curves across all iterations for stability
#'
#' @param all_r2_curves List of R² curves from all iterations
#' @param all_metric_results List of metric results from all iterations
#' @param p_true Integer. True model size
#' @param filename String
#' @export
plot_r2_and_mp_curves <- function(all_r2_curves, all_metric_results, p_true, filename) {
  
  # Average R² across all iterations
  p_vals <- all_r2_curves[[1]]$p
  R2_avg <- numeric(length(p_vals))
  
  for (i in seq_along(p_vals)) {
    R2_values <- sapply(all_r2_curves, function(curve) curve$R2[i])
    R2_avg[i] <- mean(R2_values)
  }
  
  # Compute average M_p curve
  M_p_avg <- R2_avg / p_vals
  
  # Get most frequent p* selection (mode)
  p_star_selections <- sapply(all_metric_results, function(res) res$M_p$p_star)
  p_star_mp <- as.numeric(names(sort(table(p_star_selections), decreasing = TRUE)[1]))
  
  png(filename, width = 10, height = 8, units = "in", res = 300)
  par(mfrow = c(2, 1), mar = c(4, 4.5, 3, 2))
  
  # Panel 1: R² curve (averaged)
  plot(p_vals, R2_avg, type = "b", pch = 19, col = "#2E86AB",
       xlab = "Number of Predictors (p)", ylab = expression(R^2),
       main = "R² vs Model Complexity (Averaged Across Iterations)", cex = 1.2, lwd = 2,
       ylim = c(0, 1), xaxt = "n", cex.lab = 1.2, cex.main = 1.3)
  axis(1, at = p_vals)
  grid(col = "gray90", lty = 1)
  abline(v = p_true, lty = 1, col = "green3", lwd = 2)
  abline(v = p_star_mp, lty = 2, col = "#E63946", lwd = 2)
  
  # Panel 2: M_p curve (averaged)
  plot(p_vals, M_p_avg, type = "b", pch = 19, col = "#A23B72",
       xlab = "Number of Predictors (p)", ylab = expression(M[p] == R^2 / p),
       main = expression(paste(M[p], " Efficiency Curve (Averaged Across Iterations)")),
       xaxt = "n", cex.lab = 1.2, cex.main = 1.3)
  axis(1, at = p_vals)
  grid(col = "gray90", lty = 1)
  abline(v = p_true, lty = 1, col = "green3", lwd = 2)
  abline(v = p_star_mp, lty = 2, col = "#E63946", lwd = 2)
  
  legend("topright", legend = c("True p", "Most Frequent p*"),
         col = c("green3", "#E63946"), lty = c(1, 2), lwd = 2, cex = 1.0)
  
  dev.off()
}


#' Plot Criterion Comparison (M_p, AIC, BIC)
#'
#' Normalized comparison of all three selection criteria
#' Uses averaged curves across all iterations for stability
#'
#' @param all_r2_curves List of R² curves from all iterations
#' @param all_metric_results List of metric results from all iterations
#' @param p_true Integer
#' @param filename String
#' @export
plot_criterion_comparison <- function(all_r2_curves, all_metric_results, p_true, filename) {
  
  # Average across all iterations
  p_vals <- all_r2_curves[[1]]$p
  R2_avg <- numeric(length(p_vals))
  AIC_avg <- numeric(length(p_vals))
  BIC_avg <- numeric(length(p_vals))
  
  for (i in seq_along(p_vals)) {
    R2_values <- sapply(all_r2_curves, function(curve) curve$R2[i])
    AIC_values <- sapply(all_r2_curves, function(curve) curve$AIC[i])
    BIC_values <- sapply(all_r2_curves, function(curve) curve$BIC[i])
    
    R2_avg[i] <- mean(R2_values)
    AIC_avg[i] <- mean(AIC_values)
    BIC_avg[i] <- mean(BIC_values)
  }
  
  # Compute average M_p
  M_p_avg <- R2_avg / p_vals
  
  # Normalize for comparison (0-1 scale)
  normalize <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  
  # For AIC/BIC, invert since lower is better
  Mp_norm <- normalize(M_p_avg)
  AIC_norm <- 1 - normalize(AIC_avg)
  BIC_norm <- 1 - normalize(BIC_avg)
  
  # Get most frequent selections (mode)
  p_star_mp <- as.numeric(names(sort(table(sapply(all_metric_results, function(res) res$M_p$p_star)), decreasing = TRUE)[1]))
  p_star_aic <- as.numeric(names(sort(table(sapply(all_metric_results, function(res) res$AIC$p_star)), decreasing = TRUE)[1]))
  p_star_bic <- as.numeric(names(sort(table(sapply(all_metric_results, function(res) res$BIC$p_star)), decreasing = TRUE)[1]))
  
  png(filename, width = 10, height = 6, units = "in", res = 300)
  par(mar = c(4, 4.5, 3, 2))
  
  plot(p_vals, Mp_norm, type = "b", pch = 19, col = "#A23B72",
       xlab = "Number of Predictors (p)", ylab = "Normalized Score (higher is better)",
       main = "Model Selection Criteria Comparison (Averaged)", lwd = 2, cex = 1.2,
       ylim = c(0, 1), xaxt = "n", cex.lab = 1.2, cex.main = 1.3)
  axis(1, at = p_vals)
  grid(col = "gray90", lty = 1)
  
  lines(p_vals, AIC_norm, type = "b", pch = 17, 
        col = "#2E86AB", lty = 2, lwd = 2, cex = 1.2)
  lines(p_vals, BIC_norm, type = "b", pch = 15, 
        col = "#F18F01", lty = 3, lwd = 2, cex = 1.2)
  
  # Mark true p
  abline(v = p_true, lty = 1, col = "green3", lwd = 2)
  
  legend("topright", legend = c(
    sprintf("M_p (p*=%d)", p_star_mp),
    sprintf("AIC (p*=%d)", p_star_aic),
    sprintf("BIC (p*=%d)", p_star_bic),
    sprintf("True (p=%d)", p_true)
  ), col = c("#A23B72", "#2E86AB", "#F18F01", "green3"),
  lty = c(1, 2, 3, 1), pch = c(19, 17, 15, NA), lwd = 2, cex = 1.0)
  
  dev.off()
}


#' Plot p* Distribution Across Iterations
#'
#' Histograms showing distribution of selected p* for each metric
#'
#' @param all_iterations List. Evaluation results
#' @param p_true Integer. True model size
#' @param filename String. Output filename
#' @export
plot_p_star_distribution <- function(all_iterations, p_true, filename) {
  
  df <- do.call(rbind, all_iterations)
  metrics <- unique(df$metric)
  
  png(filename, width = 10, height = 4, units = "in", res = 300)
  par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
  
  colors <- c("#A23B72", "#2E86AB", "#F18F01")
  
  for (i in seq_along(metrics)) {
    m <- metrics[i]
    subset <- df[df$metric == m, ]
    
    hist(subset$p_star, 
         breaks = seq(0.5, max(subset$p_star) + 0.5, by = 1),
         col = colors[i],
         border = "white",
         xlab = "Selected p*",
         main = m,
         xlim = c(0, max(df$p_star) + 1),
         cex.lab = 1.1, cex.main = 1.2)
    
    abline(v = p_true, col = "green3", lwd = 3, lty = 1)
    abline(v = mean(subset$p_star), col = "black", lwd = 2, lty = 2)
  }
  
  dev.off()
}


#' Plot p* Scatter Across Iterations
#'
#' Clean scatter plot showing selected p* for each metric across iterations
#'
#' @param all_iterations List
#' @param p_true Integer
#' @param filename String
#' @export
plot_p_star_scatter <- function(all_iterations, p_true, filename) {
  
  df <- do.call(rbind, all_iterations)
  df$iteration <- rep(1:(nrow(df) / length(unique(df$metric))), 
                     each = length(unique(df$metric)))
  
  metrics <- unique(df$metric)
  colors <- c("#A23B72", "#2E86AB", "#F18F01")
  pch_symbols <- c(19, 19, 19)  # circles for all
  
  png(filename, width = 10, height = 6, units = "in", res = 300)
  par(mar = c(4, 4.5, 3, 8))  # Extra right margin for legend
  
  # Determine x-axis limits (now p* is on x-axis)
  x_max <- max(df$p_star) + 1
  x_min <- 0
  
  # Swapped axes: p* on x-axis, iteration on y-axis
  plot(NULL, xlim = c(x_min, x_max), ylim = c(0.8, max(df$iteration) + 0.2),
       xlab = "Selected p*", ylab = "Iteration",
       main = "p* Selections Across Iterations",
       cex.lab = 1.2, cex.main = 1.3,
       xaxt = "n", yaxt = "n")
  
  # Custom x-axis
  axis(1, at = 0:x_max, cex.axis = 1.1)
  
  # Custom y-axis with integer ticks
  axis(2, at = 1:max(df$iteration), cex.axis = 1.1, las = 1)
  
  # Add grid
  grid(nx = NULL, ny = NULL, col = "gray90", lty = 1)
  
  # Add true p line (vertical now)
  abline(v = p_true, col = "green3", lwd = 2.5, lty = 1)
  
  # Plot points for each metric (axes swapped)
  for (i in seq_along(metrics)) {
    m <- metrics[i]
    subset <- df[df$metric == m, ]
    points(subset$p_star, subset$iteration,
           col = colors[i], pch = pch_symbols[i], cex = 1.8, lwd = 2)
  }
  
  # Legend outside plot area
  legend(x = x_max * 1.05, y = max(df$iteration) * 0.95, 
         legend = c(metrics, "True p"), 
         col = c(colors, "green3"), 
         pch = c(pch_symbols, NA),
         lty = c(rep(NA, length(metrics)), 1),
         lwd = c(rep(2, length(metrics)), 2.5),
         pt.cex = 1.8,
         cex = 1.1,
         bty = "n",
         xpd = TRUE)
  
  dev.off()
}



#' Plot Error Distribution
#'
#' @param all_iterations List
#' @param filename String
#' @export
plot_error_distribution <- function(all_iterations, filename) {
  
  df <- do.call(rbind, all_iterations)
  metrics <- unique(df$metric)
  colors <- c("#A23B72", "#2E86AB", "#F18F01")
  
  png(filename, width = 10, height = 4, units = "in", res = 300)
  par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
  
  for (i in seq_along(metrics)) {
    m <- metrics[i]
    subset <- df[df$metric == m, ]
    
    hist(subset$error, 
         breaks = 20,
         col = colors[i],
         border = "white",
         xlab = "Error (p* - p_true)",
         main = m,
         cex.lab = 1.1, cex.main = 1.2)
    
    grid(col = "gray90", lty = 1)
    abline(v = 0, col = "green3", lwd = 3)
    abline(v = mean(subset$error), col = "black", lwd = 2, lty = 2)
  }
  
  dev.off()
}


#' Plot Hit Rate Comparison
#'
#' @param summary_stats Data frame from compute_summary_statistics()
#' @param filename String
#' @export
plot_hit_rate <- function(summary_stats, filename) {
  
  colors <- c("#A23B72", "#2E86AB", "#F18F01")
  
  png(filename, width = 8, height = 6, units = "in", res = 300)
  par(mar = c(4, 4.5, 3, 2))
  
  bp <- barplot(summary_stats$HitRate,
          names.arg = summary_stats$metric,
          col = colors,
          ylab = "Hit Rate",
          main = "Hit Rate Comparison",
          ylim = c(0, 1),
          border = "white",
          cex.lab = 1.2, cex.main = 1.3, cex.names = 1.1)
  
  grid(NA, NULL, col = "gray90", lty = 1)
  
  # Add value labels on top of bars
  text(bp, summary_stats$HitRate + 0.05, 
       labels = sprintf("%.1f%%", summary_stats$HitRate * 100),
       cex = 1.1, font = 2)
  
  dev.off()
}


#' Plot Classification Breakdown
#'
#' @param summary_stats Data frame
#' @param filename String
#' @export
plot_classification_breakdown <- function(summary_stats, filename) {
  
  mat <- as.matrix(summary_stats[, c("n_correct", "n_underfit", "n_overfit")])
  rownames(mat) <- summary_stats$metric
  
  png(filename, width = 10, height = 6, units = "in", res = 300)
  par(mar = c(4, 4.5, 3, 2))
  
  barplot(t(mat), beside = TRUE,
          col = c("green3", "#2E86AB", "#E63946"),
          legend.text = c("Correct", "Underfit", "Overfit"),
          xlab = "Metric",
          ylab = "Count",
          main = "Classification Breakdown",
          border = "white",
          cex.lab = 1.2, cex.main = 1.3, cex.names = 1.1,
          args.legend = list(x = "topright", cex = 1.0, bty = "n"))
  
  grid(NA, NULL, col = "gray90", lty = 1)
  
  dev.off()
}


#' Plot Second Derivative (Δ₂) for M_p
#'
#' Shows the second derivative of M_p curve to visualize inflection point detection
#' Uses averaged curves across all iterations for stability
#'
#' @param all_r2_curves List of R² curves from all iterations
#' @param all_metric_results List of metric results from all iterations
#' @param filename String
#' @export
plot_second_derivative <- function(all_r2_curves, all_metric_results, filename) {
  
  # Average delta2 across all iterations
  first_delta2 <- all_metric_results[[1]]$M_p$delta2
  delta2_avg <- numeric(length(first_delta2))
  
  for (i in seq_along(first_delta2)) {
    delta2_values <- sapply(all_metric_results, function(res) res$M_p$delta2[i])
    delta2_avg[i] <- mean(delta2_values)
  }
  
  p_vals <- all_r2_curves[[1]]$p[1:length(delta2_avg)]
  
  # Get most frequent p* selection
  p_star_selections <- sapply(all_metric_results, function(res) res$M_p$p_star)
  p_star_mp <- as.numeric(names(sort(table(p_star_selections), decreasing = TRUE)[1]))
  
  png(filename, width = 10, height = 6, units = "in", res = 300)
  par(mar = c(4, 4.5, 3, 2))
  
  plot(p_vals, delta2_avg, 
       type = "b", pch = 19, col = "#A23B72",
       xlab = "Model Size (p)", 
       ylab = expression(Delta[2]~"(Second Derivative of M"[p]*")"),
       main = "Second Derivative of M_p Curve (Averaged Across Iterations)",
       lwd = 2, cex = 1.2,
       cex.lab = 1.2, cex.main = 1.3)
  grid()
  
  abline(h = 0, col = "gray30", lty = 2, lwd = 1.5)
  
  # Mark most frequent inflection point
  inflection_idx <- which(p_vals == p_star_mp)
  
  if (length(inflection_idx) > 0 && inflection_idx <= length(delta2_avg)) {
    points(p_star_mp, delta2_avg[inflection_idx], 
           col = "#E63946", pch = 8, cex = 2.5, lwd = 2)
    text(p_star_mp, delta2_avg[inflection_idx], 
         sprintf("p* = %d (mode)", p_star_mp),
         col = "#E63946", font = 2, pos = 3, cex = 1.1)
  }
  
  dev.off()
}


#' Create All Plots
#'
#' @param all_iterations List
#' @param all_r2_curves List of all R² curves
#' @param all_metric_results List of all metric results
#' @param summary_stats Data frame
#' @param p_true Integer
#' @param output_dir String
#' @export
create_all_plots <- function(all_iterations, all_r2_curves, all_metric_results,
                             summary_stats, p_true, output_dir) {
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  cat("  Creating visualizations...\n")
  
  plot_r2_and_mp_curves(
    all_r2_curves, all_metric_results, p_true,
    file.path(output_dir, "01_r2_and_mp_curves.png")
  )
  
  plot_criterion_comparison(
    all_r2_curves, all_metric_results, p_true,
    file.path(output_dir, "02_criterion_comparison.png")
  )
  
  plot_p_star_distribution(
    all_iterations, p_true,
    file.path(output_dir, "03_p_star_distribution.png")
  )
  
  plot_p_star_scatter(
    all_iterations, p_true,
    file.path(output_dir, "04_p_star_scatter.png")
  )
  
  plot_error_distribution(
    all_iterations,
    file.path(output_dir, "05_error_distribution.png")
  )
  
  plot_hit_rate(
    summary_stats,
    file.path(output_dir, "06_hit_rate.png")
  )
  
  plot_classification_breakdown(
    summary_stats,
    file.path(output_dir, "07_classification_breakdown.png")
  )
  
  plot_second_derivative(
    all_r2_curves, all_metric_results,
    file.path(output_dir, "08_second_derivative.png")
  )
  
  plot_subset_metrics(
    summary_stats,
    file.path(output_dir, "09_subset_metrics.png")
  )
  
  plot_subset_confusion_matrix(
    summary_stats,
    file.path(output_dir, "10_subset_confusion_matrix.png")
  )
  
  cat("  ✓ All plots saved to:", output_dir, "\n")
}


#' Plot Subset Selection Metrics Comparison
#'
#' Shows Jaccard, Precision, Recall, F1 scores for all metrics
#'
#' @param summary_stats Data frame
#' @param filename String
#' @export
plot_subset_metrics <- function(summary_stats, filename) {
  
  # Check if subset metrics are available
  if (!("Jaccard" %in% names(summary_stats)) || all(is.na(summary_stats$Jaccard))) {
    cat("    Skipping subset metrics plot (no subset data available)\n")
    return(invisible(NULL))
  }
  
  metrics <- summary_stats$metric
  
  png(filename, width = 10, height = 8, units = "in", res = 300)
  par(mfrow = c(2, 2), mar = c(5, 5, 3, 2))
  
  colors <- c("#A23B72", "#2E86AB", "#F18F01")
  
  # Jaccard Index
  barplot(summary_stats$Jaccard, names.arg = metrics, col = colors,
          main = "Jaccard Index (Higher = Better)",
          ylab = "Jaccard Index", ylim = c(0, 1),
          cex.main = 1.3, cex.lab = 1.2, cex.names = 1.1)
  abline(h = seq(0, 1, 0.2), lty = 3, col = "gray70")
  grid(NA, NULL)
  
  # Precision
  barplot(summary_stats$Precision, names.arg = metrics, col = colors,
          main = "Precision (TP / (TP + FP))",
          ylab = "Precision", ylim = c(0, 1),
          cex.main = 1.3, cex.lab = 1.2, cex.names = 1.1)
  abline(h = seq(0, 1, 0.2), lty = 3, col = "gray70")
  grid(NA, NULL)
  
  # Recall
  barplot(summary_stats$Recall, names.arg = metrics, col = colors,
          main = "Recall (TP / (TP + FN))",
          ylab = "Recall", ylim = c(0, 1),
          cex.main = 1.3, cex.lab = 1.2, cex.names = 1.1)
  abline(h = seq(0, 1, 0.2), lty = 3, col = "gray70")
  grid(NA, NULL)
  
  # F1 Score
  barplot(summary_stats$F1, names.arg = metrics, col = colors,
          main = "F1 Score (Harmonic Mean)",
          ylab = "F1 Score", ylim = c(0, 1),
          cex.main = 1.3, cex.lab = 1.2, cex.names = 1.1)
  abline(h = seq(0, 1, 0.2), lty = 3, col = "gray70")
  grid(NA, NULL)
  
  dev.off()
}


#' Plot Subset Confusion Matrix (TP, FP, FN)
#'
#' Stacked bar chart showing average TP, FP, FN for each metric
#'
#' @param summary_stats Data frame
#' @param filename String
#' @export
plot_subset_confusion_matrix <- function(summary_stats, filename) {
  
  # Check if subset metrics are available
  if (!("avg_TP" %in% names(summary_stats)) || all(is.na(summary_stats$avg_TP))) {
    cat("    Skipping confusion matrix plot (no subset data available)\n")
    return(invisible(NULL))
  }
  
  metrics <- summary_stats$metric
  
  # Create matrix for stacked barplot
  confusion_data <- rbind(
    TP = summary_stats$avg_TP,
    FP = summary_stats$avg_FP,
    FN = summary_stats$avg_FN
  )
  colnames(confusion_data) <- metrics
  
  png(filename, width = 10, height = 6, units = "in", res = 300)
  par(mar = c(5, 5, 3, 8), xpd = TRUE)
  
  barplot(confusion_data, beside = TRUE,
          col = c("#2E7D32", "#C62828", "#F57C00"),
          main = "Subset Selection Performance (Avg TP, FP, FN)",
          ylab = "Average Count",
          xlab = "Metric",
          cex.main = 1.3, cex.lab = 1.2, cex.names = 1.1,
          ylim = c(0, max(confusion_data, na.rm = TRUE) * 1.2))
  
  grid(NA, NULL)
  
  legend("topright", inset = c(-0.2, 0),
         legend = c("True Positives (TP)", "False Positives (FP)", "False Negatives (FN)"),
         fill = c("#2E7D32", "#C62828", "#F57C00"),
         bty = "n", cex = 1.0)
  
  dev.off()
}
