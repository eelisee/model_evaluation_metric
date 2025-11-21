# Visualization Module
# =====================
# Creates all plots for analysis (inspired by old visualization.R)

#' Plot R² and M_p Curves
#'
#' Two-panel plot showing R² increase and M_p efficiency
#'
#' @param r2_curve Data frame with p, R2, AIC, BIC
#' @param metric_results List from apply_all_metrics()
#' @param p_true Integer. True model size
#' @param filename String
#' @export
plot_r2_and_mp_curves <- function(r2_curve, metric_results, p_true, filename) {
  
  # Compute M_p curve
  M_p <- r2_curve$R2 / r2_curve$p
  p_star_mp <- metric_results$M_p$p_star
  
  png(filename, width = 10, height = 8, units = "in", res = 300)
  par(mfrow = c(2, 1), mar = c(4, 4.5, 3, 2))
  
  # Panel 1: R² curve
  plot(r2_curve$p, r2_curve$R2, type = "b", pch = 19, col = "#2E86AB",
       xlab = "Number of Predictors (p)", ylab = expression(R^2),
       main = "R² vs Model Complexity", cex = 1.2, lwd = 2,
       ylim = c(0, 1), xaxt = "n", cex.lab = 1.2, cex.main = 1.3)
  axis(1, at = r2_curve$p)
  grid()
  abline(v = p_true, lty = 1, col = "green3", lwd = 2)
  abline(v = p_star_mp, lty = 2, col = "#E63946", lwd = 2)
  
  # Panel 2: M_p curve
  plot(r2_curve$p, M_p, type = "b", pch = 19, col = "#A23B72",
       xlab = "Number of Predictors (p)", ylab = expression(M[p]~"="~R^2/p),
       main = "M_p Efficiency Curve", cex = 1.2, lwd = 2,
       xaxt = "n", cex.lab = 1.2, cex.main = 1.3)
  axis(1, at = r2_curve$p)
  grid()
  abline(v = p_true, lty = 1, col = "green3", lwd = 2)
  abline(v = p_star_mp, lty = 2, col = "#E63946", lwd = 2)
  
  # Highlight optimal point
  optimal_mp <- M_p[r2_curve$p == p_star_mp]
  points(p_star_mp, optimal_mp, pch = 21, col = "#E63946", 
         bg = "#E63946", cex = 2.5, lwd = 2)
  
  legend("topright", legend = c("True p", "Selected p*"),
         col = c("green3", "#E63946"), lty = c(1, 2), lwd = 2, cex = 1.1)
  
  dev.off()
}


#' Plot Criterion Comparison (M_p, AIC, BIC)
#'
#' Normalized comparison of all three selection criteria
#'
#' @param r2_curve Data frame
#' @param metric_results List from apply_all_metrics()
#' @param p_true Integer
#' @param filename String
#' @export
plot_criterion_comparison <- function(r2_curve, metric_results, p_true, filename) {
  
  # Compute M_p
  M_p <- r2_curve$R2 / r2_curve$p
  
  # Normalize for comparison (0-1 scale)
  normalize <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  
  # For AIC/BIC, invert since lower is better
  Mp_norm <- normalize(M_p)
  AIC_norm <- 1 - normalize(r2_curve$AIC)
  BIC_norm <- 1 - normalize(r2_curve$BIC)
  
  # Extract optimal points
  p_star_mp <- metric_results$M_p$p_star
  p_star_aic <- metric_results$AIC$p_star
  p_star_bic <- metric_results$BIC$p_star
  
  png(filename, width = 10, height = 6, units = "in", res = 300)
  par(mar = c(4, 4.5, 3, 2))
  
  plot(r2_curve$p, Mp_norm, type = "b", pch = 19, col = "#A23B72",
       xlab = "Number of Predictors (p)", ylab = "Normalized Score (higher is better)",
       main = "Model Selection Criteria Comparison", lwd = 2, cex = 1.2,
       ylim = c(0, 1), xaxt = "n", cex.lab = 1.2, cex.main = 1.3)
  axis(1, at = r2_curve$p)
  grid()
  
  lines(r2_curve$p, AIC_norm, type = "b", pch = 17, 
        col = "#2E86AB", lty = 2, lwd = 2, cex = 1.2)
  lines(r2_curve$p, BIC_norm, type = "b", pch = 15, 
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
  
  # Determine y-axis limits
  y_max <- max(df$p_star) + 1
  y_min <- 0
  
  plot(NULL, xlim = c(0.8, max(df$iteration) + 0.2), ylim = c(y_min, y_max),
       xlab = "Iteration", ylab = "Selected p*",
       main = "p* Selections Across Iterations",
       cex.lab = 1.2, cex.main = 1.3,
       xaxt = "n", yaxt = "n")
  
  # Custom x-axis with integer ticks
  axis(1, at = 1:max(df$iteration), cex.axis = 1.1)
  
  # Custom y-axis
  axis(2, at = 0:y_max, cex.axis = 1.1, las = 1)
  
  # Add grid
  grid(nx = NULL, ny = NULL, col = "gray90", lty = 1)
  
  # Add true p line
  abline(h = p_true, col = "green3", lwd = 2.5, lty = 1)
  
  # Plot points for each metric
  for (i in seq_along(metrics)) {
    m <- metrics[i]
    subset <- df[df$metric == m, ]
    points(subset$iteration, subset$p_star, 
           col = colors[i], pch = pch_symbols[i], cex = 1.8, lwd = 2)
  }
  
  # Legend outside plot area
  legend(x = max(df$iteration) * 1.05, y = y_max * 0.95, 
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
  
  dev.off()
}


#' Plot Second Derivative (Δ₂) for M_p
#'
#' Shows the second derivative of M_p curve to visualize inflection point detection
#'
#' @param r2_curve Data frame
#' @param mp_result Result from metric_mp()
#' @param filename String
#' @export
plot_second_derivative <- function(r2_curve, mp_result, filename) {
  
  delta2 <- mp_result$delta2
  p_vals <- r2_curve$p[1:length(delta2)]
  
  png(filename, width = 10, height = 6, units = "in", res = 300)
  par(mar = c(4, 4.5, 3, 2))
  
  plot(p_vals, delta2, 
       type = "b", pch = 19, col = "#A23B72",
       xlab = "Model Size (p)", 
       ylab = expression(Delta[2]~"(Second Derivative of M"[p]*")"),
       main = "Second Derivative of M_p Curve (Inflection Point Detection)",
       lwd = 2, cex = 1.2,
       cex.lab = 1.2, cex.main = 1.3)
  grid()
  
  abline(h = 0, col = "gray30", lty = 2, lwd = 1.5)
  
  # Mark inflection point (minimum of delta2)
  inflection_p <- mp_result$p_star
  inflection_idx <- mp_result$inflection_index - 1
  
  if (inflection_idx >= 1 && inflection_idx <= length(delta2)) {
    points(inflection_p, delta2[inflection_idx], 
           col = "#E63946", pch = 8, cex = 2.5, lwd = 2)
    text(inflection_p, delta2[inflection_idx], 
         sprintf("p* = %d (min)", inflection_p),
         col = "#E63946", font = 2, pos = 3, cex = 1.1)
  }
  
  dev.off()
}


#' Create All Plots
#'
#' @param r2_curve Data frame
#' @param metric_results List
#' @param all_iterations List
#' @param all_r2_curves List of all R² curves
#' @param summary_stats Data frame
#' @param p_true Integer
#' @param output_dir String
#' @export
create_all_plots <- function(r2_curve, metric_results, all_iterations, 
                             all_r2_curves, summary_stats, p_true, output_dir) {
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  cat("  Creating visualizations...\n")
  
  plot_r2_and_mp_curves(
    r2_curve, metric_results, p_true,
    file.path(output_dir, "01_r2_and_mp_curves.png")
  )
  
  plot_criterion_comparison(
    r2_curve, metric_results, p_true,
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
    r2_curve, metric_results$M_p,
    file.path(output_dir, "08_second_derivative.png")
  )
  
  cat("  ✓ All plots saved to:", output_dir, "\n")
}
