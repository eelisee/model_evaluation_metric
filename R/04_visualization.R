# Visualization Module
# =====================
# Creates all plots for analysis

#' Plot Metrics (M_p, AIC, BIC) Over Model Size
#'
#' Shows all metric curves together on one plot
#'
#' @param r2_curve Data frame with p, R2, AIC, BIC
#' @param filename String
#' @export
plot_metrics_over_p <- function(r2_curve, filename) {
  
  # Compute M_p and M_p_sqrt
  M_p <- r2_curve$R2 / r2_curve$p
  M_p_sqrt <- r2_curve$R2 / sqrt(r2_curve$p)
  
  png(filename, width = 1000, height = 600)
  par(mfrow = c(2, 2))
  
  # Plot 1: M_p
  plot(r2_curve$p, M_p, 
       type = "b", pch = 19, col = "blue",
       xlab = "Model Size (p)", ylab = "M_p = R²/p",
       main = "M_p Metric")
  
  # Plot 2: M_p_sqrt
  plot(r2_curve$p, M_p_sqrt,
       type = "b", pch = 19, col = "purple",
       xlab = "Model Size (p)", ylab = "M_p_sqrt = R²/√p",
       main = "M_p_sqrt Metric")
  
  # Plot 3: AIC
  plot(r2_curve$p, r2_curve$AIC,
       type = "b", pch = 19, col = "red",
       xlab = "Model Size (p)", ylab = "AIC",
       main = "AIC")
  
  # Plot 4: BIC
  plot(r2_curve$p, r2_curve$BIC,
       type = "b", pch = 19, col = "orange",
       xlab = "Model Size (p)", ylab = "BIC",
       main = "BIC")
  
  dev.off()
}


#' Plot Metric Values at Selected p* Across Iterations
#'
#' For each p, shows distribution of M_p values across iterations
#' where that p was selected as p*
#'
#' @param all_iterations List
#' @param all_r2_curves List of R² curves from each iteration
#' @param filename String
#' @export
plot_metric_values_at_p_star <- function(all_iterations, all_r2_curves, filename) {
  
  # Extract M_p selections
  df <- do.call(rbind, all_iterations)
  mp_selections <- df[df$metric == "M_p", ]
  
  # For each iteration, get the M_p value at selected p*
  metric_values <- list()
  
  for (i in 1:length(all_r2_curves)) {
    r2_curve <- all_r2_curves[[i]]
    p_star <- mp_selections$p_star[i]
    
    # Compute M_p curve
    M_p <- r2_curve$R2 / r2_curve$p
    
    # Get M_p value at p*
    M_p_at_p_star <- M_p[r2_curve$p == p_star]
    
    metric_values[[i]] <- data.frame(
      iteration = i,
      p_star = p_star,
      M_p_value = M_p_at_p_star
    )
  }
  
  metric_df <- do.call(rbind, metric_values)
  
  png(filename, width = 800, height = 600)
  
  # Scatter plot: p* on x-axis, M_p value on y-axis
  plot(metric_df$p_star, metric_df$M_p_value,
       pch = 19, col = rgb(0, 0, 1, 0.5),
       xlab = "Selected p*",
       ylab = "M_p Value at p*",
       main = "Distribution of M_p Values by Selected p*")
  
  # Add jitter for overlapping points
  points(jitter(metric_df$p_star, amount = 0.1), metric_df$M_p_value,
         pch = 19, col = rgb(0, 0, 1, 0.3), cex = 0.8)
  
  dev.off()
}


#' Plot R² Curve with Selected p* for Each Metric
#'
#' @param r2_curve Data frame. R² curve
#' @param metric_results List. Results from all metrics
#' @param p_true Integer. True model size
#' @param filename String. Output filename
#' @export
plot_r2_with_selections <- function(r2_curve, metric_results, p_true, filename) {
  
  png(filename, width = 800, height = 600)
  
  plot(r2_curve$p, r2_curve$R2, 
       type = "b", pch = 19, col = "black",
       xlab = "Model Size (p)", 
       ylab = "R²",
       main = "R² Curve with Metric Selections",
       ylim = c(0, 1))
  
  # Add true p
  abline(v = p_true, col = "green", lwd = 2, lty = 1)
  
  # Add selections
  colors <- c("blue", "purple", "red", "orange")
  for (i in seq_along(metric_results)) {
    res <- metric_results[[i]]
    abline(v = res$p_star, col = colors[i], lwd = 2, lty = 2)
  }
  
  legend("bottomright", 
         legend = c("True p", names(metric_results)),
         col = c("green", colors[1:length(metric_results)]),
         lty = c(1, rep(2, length(metric_results))),
         lwd = 2)
  
  dev.off()
}


#' Plot Distribution of p* Across Iterations
#'
#' @param all_iterations List. Evaluation results
#' @param p_true Integer. True model size
#' @param filename String. Output filename
#' @export
plot_p_star_distribution <- function(all_iterations, p_true, filename) {
  
  df <- do.call(rbind, all_iterations)
  metrics <- unique(df$metric)
  
  png(filename, width = 1000, height = 600)
  par(mfrow = c(2, 2))
  
  for (m in metrics) {
    subset <- df[df$metric == m, ]
    
    hist(subset$p_star, 
         breaks = seq(0.5, max(subset$p_star) + 0.5, by = 1),
         col = "lightblue",
         border = "black",
         xlab = "Selected p*",
         main = paste0("Distribution: ", m),
         xlim = c(0, max(df$p_star) + 1))
    
    abline(v = p_true, col = "red", lwd = 2)
  }
  
  dev.off()
}


#' Scatter Plot of p* Across Iterations
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
  colors <- c("blue", "purple", "red", "orange")
  
  png(filename, width = 800, height = 600)
  
  plot(NULL, xlim = c(1, max(df$iteration)), ylim = c(0, max(df$p_star)),
       xlab = "Iteration", ylab = "Selected p*",
       main = "p* Selections Across Iterations")
  
  abline(h = p_true, col = "green", lwd = 2)
  
  for (i in seq_along(metrics)) {
    m <- metrics[i]
    subset <- df[df$metric == m, ]
    points(subset$iteration, subset$p_star, 
           col = colors[i], pch = 19, cex = 0.8)
  }
  
  legend("topright", legend = metrics, col = colors, pch = 19)
  
  dev.off()
}


#' Plot Second Derivative (Δ₂)
#'
#' @param r2_curve Data frame
#' @param mp_result Result from metric_mp()
#' @param filename String
#' @export
plot_second_derivative <- function(r2_curve, mp_result, filename) {
  
  delta2 <- mp_result$delta2
  p_vals <- r2_curve$p[1:length(delta2)]
  
  png(filename, width = 800, height = 600)
  
  plot(p_vals, delta2, 
       type = "b", pch = 19, col = "purple",
       xlab = "Model Size (p)", 
       ylab = "Δ₂ (Second Derivative)",
       main = "Second Derivative of R² Curve")
  
  abline(h = 0, col = "gray", lty = 2)
  
  # Mark inflection point
  inflection_p <- mp_result$p_star
  points(inflection_p, delta2[mp_result$inflection_index - 1], 
         col = "red", pch = 8, cex = 2)
  
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
  
  png(filename, width = 1000, height = 600)
  par(mfrow = c(2, 2))
  
  for (m in metrics) {
    subset <- df[df$metric == m, ]
    
    hist(subset$error, 
         breaks = 20,
         col = "lightcoral",
         border = "black",
         xlab = "Error (p* - p_true)",
         main = paste0("Error Distribution: ", m))
    
    abline(v = 0, col = "blue", lwd = 2)
  }
  
  dev.off()
}


#' Plot Hit Rate Comparison
#'
#' @param summary_stats Data frame from compute_summary_statistics()
#' @param filename String
#' @export
plot_hit_rate <- function(summary_stats, filename) {
  
  png(filename, width = 800, height = 600)
  
  barplot(summary_stats$HitRate,
          names.arg = summary_stats$metric,
          col = "steelblue",
          ylab = "Hit Rate",
          main = "Hit Rate Comparison",
          ylim = c(0, 1))
  
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
  
  png(filename, width = 800, height = 600)
  
  barplot(t(mat), beside = TRUE,
          col = c("green", "blue", "red"),
          legend.text = c("Correct", "Underfit", "Overfit"),
          xlab = "Metric",
          ylab = "Count",
          main = "Classification Breakdown")
  
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
  
  plot_r2_with_selections(
    r2_curve, metric_results, p_true,
    file.path(output_dir, "01_r2_curve.png")
  )
  
  plot_metrics_over_p(
    r2_curve,
    file.path(output_dir, "02_metrics_over_p.png")
  )
  
  plot_p_star_distribution(
    all_iterations, p_true,
    file.path(output_dir, "03_p_star_distribution.png")
  )
  
  plot_p_star_scatter(
    all_iterations, p_true,
    file.path(output_dir, "04_p_star_scatter.png")
  )
  
  plot_metric_values_at_p_star(
    all_iterations, all_r2_curves,
    file.path(output_dir, "05_metric_values_at_p_star.png")
  )
  
  plot_second_derivative(
    r2_curve, metric_results$M_p,
    file.path(output_dir, "06_second_derivative.png")
  )
  
  plot_error_distribution(
    all_iterations,
    file.path(output_dir, "07_error_distribution.png")
  )
  
  plot_hit_rate(
    summary_stats,
    file.path(output_dir, "08_hit_rate.png")
  )
  
  plot_classification_breakdown(
    summary_stats,
    file.path(output_dir, "09_classification_breakdown.png")
  )
  
  cat("All plots saved to:", output_dir, "\n")
}
