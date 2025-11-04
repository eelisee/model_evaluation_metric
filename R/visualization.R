# Visualization Functions for M_p Model Evaluation
# =================================================
# Uses base R graphics - no external dependencies required


#' Plot M_p and R² Curves
#'
#' Creates a comprehensive visualization showing both R² and M_p as functions
#' of model complexity p, with the optimal point highlighted
#'
#' @param results Data frame. Output from evaluate_all_models()
#' @param p_star Integer. Optimal model complexity (optional)
#' @param save_path Character. Path to save the plot (optional)
#' @return ggplot object
#' @export
plot_mp_curves <- function(results, p_star = NULL, save_path = NULL) {
  
  # Get best models for each p
  best_models <- find_best_models_by_p(results)
  
  # Create dual-axis plot using facets instead
  # Prepare data for plotting
  plot_data <- data.frame(
    p = rep(best_models$p, 2),
    value = c(best_models$R2, best_models$Mp),
    metric = rep(c("R²", "M_p"), each = nrow(best_models)),
    stringsAsFactors = FALSE
  )
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = p, y = value, color = metric, group = metric)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    facet_wrap(~ metric, scales = "free_y", ncol = 1) +
    scale_x_continuous(breaks = unique(plot_data$p)) +
    scale_color_manual(values = c("R²" = "#2E86AB", "M_p" = "#A23B72")) +
    labs(
      title = "Model Selection: R² and M_p vs Model Complexity",
      subtitle = if (!is.null(p_star)) sprintf("Optimal complexity: p* = %d", p_star) else NULL,
      x = "Number of Predictors (p)",
      y = "Value",
      color = "Metric"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#A23B72", face = "bold"),
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 12),
      strip.background = element_rect(fill = "grey90", color = "grey70"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA)
    )
  
  # Add vertical line at optimal p if provided
  if (!is.null(p_star)) {
    p <- p + geom_vline(xintercept = p_star, linetype = "dashed", 
                        color = "#A23B72", linewidth = 1, alpha = 0.7)
  }
  
  # Save if path provided
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = 10, height = 8, dpi = 300)
    cat(sprintf("Plot saved to: %s\n", save_path))
  }
  
  return(p)
}


#' Plot M_p Curve Only
#'
#' Creates a focused plot of the M_p curve with optimal point highlighted
#'
#' @param results Data frame. Output from evaluate_all_models()
#' @param p_star Integer. Optimal model complexity (optional)
#' @param save_path Character. Path to save the plot (optional)
#' @return ggplot object
#' @export
plot_best_mp_curve <- function(results, p_star = NULL, save_path = NULL) {
  
  # Get best models for each p
  best_models <- find_best_models_by_p(results)
  
  # Create the plot
  p <- ggplot(best_models, aes(x = p, y = Mp)) +
    geom_line(color = "#A23B72", linewidth = 1.2) +
    geom_point(color = "#A23B72", size = 4) +
    scale_x_continuous(breaks = best_models$p) +
    labs(
      title = "M_p Efficiency Curve",
      subtitle = "Explained variance per parameter vs. model complexity",
      x = "Number of Predictors (p)",
      y = expression(M[p]~"="~R^2/p)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA)
    )
  
  # Highlight optimal point if provided
  if (!is.null(p_star)) {
    optimal_point <- best_models[best_models$p == p_star, ]
    
    p <- p + 
      geom_vline(xintercept = p_star, linetype = "dashed", 
                 color = "#E63946", linewidth = 1, alpha = 0.7) +
      geom_point(data = optimal_point, aes(x = p, y = Mp),
                 color = "#E63946", size = 6, shape = 21, 
                 fill = "#E63946", stroke = 2) +
      annotate("text", x = p_star, y = max(best_models$Mp) * 0.95,
               label = sprintf("p* = %d", p_star),
               color = "#E63946", fontface = "bold", size = 5, hjust = -0.2)
  }
  
  # Save if path provided
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = 10, height = 6, dpi = 300)
    cat(sprintf("Plot saved to: %s\n", save_path))
  }
  
  return(p)
}


#' Plot R² Curve
#'
#' Creates a plot showing how R² increases with model complexity
#'
#' @param results Data frame. Output from evaluate_all_models()
#' @param p_star Integer. Optimal model complexity (optional)
#' @param save_path Character. Path to save the plot (optional)
#' @return ggplot object
#' @export
plot_r2_curve <- function(results, p_star = NULL, save_path = NULL) {
  
  # Get best models for each p
  best_models <- find_best_models_by_p(results)
  
  # Create the plot
  p <- ggplot(best_models, aes(x = p, y = R2)) +
    geom_line(color = "#2E86AB", linewidth = 1.2) +
    geom_point(color = "#2E86AB", size = 4) +
    scale_x_continuous(breaks = best_models$p) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(
      title = "R² vs Model Complexity",
      subtitle = "Coefficient of determination for best model at each complexity level",
      x = "Number of Predictors (p)",
      y = expression(R^2)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA)
    )
  
  # Highlight optimal point if provided
  if (!is.null(p_star)) {
    optimal_point <- best_models[best_models$p == p_star, ]
    
    p <- p + 
      geom_vline(xintercept = p_star, linetype = "dashed", 
                 color = "#E63946", linewidth = 1, alpha = 0.7) +
      geom_point(data = optimal_point, aes(x = p, y = R2),
                 color = "#E63946", size = 6, shape = 21, 
                 fill = "#E63946", stroke = 2)
  }
  
  # Save if path provided
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = 10, height = 6, dpi = 300)
    cat(sprintf("Plot saved to: %s\n", save_path))
  }
  
  return(p)
}


#' Plot All Models (Scatter)
#'
#' Creates a scatter plot showing all evaluated models
#'
#' @param results Data frame. Output from evaluate_all_models()
#' @param p_star Integer. Optimal model complexity (optional)
#' @param save_path Character. Path to save the plot (optional)
#' @return ggplot object
#' @export
plot_all_models_scatter <- function(results, p_star = NULL, save_path = NULL) {
  
  # Get best models for overlay
  best_models <- find_best_models_by_p(results)
  
  # Create the plot
  p <- ggplot() +
    geom_point(data = results, aes(x = p, y = Mp), 
               alpha = 0.3, color = "grey50", size = 2) +
    geom_line(data = best_models, aes(x = p, y = Mp),
              color = "#A23B72", linewidth = 1.2) +
    geom_point(data = best_models, aes(x = p, y = Mp),
               color = "#A23B72", size = 4) +
    scale_x_continuous(breaks = unique(results$p)) +
    labs(
      title = "M_p Values for All Possible Models",
      subtitle = "Best model at each complexity level highlighted in purple",
      x = "Number of Predictors (p)",
      y = expression(M[p])
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA)
    )
  
  # Highlight optimal point if provided
  if (!is.null(p_star)) {
    optimal_point <- best_models[best_models$p == p_star, ]
    
    p <- p + 
      geom_vline(xintercept = p_star, linetype = "dashed", 
                 color = "#E63946", linewidth = 1, alpha = 0.7) +
      geom_point(data = optimal_point, aes(x = p, y = Mp),
                 color = "#E63946", size = 6, shape = 21, 
                 fill = "#E63946", stroke = 2)
  }
  
  # Save if path provided
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = 10, height = 6, dpi = 300)
    cat(sprintf("Plot saved to: %s\n", save_path))
  }
  
  return(p)
}


#' Create Comprehensive Visualization Dashboard
#'
#' Generates all key plots and saves them to a directory
#'
#' @param results Data frame. Output from evaluate_all_models()
#' @param optimal_result List. Output from find_optimal_model()
#' @param output_dir Character. Directory to save plots
#' @export
create_visualization_dashboard <- function(results, optimal_result, output_dir = "example_output/plots") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat(sprintf("Created directory: %s\n", output_dir))
  }
  
  p_star <- optimal_result$p_star
  
  # Generate all plots
  cat("Generating visualizations...\n")
  
  plot1 <- plot_mp_curves(results, p_star, 
                          file.path(output_dir, "mp_and_r2_curves.png"))
  
  plot2 <- plot_best_mp_curve(results, p_star,
                              file.path(output_dir, "mp_efficiency_curve.png"))
  
  plot3 <- plot_r2_curve(results, p_star,
                         file.path(output_dir, "r2_curve.png"))
  
  plot4 <- plot_all_models_scatter(results, p_star,
                                   file.path(output_dir, "all_models_scatter.png"))
  
  cat("\nAll visualizations generated successfully!\n")
  
  invisible(list(
    combined = plot1,
    mp_curve = plot2,
    r2_curve = plot3,
    scatter = plot4
  ))
}


#' Plot Comparison with AIC/BIC
#'
#' Creates a plot comparing M_p with AIC and BIC model selection
#'
#' @param results Data frame with AIC and BIC columns
#' @param p_star_mp Integer. Optimal p according to M_p
#' @param save_path Character. Path to save the plot (optional)
#' @return ggplot object
#' @export
plot_criterion_comparison <- function(results, p_star_mp = NULL, save_path = NULL) {
  
  # Find best models according to each criterion
  best_models <- find_best_models_by_p(results)
  
  # Find optimal according to AIC and BIC (minimum values)
  p_star_aic <- best_models$p[which.min(best_models$AIC)]
  p_star_bic <- best_models$p[which.min(best_models$BIC)]
  
  # Normalize metrics for comparison (0-1 scale)
  normalize <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  
  # For AIC/BIC, we want to invert since lower is better
  best_models$Mp_norm <- normalize(best_models$Mp)
  best_models$AIC_norm <- 1 - normalize(best_models$AIC)
  best_models$BIC_norm <- 1 - normalize(best_models$BIC)
  
  # Reshape for plotting
  plot_data <- data.frame(
    p = rep(best_models$p, 3),
    value = c(best_models$Mp_norm, best_models$AIC_norm, best_models$BIC_norm),
    criterion = rep(c("M_p", "AIC", "BIC"), each = nrow(best_models)),
    stringsAsFactors = FALSE
  )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = p, y = value, color = criterion, linetype = criterion)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = unique(plot_data$p)) +
    scale_color_manual(values = c("M_p" = "#A23B72", "AIC" = "#2E86AB", "BIC" = "#F18F01")) +
    scale_linetype_manual(values = c("M_p" = "solid", "AIC" = "dashed", "BIC" = "dotted")) +
    labs(
      title = "Model Selection Criteria Comparison",
      subtitle = sprintf("Optimal: M_p → p=%d, AIC → p=%d, BIC → p=%d", 
                        p_star_mp, p_star_aic, p_star_bic),
      x = "Number of Predictors (p)",
      y = "Normalized Score (higher is better)",
      color = "Criterion",
      linetype = "Criterion"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA)
    )
  
  # Save if path provided
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = 10, height = 6, dpi = 300)
    cat(sprintf("Plot saved to: %s\n", save_path))
  }
  
  return(p)
}
