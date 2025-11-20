# Visualization Functions for M_p Model Evaluation
# =================================================
# Uses base R graphics - no external dependencies required

# Check if ggplot2 is available
use_ggplot <- requireNamespace("ggplot2", quietly = TRUE)

if (use_ggplot) {
  library(ggplot2)
}


#' Plot M_p and R² Curves
#'
#' Creates a comprehensive visualization showing both R² and M_p as functions
#' of model complexity p, with the optimal point highlighted
#'
#' @param results Data frame. Output from evaluate_all_models()
#' @param p_star Integer. Optimal model complexity (optional)
#' @param save_path Character. Path to save the plot (optional)
#' @return NULL (displays plot)
#' @export
plot_mp_curves <- function(results, p_star = NULL, save_path = NULL) {
  
  # Get best models for each p
  best_models <- find_best_models_by_p(results)
  
  if (use_ggplot) {
    # Use ggplot2 if available
    plot_data <- data.frame(
      p = rep(best_models$p, 2),
      value = c(best_models$R2, best_models$Mp),
      metric = rep(c("R²", "M_p"), each = nrow(best_models)),
      stringsAsFactors = FALSE
    )
    
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
        y = "Value"
      ) +
      theme_minimal(base_size = 12)
    
    if (!is.null(p_star)) {
      p <- p + geom_vline(xintercept = p_star, linetype = "dashed", color = "#A23B72")
    }
    
    if (!is.null(save_path)) {
      ggsave(save_path, p, width = 10, height = 8, dpi = 300)
      #cat(sprintf("Plot saved to: %s\n", save_path))
    }
    
    return(p)
  } else {
    # Use base R graphics
    if (!is.null(save_path)) {
      png(save_path, width = 10, height = 8, units = "in", res = 300)
    }
    
    # Set up 2-panel plot
    par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
    
    # Plot R²
    plot(best_models$p, best_models$R2, type = "b", pch = 19, col = "#2E86AB",
         xlab = "Number of Predictors (p)", ylab = "R²",
         main = "R² vs Model Complexity", cex = 1.2, lwd = 2,
         ylim = c(0, 1), xaxt = "n")
    axis(1, at = best_models$p)
    grid()
    if (!is.null(p_star)) {
      abline(v = p_star, lty = 2, col = "#E63946", lwd = 2)
    }
    
    # Plot M_p
    plot(best_models$p, best_models$Mp, type = "b", pch = 19, col = "#A23B72",
         xlab = "Number of Predictors (p)", ylab = "M_p",
         main = "M_p Efficiency vs Model Complexity", cex = 1.2, lwd = 2,
         xaxt = "n")
    axis(1, at = best_models$p)
    grid()
    if (!is.null(p_star)) {
      abline(v = p_star, lty = 2, col = "#E63946", lwd = 2)
      text(p_star, max(best_models$Mp) * 0.95, sprintf("p* = %d", p_star),
           col = "#E63946", font = 2, pos = 4)
    }
    
    if (!is.null(save_path)) {
      dev.off()
      #cat(sprintf("Plot saved to: %s\n", save_path))
    }
    
    par(mfrow = c(1, 1))
    invisible(NULL)
  }
}


#' Plot M_p Curve Only
#'
#' Creates a focused plot of the M_p curve with optimal point highlighted
#'
#' @param results Data frame. Output from evaluate_all_models()
#' @param p_star Integer. Optimal model complexity (optional)
#' @param save_path Character. Path to save the plot (optional)
#' @return NULL (displays plot)
#' @export
plot_best_mp_curve <- function(results, p_star = NULL, save_path = NULL) {
  
  # Get best models for each p
  best_models <- find_best_models_by_p(results)
  
  if (use_ggplot) {
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
      theme_minimal(base_size = 12)
    
    if (!is.null(p_star)) {
      optimal_point <- best_models[best_models$p == p_star, ]
      p <- p + 
        geom_vline(xintercept = p_star, linetype = "dashed", color = "#E63946") +
        geom_point(data = optimal_point, aes(x = p, y = Mp),
                   color = "#E63946", size = 6)
    }
    
    if (!is.null(save_path)) {
      ggsave(save_path, p, width = 10, height = 6, dpi = 300)
      #cat(sprintf("Plot saved to: %s\n", save_path))
    }
    return(p)
  } else {
    if (!is.null(save_path)) {
      png(save_path, width = 10, height = 6, units = "in", res = 300)
    }
    
    plot(best_models$p, best_models$Mp, type = "b", pch = 19, col = "#A23B72",
         xlab = "Number of Predictors (p)", ylab = expression(M[p]),
         main = "M_p Efficiency Curve", cex = 1.5, lwd = 2, xaxt = "n")
    axis(1, at = best_models$p)
    grid()
    
    if (!is.null(p_star)) {
      abline(v = p_star, lty = 2, col = "#E63946", lwd = 2)
      optimal_point <- best_models[best_models$p == p_star, ]
      points(p_star, optimal_point$Mp, pch = 21, col = "#E63946", 
             bg = "#E63946", cex = 2.5, lwd = 2)
      text(p_star, max(best_models$Mp) * 0.95, sprintf("p* = %d", p_star),
           col = "#E63946", font = 2, pos = 4)
    }
    
    if (!is.null(save_path)) {
      dev.off()
      #cat(sprintf("Plot saved to: %s\n", save_path))
    }
    
    invisible(NULL)
  }
}


#' Plot R² Curve
#'
#' Creates a plot showing how R² increases with model complexity
#'
#' @param results Data frame. Output from evaluate_all_models()
#' @param p_star Integer. Optimal model complexity (optional)
#' @param save_path Character. Path to save the plot (optional)
#' @return NULL (displays plot)
#' @export
plot_r2_curve <- function(results, p_star = NULL, save_path = NULL) {
  
  # Get best models for each p
  best_models <- find_best_models_by_p(results)
  
  if (use_ggplot) {
    p <- ggplot(best_models, aes(x = p, y = R2)) +
      geom_line(color = "#2E86AB", linewidth = 1.2) +
      geom_point(color = "#2E86AB", size = 4) +
      scale_x_continuous(breaks = best_models$p) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
      labs(
        title = "R² vs Model Complexity",
        x = "Number of Predictors (p)",
        y = expression(R^2)
      ) +
      theme_minimal(base_size = 12)
    
    if (!is.null(p_star)) {
      optimal_point <- best_models[best_models$p == p_star, ]
      p <- p + 
        geom_vline(xintercept = p_star, linetype = "dashed", color = "#E63946") +
        geom_point(data = optimal_point, aes(x = p, y = R2),
                   color = "#E63946", size = 6)
    }
    
    if (!is.null(save_path)) {
      ggsave(save_path, p, width = 10, height = 6, dpi = 300)
      #cat(sprintf("Plot saved to: %s\n", save_path))
    }
    return(p)
  } else {
    if (!is.null(save_path)) {
      png(save_path, width = 10, height = 6, units = "in", res = 300)
    }
    
    plot(best_models$p, best_models$R2, type = "b", pch = 19, col = "#2E86AB",
         xlab = "Number of Predictors (p)", ylab = expression(R^2),
         main = "R² vs Model Complexity", cex = 1.5, lwd = 2,
         ylim = c(0, 1), xaxt = "n")
    axis(1, at = best_models$p)
    grid()
    
    if (!is.null(p_star)) {
      abline(v = p_star, lty = 2, col = "#E63946", lwd = 2)
      optimal_point <- best_models[best_models$p == p_star, ]
      points(p_star, optimal_point$R2, pch = 21, col = "#E63946", 
             bg = "#E63946", cex = 2.5, lwd = 2)
    }
    
    if (!is.null(save_path)) {
      dev.off()
      #cat(sprintf("Plot saved to: %s\n", save_path))
    }
    
    invisible(NULL)
  }
}


#' Plot All Models (Scatter)
#'
#' Creates a scatter plot showing all evaluated models
#'
#' @param results Data frame. Output from evaluate_all_models()
#' @param p_star Integer. Optimal model complexity (optional)
#' @param save_path Character. Path to save the plot (optional)
#' @return NULL (displays plot)
#' @export
plot_all_models_scatter <- function(results, p_star = NULL, save_path = NULL) {
  
  # Get best models for overlay
  best_models <- find_best_models_by_p(results)
  
  if (use_ggplot) {
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
        x = "Number of Predictors (p)",
        y = expression(M[p])
      ) +
      theme_minimal(base_size = 12)
    
    if (!is.null(p_star)) {
      optimal_point <- best_models[best_models$p == p_star, ]
      p <- p + 
        geom_vline(xintercept = p_star, linetype = "dashed", color = "#E63946") +
        geom_point(data = optimal_point, aes(x = p, y = Mp),
                   color = "#E63946", size = 6)
    }
    
    if (!is.null(save_path)) {
      ggsave(save_path, p, width = 10, height = 6, dpi = 300)
      #cat(sprintf("Plot saved to: %s\n", save_path))
    }
    return(p)
  } else {
    if (!is.null(save_path)) {
      png(save_path, width = 10, height = 6, units = "in", res = 300)
    }
    
    # Plot all models as scatter
    plot(results$p, results$Mp, pch = 19, col = rgb(0.5, 0.5, 0.5, 0.3),
         xlab = "Number of Predictors (p)", ylab = expression(M[p]),
         main = "M_p Values for All Possible Models", xaxt = "n")
    axis(1, at = unique(results$p))
    grid()
    
    # Overlay best models
    lines(best_models$p, best_models$Mp, col = "#A23B72", lwd = 2)
    points(best_models$p, best_models$Mp, pch = 19, col = "#A23B72", cex = 1.5)
    
    if (!is.null(p_star)) {
      abline(v = p_star, lty = 2, col = "#E63946", lwd = 2)
      optimal_point <- best_models[best_models$p == p_star, ]
      points(p_star, optimal_point$Mp, pch = 21, col = "#E63946", 
             bg = "#E63946", cex = 2.5, lwd = 2)
    }
    
    if (!is.null(save_path)) {
      dev.off()
      #cat(sprintf("Plot saved to: %s\n", save_path))
    }
    
    invisible(NULL)
  }
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
  
  plot_mp_curves(results, p_star, 
                 file.path(output_dir, "mp_and_r2_curves.png"))
  
  plot_best_mp_curve(results, p_star,
                     file.path(output_dir, "mp_efficiency_curve.png"))
  
  plot_r2_curve(results, p_star,
                file.path(output_dir, "r2_curve.png"))
  
  plot_all_models_scatter(results, p_star,
                          file.path(output_dir, "all_models_scatter.png"))
  
  cat("\nAll visualizations generated successfully!\n")
  
  invisible(NULL)
}


#' Plot Comparison with AIC/BIC
#'
#' Creates a plot comparing M_p with AIC and BIC model selection
#'
#' @param results Data frame with AIC and BIC columns
#' @param p_star_mp Integer. Optimal p according to M_p
#' @param save_path Character. Path to save the plot (optional)
#' @return NULL (displays plot)
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
  
  if (use_ggplot) {
    plot_data <- data.frame(
      p = rep(best_models$p, 3),
      value = c(best_models$Mp_norm, best_models$AIC_norm, best_models$BIC_norm),
      criterion = rep(c("M_p", "AIC", "BIC"), each = nrow(best_models)),
      stringsAsFactors = FALSE
    )
    
    p <- ggplot(plot_data, aes(x = p, y = value, color = criterion, linetype = criterion)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_x_continuous(breaks = unique(plot_data$p)) +
      scale_color_manual(values = c("M_p" = "#A23B72", "AIC" = "#2E86AB", "BIC" = "#F18F01")) +
      scale_linetype_manual(values = c("M_p" = "solid", "AIC" = "dashed", "BIC" = "dotted")) +
      labs(
        title = "Model Selection Criteria Comparison",
        subtitle = sprintf("Optimal: M_p -> p=%d, AIC -> p=%d, BIC -> p=%d", 
                          p_star_mp, p_star_aic, p_star_bic),
        x = "Number of Predictors (p)",
        y = "Normalized Score (higher is better)"
      ) +
      theme_minimal(base_size = 12)
    
    if (!is.null(save_path)) {
      ggsave(save_path, p, width = 10, height = 6, dpi = 300)
      #cat(sprintf("Plot saved to: %s\n", save_path))
    }
    return(p)
  } else {
    if (!is.null(save_path)) {
      png(save_path, width = 10, height = 6, units = "in", res = 300)
    }
    
    plot(best_models$p, best_models$Mp_norm, type = "b", pch = 19, col = "#A23B72",
         xlab = "Number of Predictors (p)", ylab = "Normalized Score",
         main = "Model Selection Criteria Comparison", lwd = 2, cex = 1.2,
         ylim = c(0, 1), xaxt = "n")
    axis(1, at = best_models$p)
    grid()
    
    lines(best_models$p, best_models$AIC_norm, type = "b", pch = 17, 
          col = "#2E86AB", lty = 2, lwd = 2, cex = 1.2)
    lines(best_models$p, best_models$BIC_norm, type = "b", pch = 15, 
          col = "#F18F01", lty = 3, lwd = 2, cex = 1.2)
    
    legend("topright", legend = c(
      sprintf("M_p (p*=%d)", p_star_mp),
      sprintf("AIC (p*=%d)", p_star_aic),
      sprintf("BIC (p*=%d)", p_star_bic)
    ), col = c("#A23B72", "#2E86AB", "#F18F01"),
    lty = c(1, 2, 3), pch = c(19, 17, 15), lwd = 2, cex = 0.9)
    
    if (!is.null(save_path)) {
      dev.off()
      #cat(sprintf("Plot saved to: %s\n", save_path))
    }
    
    invisible(NULL)
  }
}
