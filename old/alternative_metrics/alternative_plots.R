# Plotting Functions for Alternative Metrics
# ============================================

#' Plot comparison of M_p, M_p_minus, M_p_sqrt, and M_p_scaled
#'
#' @param best_models Data frame with Mp, Mp_minus, Mp_sqrt, Mp_scaled columns
#' @param p_star_mp Selected complexity for M_p
#' @param p_star_mp_minus Selected complexity for M_p_minus
#' @param p_star_mp_sqrt Selected complexity for M_p_sqrt
#' @param p_star_mp_scaled Selected complexity for M_p_scaled
#' @param save_path Path to save plot
plot_alternative_metrics_comparison <- function(best_models, p_star_mp, 
                                               p_star_mp_minus, p_star_mp_sqrt,
                                               p_star_mp_scaled, save_path = NULL) {
  
  # Check if ggplot2 is available
  use_ggplot <- requireNamespace("ggplot2", quietly = TRUE)
  
  if (!is.null(save_path)) {
    png(save_path, width = 1000, height = 600, res = 120)
  }
  
  if (use_ggplot) {
    library(ggplot2)
    
    # Reshape data for ggplot
    plot_data <- data.frame(
      p = rep(best_models$p, 4),
      value = c(best_models$Mp, best_models$Mp_minus, best_models$Mp_sqrt, best_models$Mp_scaled),
      metric = rep(c("M_p", "M_p_minus", "M_p_sqrt", "M_p_scaled"), each = nrow(best_models))
    )
    
    p <- ggplot(plot_data, aes(x = p, y = value, color = metric, group = metric)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      geom_vline(xintercept = p_star_mp, linetype = "dashed", color = "#F8766D", alpha = 0.5) +
      geom_vline(xintercept = p_star_mp_minus, linetype = "dashed", color = "#00BA38", alpha = 0.5) +
      geom_vline(xintercept = p_star_mp_sqrt, linetype = "dashed", color = "#619CFF", alpha = 0.5) +
      geom_vline(xintercept = p_star_mp_scaled, linetype = "dashed", color = "#C77CFF", alpha = 0.5) +
      labs(
        title = "Alternative Metrics Comparison",
        subtitle = sprintf("M_p: p*=%d, M_p_minus: p*=%s, M_p_sqrt: p*=%s, M_p_scaled: p*=%d",
                          p_star_mp, 
                          ifelse(is.na(p_star_mp_minus), "NA", as.character(p_star_mp_minus)),
                          ifelse(is.na(p_star_mp_sqrt), "NA", as.character(p_star_mp_sqrt)),
                          p_star_mp_scaled),
        x = "Model Complexity (p)",
        y = "Metric Value",
        color = "Metric"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)
      )
    
    print(p)
    
  } else {
    # Base R plotting
    par(mfrow = c(1, 1), mar = c(5, 5, 4, 2))
    
    y_max <- max(c(best_models$Mp, best_models$Mp_minus, best_models$Mp_sqrt, best_models$Mp_scaled), na.rm = TRUE)
    
    plot(best_models$p, best_models$Mp, type = "b", 
         col = "red", pch = 16, lwd = 2,
         xlab = "Model Complexity (p)", 
         ylab = "Metric Value",
         main = "Alternative Metrics Comparison",
         ylim = c(0, y_max * 1.1))
    
    lines(best_models$p, best_models$Mp_minus, type = "b", 
          col = "green", pch = 17, lwd = 2)
    
    lines(best_models$p, best_models$Mp_sqrt, type = "b", 
          col = "blue", pch = 15, lwd = 2)
    
    lines(best_models$p, best_models$Mp_scaled, type = "b", 
          col = "purple", pch = 18, lwd = 2)
    
    # Add vertical lines for selections
    abline(v = p_star_mp, lty = 2, col = "red", lwd = 1.5)
    if (!is.na(p_star_mp_minus)) {
      abline(v = p_star_mp_minus, lty = 2, col = "green", lwd = 1.5)
    }
    if (!is.na(p_star_mp_sqrt)) {
      abline(v = p_star_mp_sqrt, lty = 2, col = "blue", lwd = 1.5)
    }
    abline(v = p_star_mp_scaled, lty = 2, col = "purple", lwd = 1.5)
    
    legend("topright", 
           legend = c(sprintf("M_p (p*=%d)", p_star_mp),
                     sprintf("M_p_minus (p*=%s)", 
                            ifelse(is.na(p_star_mp_minus), "NA", as.character(p_star_mp_minus))),
                     sprintf("M_p_sqrt (p*=%s)", 
                            ifelse(is.na(p_star_mp_sqrt), "NA", as.character(p_star_mp_sqrt))),
                     sprintf("M_p_scaled (p*=%d)", p_star_mp_scaled)),
           col = c("red", "green", "blue", "purple"),
           lty = 1, pch = c(16, 17, 15, 18), lwd = 2, cex = 0.8)
  }
  
  if (!is.null(save_path)) {
    dev.off()
  }
}
