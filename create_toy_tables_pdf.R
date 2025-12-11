# ============================================================================
# Create PDF with R²/M_p Tables for All Toy Examples
# ============================================================================

library(gridExtra)
library(grid)

# Output PDF file
output_pdf <- "results/toy_examples/all_data_tables.pdf"

# Get all scenario directories
toy_dir <- "results/toy_examples"
scenarios <- c("S1_Equal_Strong", "S2_Unequal_Strong", "S3_Five_Equal", 
               "S4_Five_Unequal", "S5_Seven_Unequal", "S6_Ten_Unequal",
               "S7_Ten_equal", "S8_Ten_Unequal_weird")

# Configuration for true p* values
true_p_stars <- c(3, 3, 5, 5, 7, 10, 10, 10)

# Read toy_example.R to extract selected p* values by running scenarios
# Alternative: Parse from console output or re-run the scenarios
# For now, we'll run a minimal version to get selected p*

# Function to get selected p* from scenario
get_selected_p_star <- function(scenario_idx) {
  # We need to re-run metric_sigmoid_mp on the data
  # Load functions
  source("R/01_data_generation.R")
  source("R/02_metrics.R")
  
  library(MASS)
  
  # Fixed parameters
  n <- 500
  p <- 10
  sigma_eps <- 0.2
  Sigma <- diag(p)
  
  # Scenario configurations
  configs <- list(
    list(support = c(1, 2, 3), beta_values = c(3.0, 3.0, 3.0)),
    list(support = c(1, 2, 3), beta_values = c(3.0, 2.0, 4.0)),
    list(support = c(1, 2, 3, 4, 5), beta_values = c(3.0, 3.0, 3.0, 3.0, 3.0)),
    list(support = c(1, 2, 3, 4, 5), beta_values = c(3.0, 2.0, 4.0, 5.0, 7.0)),
    list(support = c(1, 2, 3, 4, 5, 6, 7), beta_values = c(3.0, 2.0, 4.0, 5.0, 7.0, 10.0, 15.0)),
    list(support = 1:10, beta_values = c(3.0, 2.0, 4.0, 5.0, 7.0, 10.0, 15.0, 20.0, 25.0, 30.0)),
    list(support = 1:10, beta_values = rep(3.0, 10)),
    list(support = 1:10, beta_values = c(3.0, 2.0, 4.0, 5.0, 7.0, 10.0, 15.0, 20.0, 25.0, 30.0))
  )
  
  config <- configs[[scenario_idx]]
  
  # Generate beta vector
  beta <- rep(0, p)
  beta[config$support] <- config$beta_values
  
  # Generate data
  set.seed(123 + scenario_idx)  # Consistent seed per scenario
  X <- mvrnorm(n = n, mu = rep(0, p), Sigma = Sigma)
  epsilon <- rnorm(n, 0, sigma_eps)
  y <- X %*% beta + epsilon
  y <- as.vector(y)
  
  # Compute R² curve
  r2_curve <- compute_r2_curve(X, y, n_cores = 1)
  
  # Apply sigmoid
  result_sigmoid <- metric_sigmoid_mp(r2_curve, use_3param = FALSE)
  
  return(result_sigmoid$p_star)
}

# Get selected p* for all scenarios
cat("Computing selected p* values...\n")
selected_p_stars <- sapply(1:length(scenarios), function(i) {
  cat(sprintf("  Scenario %d...\n", i))
  get_selected_p_star(i)
})

# Start PDF device
pdf(output_pdf, width = 11, height = 8.5)

for (i in seq_along(scenarios)) {
  scenario <- scenarios[i]
  scenario_name <- paste0("S", i)
  csv_path <- file.path(toy_dir, scenario, "r2_mp_curve.csv")
  
  if (file.exists(csv_path)) {
    cat(sprintf("Processing %s...\n", scenario_name))
    
    # Read CSV
    data <- read.csv(csv_path)
    
    # Extract first three columns: p, R2, M_p
    table_data <- data[, 1:3]
    
    # Transpose: rows become columns
    transposed <- t(table_data)
    colnames(transposed) <- paste0("p=", table_data$p)
    
    # Round values for display
    transposed_rounded <- round(transposed, 4)
    
    # Convert to data frame for display
    df_display <- as.data.frame(transposed_rounded)
    rownames(df_display) <- c("p", "R²", "M_p")
    
    # Determine p* values
    true_p_star <- true_p_stars[i]
    selected_p <- selected_p_stars[i]
    
    # Create new page
    grid.newpage()
    
    # Add title
    title_text <- sprintf("Scenario %s: %s\nTrue p* = %d | Selected p* = %d", 
                         scenario_name, 
                         gsub("_", " ", scenario),
                         true_p_star,
                         selected_p)
    grid.text(title_text,
              x = 0.5, y = 0.95, 
              gp = gpar(fontsize = 14, fontface = "bold"))
    
    # Create table with highlighting for BOTH true and selected p*
    n_rows <- 3
    n_cols <- ncol(df_display)
    
    # Build fill colors: highlight both columns
    fill_matrix <- matrix("white", nrow = n_rows, ncol = n_cols)
    colhead_fill <- rep("lightblue", n_cols)
    
    # Mark true p* in light green
    if (!is.na(true_p_star) && true_p_star <= n_cols) {
      fill_matrix[, true_p_star] <- "#C6EFCE"  # Light green
      colhead_fill[true_p_star] <- "#90EE90"   # Lighter green for header
    }
    
    # Mark selected p* in light orange (may overlap with true p*)
    if (!is.na(selected_p) && selected_p <= n_cols) {
      if (selected_p == true_p_star) {
        # If they match, use a special color (light blue/cyan)
        fill_matrix[, selected_p] <- "#B4E7FF"
        colhead_fill[selected_p] <- "#87CEEB"
      } else {
        # Different color for selected
        fill_matrix[, selected_p] <- "#FFE5B4"  # Light orange/peach
        colhead_fill[selected_p] <- "#FFD700"   # Gold for header
      }
    }
    
    tt <- ttheme_default(
      core = list(
        bg_params = list(fill = fill_matrix),
        fg_params = list(fontsize = 10)
      ),
      colhead = list(
        bg_params = list(fill = colhead_fill),
        fg_params = list(fontsize = 11, fontface = "bold")
      ),
      rowhead = list(
        bg_params = list(fill = "lightgray"),
        fg_params = list(fontsize = 11, fontface = "bold")
      )
    )
    
    # Create table grob
    table_grob <- tableGrob(df_display, theme = tt)
    
    # Draw table
    grid.draw(table_grob)
    
    # Add legend explaining colors
    legend_y <- 0.08
    grid.rect(x = 0.25, y = legend_y, width = 0.03, height = 0.02, 
              gp = gpar(fill = "#C6EFCE", col = "black"))
    grid.text("True p*", x = 0.27, y = legend_y, just = "left",
              gp = gpar(fontsize = 9))
    
    grid.rect(x = 0.45, y = legend_y, width = 0.03, height = 0.02,
              gp = gpar(fill = "#FFE5B4", col = "black"))
    grid.text("Selected p*", x = 0.47, y = legend_y, just = "left",
              gp = gpar(fontsize = 9))
    
    grid.rect(x = 0.68, y = legend_y, width = 0.03, height = 0.02,
              gp = gpar(fill = "#B4E7FF", col = "black"))
    grid.text("Both match", x = 0.70, y = legend_y, just = "left",
              gp = gpar(fontsize = 9))
    
  } else {
    cat(sprintf("Warning: %s not found\n", csv_path))
  }
}

dev.off()

cat(sprintf("\n✓ PDF created: %s\n", output_pdf))
cat(sprintf("  Contains %d data tables\n", length(scenarios)))
