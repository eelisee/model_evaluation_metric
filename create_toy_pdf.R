# ============================================================================
# Create PDF with All Toy Example Sigmoid Plots
# ============================================================================

# Install png package if not available
if (!require("png", quietly = TRUE)) {
  install.packages("png", repos = "https://cloud.r-project.org")
}

library(png)
library(grid)

# Output PDF file
output_pdf <- "results/toy_examples/all_sigmoid_plots_10iterations.pdf" #all_metrics_plots_10iterations.pdf" #"results/toy_examples/all_sigmoid_plots.pdf"

# Get all scenario directories
toy_dir <- "results/toy_examples"
scenarios <- c("S1_Equal_Strong", "S2_Unequal_Strong", "S3_Five_Equal", 
               "S4_Five_Unequal", "S5_Seven_Unequal", "S6_Ten_Unequal",
               "S7_Ten_equal", "S8_Ten_Unequal_weird")

# Start PDF device
pdf(output_pdf, width = 11, height = 8.5)

for (i in seq_along(scenarios)) {
  scenario <- scenarios[i]
  scenario_name <- paste0("S", i)
  img_path <- file.path(toy_dir, scenario, "01_sigmoid_fit.png") #"03_criteria_comparison.png") # "01_sigmoid_fit.png")
  
  if (file.exists(img_path)) {
    cat(sprintf("Adding %s...\n", scenario_name))
    
    # Read PNG
    img <- readPNG(img_path)
    
    # Create new page
    grid.newpage()
    
    # Add title at top
    grid.text(sprintf("Scenario %s: %s", scenario_name, 
                     gsub("_", " ", scenario)),
              x = 0.5, y = 0.97, 
              gp = gpar(fontsize = 16, fontface = "bold"))
    
    # Display image
    grid.raster(img, x = 0.5, y = 0.48, width = 0.95, height = 0.90)
    
  } else {
    cat(sprintf("Warning: %s not found\n", img_path))
  }
}

dev.off()

cat(sprintf("\n✓ PDF created: %s\n", output_pdf))
cat(sprintf("  Contains %d scenario plots\n", length(scenarios)))
