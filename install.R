# Installation and Setup Guide
# =============================

cat("M_p Model Evaluation Metric - Installation Guide\n")
cat("=================================================\n\n")

# Step 1: Check R version
cat("Step 1: Checking R version...\n")
r_version <- R.version.string
cat(sprintf("  %s\n", r_version))

if (R.version$major < 4) {
  warning("R version 4.0.0 or higher is recommended")
} else {
  cat("  ✓ R version is sufficient\n")
}
cat("\n")

# Step 2: Install required packages
cat("Step 2: Installing required packages...\n")

required_packages <- c("ggplot2")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("  Installing %s...\n", pkg))
    install.packages(pkg, quiet = TRUE)
    cat(sprintf("  ✓ %s installed successfully\n", pkg))
  } else {
    cat(sprintf("  ✓ %s already installed\n", pkg))
  }
}

cat("\n")

# Step 3: Verify installation
cat("Step 3: Verifying installation...\n")

all_installed <- TRUE
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("  ✗ Failed to load %s\n", pkg))
    all_installed <- FALSE
  } else {
    cat(sprintf("  ✓ %s loaded successfully\n", pkg))
  }
}

cat("\n")

# Step 4: Check project structure
cat("Step 4: Checking project structure...\n")

required_files <- c(
  "R/mp_functions.R",
  "R/visualization.R",
  "R/utils.R",
  "main_analysis.R",
  "example.R",
  "README.md"
)

all_files_present <- TRUE
for (file in required_files) {
  if (file.exists(file)) {
    cat(sprintf("  ✓ %s\n", file))
  } else {
    cat(sprintf("  ✗ Missing: %s\n", file))
    all_files_present <- FALSE
  }
}

cat("\n")

# Final status
cat(paste(rep("=", 60), collapse = ""), "\n")
if (all_installed && all_files_present) {
  cat("Installation Complete!\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  
  cat("Quick Start:\n")
  cat("  1. Run the simple example:\n")
  cat("       source('example.R')\n\n")
  cat("  2. Run the full analysis:\n")
  cat("       source('main_analysis.R')\n\n")
  cat("  3. Or use the quick analysis function:\n")
  cat("       source('R/mp_functions.R')\n")
  cat("       source('R/visualization.R')\n")
  cat("       source('R/utils.R')\n")
  cat("       results <- quick_analysis()\n\n")
  
} else {
  cat("Installation Issues Detected\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  
  if (!all_installed) {
    cat("Please install missing packages manually:\n")
    cat("  install.packages(c('ggplot2'))\n\n")
  }
  
  if (!all_files_present) {
    cat("Please ensure all project files are present.\n\n")
  }
}

cat("For help, see README.md\n\n")
