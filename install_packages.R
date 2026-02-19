#!/usr/bin/env Rscript
# install_packages.R
# Script to install all required packages for VMI Baseball Analytics

# Set options for package installation
options(repos = c(CRAN = "https://cloud.r-project.org/"))
options(timeout = 300)

Sys.setenv(R_LIBS_USER = "packrat/lib-R")

cat("CBU Baseball Analytics - Package Installation\n")
cat("=============================================\n")

# Track installation failures
failed_packages <- c()

# Function to install packages with error handling
install_package_safe <- function(pkg, critical = TRUE) {
  tryCatch({
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat("Installing", pkg, "...\n")
      install.packages(pkg, dependencies = TRUE, quiet = FALSE)
      
      # Verify installation
      if (requireNamespace(pkg, quietly = TRUE)) {
        cat("✓ Successfully installed:", pkg, "\n")
        return(TRUE)
      } else {
        cat("✗ Installation reported success but package not available:", pkg, "\n")
        if (critical) failed_packages <<- c(failed_packages, pkg)
        return(FALSE)
      }
    } else {
      cat("✓ Already installed:", pkg, "\n")
      return(TRUE)
    }
  }, error = function(e) {
    cat("✗ Failed to install", pkg, ":", e$message, "\n")
    if (critical) failed_packages <<- c(failed_packages, pkg)
    return(FALSE)
  })
}

# Core packages (install first) - these MUST work
install_package_safe("shiny", critical = TRUE)
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  cat("Installing rsconnect...\n")
  install.packages("rsconnect", dependencies = TRUE)
}

# Core tidy packages (install individually rather than via meta tidyverse)
essential_packages <- c(
  "ggplot2",
  "dplyr", 
  "readr",
  "tibble",
  "stringr",
  "lubridate",
  "purrr",
  "shinyjs"
)

cat("\nInstalling core data manipulation packages...\n")
for (pkg in essential_packages) {
  install_package_safe(pkg, critical = TRUE)
}

# Additional packages for app functionality
app_packages <- c(
  "DT",
  "gridExtra",
  "patchwork",
  "hexbin",
  "httr2",
  "MASS",
  "curl",
  "RCurl",
  "akima",
  "plotly",
  "jsonlite",
  "ggiraph",
  "colourpicker",
  "memoise",
  "shinymanager",
  "DBI",
  "RSQLite",
  "RMariaDB",
  "RPostgres",
  "digest"
)

cat("\nInstalling app-specific packages...\n")
for (pkg in app_packages) {
  install_package_safe(pkg, critical = TRUE)
}

# Optional packages (nice to have but not critical)
optional_packages <- character(0)

if (length(optional_packages)) {
  cat("\nInstalling optional packages...\n")
  for (pkg in optional_packages) {
    install_package_safe(pkg, critical = FALSE)
  }
}

# Check for critical package failures
if (length(failed_packages) > 0) {
  cat("\n❌ Critical packages failed to install:\n")
  for (pkg in failed_packages) {
    cat("  - ", pkg, "\n")
  }
  cat("\nStopping installation due to critical package failures.\n")
  quit(status = 1)
}

# Verify installation by loading critical packages
cat("\nVerifying package installation...\n")

# Verify essential packages can be loaded
essential_verify <- c("shiny", "ggplot2", "dplyr", "purrr", "DT")
verification_failed <- FALSE

for (pkg in essential_verify) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat("✓ Successfully loaded:", pkg, "\n")
  }, error = function(e) {
    cat("✗ Failed to load:", pkg, "- Error:", e$message, "\n")
    verification_failed <- TRUE
  })
}

if (verification_failed) {
  cat("\n❌ Some critical packages failed to load.\n")
  cat("Please check the error messages above.\n")
  quit(status = 1)
} else {
  cat("\n🎉 All critical packages installed and verified successfully!\n")
  cat("Ready for deployment to shinyapps.io\n")
}
