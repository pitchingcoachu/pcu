# deploy_script.R
# VMI Baseball App Deployment Script
# Deploys the Shiny app to shinyapps.io

# Set CRAN repository
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Load required libraries
suppressPackageStartupMessages({
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    install.packages("rsconnect", dependencies = TRUE)
  }
  library(rsconnect)
})

# Deploy to shinyapps.io
deploy_app <- function() {
  tryCatch({
    cat("Starting deployment of VMI Baseball Analytics app...\n")
    
    # Run package installation script
    cat("Running package installation script...\n")
    if (file.exists("install_packages.R")) {
      system2("Rscript", "install_packages.R", stdout = TRUE, stderr = TRUE)
    } else {
      cat("Warning: install_packages.R not found, installing packages manually...\n")
      
      # Install required packages if not already installed
      required_packages <- c(
        "shiny", "dplyr", "purrr", "ggplot2", "DT", "gridExtra", 
        "patchwork", "hexbin", "ggiraph", "httr2", "MASS", 
        "curl", "readr", "lubridate", "stringr", "akima", 
        "plotly", "RCurl", "jsonlite"
      )
      
      for (pkg in required_packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
          cat("Installing package:", pkg, "\n")
          install.packages(pkg, dependencies = TRUE, quiet = TRUE)
        }
      }
    }
    
    # Verify critical packages can be loaded
    cat("Verifying package loading...\n")
    critical_packages <- c("shiny", "dplyr", "purrr", "DT")
    for (pkg in critical_packages) {
      tryCatch({
        library(pkg, character.only = TRUE)
        cat("✓ Successfully loaded:", pkg, "\n")
      }, error = function(e) {
        cat("✗ Failed to load:", pkg, "- Error:", e$message, "\n")
        stop(paste("Critical package", pkg, "failed to load"))
      })
    }
    
    # Deploy the app with better error handling
    cat("Deploying to shinyapps.io...\n")
    deployApp(
      appDir = ".",
      appName = "vmibaseball",
      forceUpdate = TRUE,
      launch.browser = FALSE,
      logLevel = "verbose"
    )
    
    cat("✓ App deployed successfully!\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("✗ Deployment failed with error:", e$message, "\n")
    cat("Full error details:\n")
    print(e)
    return(FALSE)
  })
}

# Run deployment
if (!interactive()) {
  cat("VMI Baseball Analytics - Deployment Script\n")
  cat("==========================================\n")
  success <- deploy_app()
  if (!success) {
    cat("Deployment failed. Exiting with error code 1.\n")
    quit(status = 1)
  } else {
    cat("Deployment completed successfully!\n")
  }
}
