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
    cat("Starting deployment of Harvard app...\n")
    if (file.exists(".Renviron")) {
      readRenviron(".Renviron")
    }
    
    # Dependency setup
    if (file.exists("renv.lock")) {
      cat("renv.lock found; restoring exact dependencies from lockfile...\n")
      if (!requireNamespace("renv", quietly = TRUE)) {
        install.packages("renv", dependencies = TRUE)
      }
      renv::consent(provided = TRUE)
      restore_ok <- TRUE
      tryCatch({
        renv::restore(prompt = FALSE)
      }, error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("failed to find source for 'DT 0\\\\.34'", msg, ignore.case = TRUE)) {
          cat("Detected DT lockfile mismatch (0.34 vs 0.34.0); repairing lockfile and retrying restore...\n")
          renv::record("DT@0.34.0")
          renv::snapshot(prompt = FALSE)
          renv::restore(prompt = FALSE)
        } else {
          restore_ok <<- FALSE
          stop(e)
        }
      })
      if (!restore_ok) {
        stop("renv restore failed")
      }
      cat("✓ renv dependencies restored\n")
    } else {
      cat("No renv.lock found; running package installation fallback...\n")
      if (file.exists("install_packages.R")) {
        system2("Rscript", "install_packages.R", stdout = TRUE, stderr = TRUE)
      } else {
        cat("Warning: install_packages.R not found, installing packages manually...\n")

        required_packages <- c(
          "shiny", "shinyjs", "dplyr", "purrr", "ggplot2", "DT", "gridExtra",
          "patchwork", "hexbin", "ggiraph", "httr2", "MASS", "digest",
          "curl", "readr", "lubridate", "stringr", "akima", "colourpicker",
          "memoise", "shinymanager", "DBI", "RSQLite",
          "plotly", "jsonlite"
        )

        for (pkg in required_packages) {
          if (!requireNamespace(pkg, quietly = TRUE)) {
            cat("Installing package:", pkg, "\n")
            install.packages(pkg, dependencies = TRUE, quiet = TRUE)
          }
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
    
    # Deploy the app with better error handling.
    # shinyapps.io does not support deployApp(envVars=...).
    cat("Deploying to shinyapps.io...\n")
    if (!file.exists(".Renviron")) {
      cat("Warning: .Renviron not found; app may fall back to sqlite state backend in production.\n")
    }
    deployApp(
      appDir = ".",
      appName = "tmdata",
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
  cat("CBU - Deployment Script\n")
  cat("==========================================\n")
  success <- deploy_app()
  if (!success) {
    cat("Deployment failed. Exiting with error code 1.\n")
    quit(status = 1)
  } else {
    cat("Deployment completed successfully!\n")
  }
}
