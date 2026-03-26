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
      lock_text <- paste(readLines("renv.lock", warn = FALSE), collapse = "\n")
      if (grepl("\"DT\"\\s*:\\s*\\{[^\\}]*\"Version\"\\s*:\\s*\"0\\.34\"", lock_text, perl = TRUE)) {
        cat("Normalizing DT version in renv.lock (0.34 -> 0.34.0) before restore...\n")
        lock_text <- sub(
          "(\"DT\"\\s*:\\s*\\{[^\\}]*\"Version\"\\s*:\\s*\")0\\.34(\")",
          "\\10.34.0\\2",
          lock_text,
          perl = TRUE
        )
      }
      if (grepl("\"curl\"\\s*:\\s*\\{[^\\}]*\"Version\"\\s*:\\s*\"5\\.2\\.1\"", lock_text, perl = TRUE)) {
        cat("Normalizing curl version in renv.lock (5.2.1 -> 7.0.0) before restore...\n")
        lock_text <- sub(
          "(\"curl\"\\s*:\\s*\\{[^\\}]*\"Version\"\\s*:\\s*\")5\\.2\\.1(\")",
          "\\17.0.0\\2",
          lock_text,
          perl = TRUE
        )
      }
      writeLines(lock_text, "renv.lock", useBytes = TRUE)
      tryCatch({
        renv::restore(prompt = FALSE)
      }, error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("failed to find source for 'DT 0\\.34'", msg, ignore.case = TRUE)) {
          cat("Detected DT lockfile mismatch during restore; forcing DT@0.34.0 and retrying...\n")
          renv::record("DT@0.34.0")
          renv::snapshot(prompt = FALSE)
          renv::restore(prompt = FALSE)
        } else {
          stop(e)
        }
      })
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
    Sys.setenv(RENV_CONFIG_SNAPSHOT_VALIDATE = "FALSE")

    # Build an explicit deploy file list so non-runtime scripts are never bundled.
    all_files <- list.files(
      path = ".",
      recursive = TRUE,
      all.files = TRUE,
      include.dirs = FALSE,
      no.. = TRUE
    )
    keep <- all_files
    keep <- keep[!grepl("^\\.git/", keep)]
    keep <- keep[!grepl("^\\.github/", keep)]
    keep <- keep[!grepl("^docs/", keep)]
    keep <- keep[!grepl("^packrat/", keep)]
    keep <- keep[!grepl("^data/practice/", keep)]
    keep <- keep[!grepl("^data/v3/", keep)]
    keep <- keep[!grepl("^scripts/", keep)]
    keep <- keep[!grepl("^deploy_script\\.R$", keep)]
    keep <- keep[!grepl("^automated_data_sync\\.R$", keep)]
    keep <- keep[!grepl("^backup_modifications\\.R$", keep)]
    keep <- keep[!grepl("\\.md$", keep, ignore.case = TRUE)]
    keep <- keep[file.exists(keep)]

    info <- file.info(keep)
    total_mb <- sum(info$size, na.rm = TRUE) / (1024^2)
    cat(sprintf("Deploying %d files (%.2f MB uncompressed)\n", length(keep), total_mb))

    deployApp(
      appDir = ".",
      appFiles = keep,
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
