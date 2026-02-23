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

    install_pkgs <- tolower(trimws(Sys.getenv(
      "DEPLOY_INSTALL_PACKAGES",
      ifelse(nzchar(Sys.getenv("CI", "")), "false", "true")
    ))) %in% c("1", "true", "yes")

    if (install_pkgs) {
      cat("Running package installation script...\n")
      if (file.exists("install_packages.R")) {
        system2("Rscript", "install_packages.R", stdout = TRUE, stderr = TRUE)
      } else {
        cat("Warning: install_packages.R not found, installing packages manually...\n")

        # Install required packages if not already installed
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
    } else {
      cat("Skipping package installation (DEPLOY_INSTALL_PACKAGES=false)\n")
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
    
    # Build a strict runtime-only file list to prevent non-app scripts from inflating
    # bundle size and dependency discovery on shinyapps build.
    core_files <- c(
      "app.R",
      ".Renviron",
      "DESCRIPTION",
      "auth_db_config.yml",
      "credentials.sqlite",
      "lookup_table.csv",
      "video_map_helpers.R",
      file.path("config", "school_config.R")
    )
    data_files <- if (dir.exists("data")) {
      list.files("data", full.names = TRUE, recursive = FALSE, no.. = TRUE)
    } else character(0)
    www_files <- if (dir.exists("www")) {
      list.files("www", full.names = TRUE, recursive = TRUE, no.. = TRUE)
    } else character(0)

    deployment_files <- unique(c(core_files, data_files, www_files))
    deployment_files <- deployment_files[file.exists(deployment_files)]
    deployment_files <- deployment_files[!grepl("/\\.DS_Store$|^\\.DS_Store$", deployment_files)]
    deployment_files <- gsub("^\\./", "", deployment_files)

    include_video_map_csv <- tolower(trimws(Sys.getenv("INCLUDE_VIDEO_MAP_CSV", "false"))) %in% c("1", "true", "yes")
    if (!include_video_map_csv) {
      deployment_files <- deployment_files[deployment_files != file.path("data", "video_map.csv")]
      cat("Excluding data/video_map.csv from bundle (INCLUDE_VIDEO_MAP_CSV=false)\n")
    }

    file_info <- file.info(deployment_files)
    total_bytes <- sum(as.numeric(file_info$size), na.rm = TRUE)
    cat("Deployment file count:", length(deployment_files), "\n")
    cat("Deployment total bytes:", total_bytes, "\n")

    # Deploy the app with better error handling
    cat("Deploying to shinyapps.io...\n")
    deployApp(
      appDir = ".",
      appName = "tmdata",
      appFiles = deployment_files,
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
