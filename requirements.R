# requirements.R
# Install required packages for VMI Baseball Analytics Shiny App

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat("Installing package:", pkg, "\n")
      install.packages(pkg, dependencies = TRUE)
    } else {
      cat("Package already installed:", pkg, "\n")
    }
  }
}

# List of required packages
required_packages <- c(
  "shiny",
  "dplyr",
  "ggplot2",
  "purrr",
  "DT",
  "gridExtra",
  "patchwork",
  "rsconnect",
  "hexbin",
  "ggiraph",
  "httr2",
  "MASS",
  "curl",
  "readr",
  "lubridate",
  "stringr",
  "akima",
  "plotly",
  "RCurl",
  "jsonlite",
  "digest"
)


# Install packages
cat("Installing required packages for VMI Baseball Analytics...\n")
install_if_missing(required_packages)
cat("All packages installed successfully!\n")
