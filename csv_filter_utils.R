# CSV Filter Management Functions for GCU TrackMan Data
# Functions to manage excluded CSV files to prevent bad data from being downloaded

# Load exclusion list
load_csv_exclusions <- function(exclusion_file = "data/csv_exclusions.txt") {
  if (!file.exists(exclusion_file)) {
    cat("No exclusion file found at:", exclusion_file, "\n")
    return(character(0))
  }
  
  lines <- readLines(exclusion_file, warn = FALSE)
  # Remove comments and empty lines
  lines <- lines[!grepl("^\\s*#", lines) & nzchar(trimws(lines))]
  trimws(lines)
}

# Check if a CSV file should be excluded
should_exclude_csv <- function(filename, exclusions = NULL) {
  if (is.null(exclusions)) {
    exclusions <- load_csv_exclusions()
  }
  
  if (length(exclusions) == 0) return(FALSE)
  
  # Check for exact matches or partial matches
  for (pattern in exclusions) {
    if (grepl(pattern, filename, fixed = TRUE)) {
      cat("EXCLUDED:", filename, "(matches pattern:", pattern, ")\n")
      return(TRUE)
    }
  }
  FALSE
}

# Add a file to the exclusion list
add_csv_exclusion <- function(pattern, exclusion_file = "data/csv_exclusions.txt", comment = NULL) {
  # Create data directory if it doesn't exist
  dir.create("data", recursive = TRUE, showWarnings = FALSE)
  
  # Create file if it doesn't exist
  if (!file.exists(exclusion_file)) {
    writeLines(c(
      "# CSV Exclusion List for GCU TrackMan Data Sync",
      "# Add filenames or patterns (one per line) that should be excluded from automatic sync",
      "# Lines starting with # are comments and will be ignored",
      "# Examples:",
      "#   exact_filename.csv",
      "#   partial_match_pattern", 
      "#   BadData_2025-10-27",
      "#"
    ), exclusion_file)
  }
  
  # Read existing content
  lines <- readLines(exclusion_file, warn = FALSE)
  
  # Add new exclusion
  new_lines <- if (!is.null(comment)) {
    c(paste("#", comment), pattern)
  } else {
    pattern
  }
  
  # Append to file
  writeLines(c(lines, new_lines), exclusion_file)
  cat("Added exclusion pattern:", pattern, "\n")
  cat("Exclusion file updated:", exclusion_file, "\n")
}

# Remove a pattern from the exclusion list
remove_csv_exclusion <- function(pattern, exclusion_file = "data/csv_exclusions.txt") {
  if (!file.exists(exclusion_file)) {
    cat("Exclusion file not found:", exclusion_file, "\n")
    return(FALSE)
  }
  
  lines <- readLines(exclusion_file, warn = FALSE)
  original_count <- length(lines)
  
  # Remove lines that match the pattern (but keep comments)
  lines <- lines[!(trimws(lines) == pattern & !grepl("^\\s*#", lines))]
  
  if (length(lines) < original_count) {
    writeLines(lines, exclusion_file)
    cat("Removed exclusion pattern:", pattern, "\n")
    return(TRUE)
  } else {
    cat("Pattern not found in exclusion list:", pattern, "\n")
    return(FALSE)
  }
}

# List current exclusions
list_csv_exclusions <- function(exclusion_file = "data/csv_exclusions.txt") {
  exclusions <- load_csv_exclusions(exclusion_file)
  if (length(exclusions) == 0) {
    cat("No CSV exclusions currently configured.\n")
  } else {
    cat("Current CSV exclusions:\n")
    for (i in seq_along(exclusions)) {
      cat(sprintf("  %d. %s\n", i, exclusions[i]))
    }
  }
  invisible(exclusions)
}

# Interactive function to manage exclusions
manage_csv_exclusions <- function() {
  while (TRUE) {
    cat("\n=== GCU CSV Exclusion Management ===\n")
    cat("1. List current exclusions\n")
    cat("2. Add new exclusion\n")
    cat("3. Remove exclusion\n")
    cat("4. Exit\n")
    
    choice <- readline("Enter your choice (1-4): ")
    
    switch(choice,
      "1" = list_csv_exclusions(),
      "2" = {
        pattern <- readline("Enter filename or pattern to exclude: ")
        if (nzchar(trimws(pattern))) {
          comment <- readline("Enter optional comment (or press Enter to skip): ")
          comment <- if (nzchar(trimws(comment))) trimws(comment) else NULL
          add_csv_exclusion(trimws(pattern), comment = comment)
        }
      },
      "3" = {
        exclusions <- load_csv_exclusions()
        if (length(exclusions) > 0) {
          list_csv_exclusions()
          pattern <- readline("Enter the exact pattern to remove: ")
          if (nzchar(trimws(pattern))) {
            remove_csv_exclusion(trimws(pattern))
          }
        }
      },
      "4" = {
        cat("Exiting...\n")
        break
      },
      cat("Invalid choice. Please enter 1-4.\n")
    )
  }
}

# Helper function to check recently downloaded files for potential issues
check_recent_files <- function(hours_back = 24) {
  cat("Checking recently downloaded CSV files...\n")
  
  cutoff_time <- Sys.time() - (hours_back * 3600)
  csv_files <- list.files("data", pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  
  recent_files <- csv_files[file.mtime(csv_files) > cutoff_time]
  
  if (length(recent_files) == 0) {
    cat("No recent CSV files found in last", hours_back, "hours.\n")
    return(invisible(character(0)))
  }
  
  cat("Recent CSV files (last", hours_back, "hours):\n")
  for (file in recent_files) {
    info <- file.info(file)
    cat(sprintf("  %s (modified: %s, size: %d bytes)\n", 
                basename(file), 
                format(info$mtime, "%Y-%m-%d %H:%M:%S"),
                info$size))
  }
  
  invisible(recent_files)
}
