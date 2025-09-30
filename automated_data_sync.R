# automated_data_sync.R
# VMI Baseball Data Automation Script
# Syncs data from TrackMan FTP, filters for VMI only

library(RCurl)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# FTP credentials
FTP_HOST <- "ftp.trackmanbaseball.com"
FTP_USER <- "VMI"
FTP_PASS <- "q7MvFhmAEN"

# Local data directories
LOCAL_DATA_DIR      <- "data/"
LOCAL_PRACTICE_DIR  <- file.path(LOCAL_DATA_DIR, "practice")
LOCAL_V3_DIR        <- file.path(LOCAL_DATA_DIR, "v3")

# Ensure data directories exist
dir.create(LOCAL_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOCAL_PRACTICE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOCAL_V3_DIR, recursive = TRUE, showWarnings = FALSE)

# Function to list files in FTP directory
list_ftp_files <- function(ftp_path) {
  url <- paste0("ftp://", FTP_USER, ":", FTP_PASS, "@", FTP_HOST, ftp_path)
  tryCatch({
    files <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
    strsplit(files, "\n")[[1]]
  }, error = function(e) {
    cat("Error listing files in", ftp_path, ":", e$message, "\n")
    character(0)
  })
}

# Function to download CSV file (no filtering - app will handle filtering)
download_csv <- function(remote_file, local_file) {
  # Skip if file already exists (incremental sync)
  if (file.exists(local_file)) {
    cat("Skipping existing file:", basename(local_file), "\n")
    return(FALSE)  # Return FALSE so we don't count it as newly downloaded
  }
  
  url <- paste0("ftp://", FTP_USER, ":", FTP_PASS, "@", FTP_HOST, remote_file)
  
  tryCatch({
    # Download file to temporary location
    temp_file <- tempfile(fileext = ".csv")
    download.file(url, temp_file, method = "curl", quiet = TRUE)
    
    # Read data to check if valid
    data <- read_csv(temp_file, show_col_types = FALSE)
    
    # Only save if we have data
    if (nrow(data) > 0) {
      write_csv(data, local_file)
      cat("Downloaded", nrow(data), "rows to", local_file, "\n")
      unlink(temp_file)
      return(TRUE)
    } else {
      cat("No data found in", remote_file, "\n")
      unlink(temp_file)
      return(FALSE)
    }
    
  }, error = function(e) {
    cat("Error processing", remote_file, ":", e$message, "\n")
    return(FALSE)
  })
}

# Function to sync practice data (2025 folder with MM/DD structure)
sync_practice_data <- function() {
  cat("Syncing practice data...\n")
  practice_base_path <- "/practice/2025/"
  
  # Get all subdirectories (month folders)
  months <- list_ftp_files(practice_base_path)
  month_dirs <- months[grepl("^\\d{2}$", months)]  # Match MM format
  
  downloaded_count <- 0
  
  for (month_dir in month_dirs) {
    month_path <- paste0(practice_base_path, month_dir, "/")
    cat("Checking practice month:", month_dir, "\n")
    
    # Get day folders in this month
    days <- list_ftp_files(month_path)
    day_dirs <- days[grepl("^\\d{2}$", days)]  # Match DD format
    
    for (day_dir in day_dirs) {
      day_path <- paste0(month_path, day_dir, "/")
      cat("Processing practice date: 2025/", month_dir, "/", day_dir, "\n")
      
      # Look for CSV files directly in the day folder (no CSV subdirectory)
      files_in_day <- list_ftp_files(day_path)
      csv_files <- files_in_day[grepl("\\.csv$", files_in_day, ignore.case = TRUE)]
      
      # Filter out files with "playerpositioning" in the name (allow unverified)
      csv_files <- csv_files[!grepl("playerpositioning", csv_files, ignore.case = TRUE)]
      
      for (file in csv_files) {
        remote_path <- paste0(day_path, file)
        local_path <- file.path(LOCAL_PRACTICE_DIR, paste0("practice_", month_dir, "_", day_dir, "_", file))
        
        if (download_csv(remote_path, local_path)) {
          downloaded_count <- downloaded_count + 1
        }
        
        # Small delay between files
        Sys.sleep(0.1)
      }
    }
  }
  
  cat("Practice sync complete:", downloaded_count, "files downloaded\n")
  return(downloaded_count > 0)
}

# Function to check if file date is in allowed ranges
is_date_in_range <- function(file_path) {
  # Extract date from file path (YYYY/MM/DD pattern)
  date_match <- stringr::str_match(file_path, "(20\\d{2})/(0[1-9]|1[0-2])/(0[1-9]|[12]\\d|3[01])")
  
  if (is.na(date_match[1])) {
    # If no date pattern found, include the file (safer approach)
    return(TRUE)
  }
  
  file_date <- as.Date(paste(date_match[2], date_match[3], date_match[4], sep = "-"))
  
  # Start date: August 1, 2025 (nothing before this)
  start_date <- as.Date("2025-08-01")
  
  # Include all data from August 1, 2025 onwards (no future year restrictions)
  return(file_date >= start_date)
}

# Function to sync v3 data with date filtering
sync_v3_data <- function() {
  cat("Syncing v3 data with date filtering...\n")
  v3_base_path <- "/v3/2025/"
  
  # Get all subdirectories (month folders)
  months <- list_ftp_files(v3_base_path)
  month_dirs <- months[grepl("^\\d{2}$", months)]  # Match MM format
  
  downloaded_count <- 0
  
  for (month_dir in month_dirs) {
    month_path <- paste0(v3_base_path, month_dir, "/")
    cat("Checking month:", month_dir, "\n")
    
    # Get day folders in this month
    days <- list_ftp_files(month_path)
    day_dirs <- days[grepl("^\\d{2}$", days)]  # Match DD format
    
    for (day_dir in day_dirs) {
      day_path <- paste0(month_path, day_dir, "/")
      full_date_path <- paste0("2025/", month_dir, "/", day_dir)
      
      # Check if this date is in our allowed ranges
      if (!is_date_in_range(full_date_path)) {
        next  # Skip this date
      }
      
      cat("Processing date: 2025/", month_dir, "/", day_dir, "\n")
      
      # Look for CSV folder or direct CSV files
      files_in_day <- list_ftp_files(day_path)
      
      # Check if there's a CSV subdirectory
      if ("CSV" %in% files_in_day) {
        csv_path <- paste0(day_path, "CSV/")
        csv_files <- list_ftp_files(csv_path)
        csv_files <- csv_files[grepl("\\.csv$", csv_files, ignore.case = TRUE)]
        
        # Filter out files with "playerpositioning" or "unverified" in v3 folder
        csv_files <- csv_files[!grepl("playerpositioning|unverified", csv_files, ignore.case = TRUE)]
        
        for (file in csv_files) {
          remote_path <- paste0(csv_path, file)
          local_path <- file.path(LOCAL_V3_DIR, paste0("v3_", month_dir, "_", day_dir, "_", file))
          
          if (download_csv(remote_path, local_path)) {
            downloaded_count <- downloaded_count + 1
          }
          
          # Small delay between files
          Sys.sleep(0.1)
        }
      } else {
        # Look for CSV files directly in the day folder
        csv_files <- files_in_day[grepl("\\.csv$", files_in_day, ignore.case = TRUE)]
        
        # Filter out files with "playerpositioning" or "unverified" in v3 folder
        csv_files <- csv_files[!grepl("playerpositioning|unverified", csv_files, ignore.case = TRUE)]
        
        for (file in csv_files) {
          remote_path <- paste0(day_path, file)
          local_path <- file.path(LOCAL_V3_DIR, paste0("v3_", month_dir, "_", day_dir, "_", file))
          
          if (download_csv(remote_path, local_path)) {
            downloaded_count <- downloaded_count + 1
          }
          
          # Small delay between files
          Sys.sleep(0.1)
        }
      }
    }
  }
  
  cat("V3 sync complete:", downloaded_count, "files downloaded\n")
  return(downloaded_count > 0)
}

# Function to remove duplicate data across all CSV files
deduplicate_files <- function() {
  cat("Starting deduplication process...\n")
  
  # Get all CSV files in the data directory
  csv_files <- list.files(LOCAL_DATA_DIR, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  
  if (length(csv_files) == 0) {
    cat("No CSV files found for deduplication\n")
    return(FALSE)
  }
  
  all_data <- list()
  file_sources <- list()
  
  # Read all CSV files and track source
  for (file in csv_files) {
    tryCatch({
      # Read all columns as character to avoid type conflicts
      data <- read_csv(file, show_col_types = FALSE, col_types = cols(.default = "c"))
      if (nrow(data) > 0) {
        # Add source file info for tracking
        data$SourceFile <- basename(file)
        all_data[[length(all_data) + 1]] <- data
        file_sources[[length(file_sources) + 1]] <- file
      }
    }, error = function(e) {
      cat("Error reading", file, ":", e$message, "\n")
    })
  }
  
  if (length(all_data) == 0) {
    cat("No valid data found in CSV files\n")
    return(FALSE)
  }
  
  # Combine all data
  combined_data <- bind_rows(all_data)
  
  # Create deduplication key based on available columns
  # Use common columns that should be unique per pitch
  key_cols <- c("Date", "Pitcher", "Batter", "PitchNo", "PlateLocSide", "PlateLocHeight", 
                "RelSpeed", "TaggedPitchType", "Balls", "Strikes")
  
  # Only use columns that actually exist
  available_key_cols <- intersect(key_cols, names(combined_data))
  
  if (length(available_key_cols) == 0) {
    cat("Warning: No key columns found for deduplication. Using all columns.\n")
    available_key_cols <- names(combined_data)[!names(combined_data) %in% "SourceFile"]
  }
  
  # Remove duplicates, keeping the first occurrence
  original_count <- nrow(combined_data)
  deduplicated_data <- combined_data %>%
    distinct(across(all_of(available_key_cols)), .keep_all = TRUE)
  
  duplicates_removed <- original_count - nrow(deduplicated_data)
  
  if (duplicates_removed > 0) {
    cat("Removed", duplicates_removed, "duplicate rows\n")
    
    # Split back by source and rewrite files
    for (source_file in unique(deduplicated_data$SourceFile)) {
      file_data <- deduplicated_data %>%
        filter(SourceFile == source_file) %>%
        select(-SourceFile)
      
      if (nrow(file_data) > 0) {
        # Find the original file path
        original_path <- csv_files[basename(csv_files) == source_file]
        if (length(original_path) == 1) {
          write_csv(file_data, original_path)
          cat("Rewrote", original_path, "with", nrow(file_data), "unique rows\n")
        }
      }
    }
  } else {
    cat("No duplicates found\n")
  }
  
  return(duplicates_removed > 0)
}

# Main sync function
main_sync <- function() {
  cat("Starting VMI data sync at", as.character(Sys.time()), "\n")
  
  start_time <- Sys.time()
  
  # Only clean old files if this is the first run (no last_sync.txt exists)
  # This prevents re-downloading everything on subsequent runs
  last_sync_file <- file.path(LOCAL_DATA_DIR, "last_sync.txt")
  if (!file.exists(last_sync_file)) {
    cat("First run detected - cleaning old data files\n")
    old_files <- list.files(LOCAL_DATA_DIR, pattern = "\\.(csv|txt)$", full.names = TRUE, recursive = TRUE)
    if (length(old_files) > 0) {
      file.remove(old_files)
      cat("Cleaned", length(old_files), "old data files\n")
    }
  } else {
    cat("Incremental sync - keeping existing files\n")
  }
  
  # Sync both data sources
  practice_updated <- sync_practice_data()
  v3_updated <- sync_v3_data()
  
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "mins")
  
  cat("Data sync completed in", round(duration, 2), "minutes\n")
  
  # Deduplicate downloaded files if any data was updated
  if (practice_updated || v3_updated) {
    deduplicate_files()
  }
  
  # Update last sync timestamp
  writeLines(as.character(Sys.time()), file.path(LOCAL_DATA_DIR, "last_sync.txt"))
  
  # Return TRUE if any data was updated
  return(practice_updated || v3_updated)
}

# Run if called directly
if (!interactive()) {
  data_updated <- main_sync()
  # Always exit successfully - no new data is normal for incremental sync
  if (!data_updated) {
    cat("No new data found during sync - this is normal for incremental sync\n")
  } else {
    cat("New data was downloaded and processed\n")
  }
  cat("Sync completed successfully\n")
}
