# Enhanced automated_data_sync.R
# This version creates a flag file when new data is synced, 
# which helps the app detect when to reapply pitch type modifications

# Copy this content to your automated_data_sync.R file

library(curl)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# Load CSV filtering utilities
source("csv_filter_utils.R")
source("video_map_helpers.R")
# FTP credentials

FTP_HOST <- "ftp.trackmanbaseball.com"
FTP_USER <- "Jared Gaynor"
FTP_PASS <- "Wev4SdE2a8"
# When passwords contain special characters like @ or %, don't embed them in the URL.
FTP_USERPWD <- paste0(FTP_USER, ":", FTP_PASS)

# Local data directories
LOCAL_DATA_DIR <- "data/"
USE_DATA_FOLDER_CACHE <- tolower(trimws(Sys.getenv("USE_DATA_FOLDER_CACHE", "false"))) %in% c("1", "true", "yes")
CSV_CACHE_ROOT <- if (USE_DATA_FOLDER_CACHE) {
  LOCAL_DATA_DIR
} else {
  path.expand(Sys.getenv("CSV_CACHE_ROOT", "~/pcu_sync_cache"))
}
LOCAL_PRACTICE_DIR <- file.path(CSV_CACHE_ROOT, "practice")
LOCAL_V3_DIR <- file.path(CSV_CACHE_ROOT, "v3")

# Ensure data directories exist
dir.create(LOCAL_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(CSV_CACHE_ROOT, recursive = TRUE, showWarnings = FALSE)
dir.create(LOCAL_PRACTICE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOCAL_V3_DIR, recursive = TRUE, showWarnings = FALSE)
cat("CSV cache root:", CSV_CACHE_ROOT, "\n")

get_sync_dates <- function() {
  fallback_lookback <- suppressWarnings(as.integer(Sys.getenv("SYNC_LOOKBACK_DAYS", "14")))
  if (!is.finite(fallback_lookback) || fallback_lookback < 1) fallback_lookback <- 14
  backfill_days <- suppressWarnings(as.integer(Sys.getenv("SYNC_BACKFILL_DAYS", "2")))
  if (!is.finite(backfill_days) || backfill_days < 0) backfill_days <- 2

  last_sync_file <- file.path(LOCAL_DATA_DIR, "last_sync.txt")
  start_date <- Sys.Date() - fallback_lookback + 1

  if (file.exists(last_sync_file)) {
    raw <- trimws(readLines(last_sync_file, warn = FALSE))
    if (length(raw) > 0 && nzchar(raw[1])) {
      parsed_time <- suppressWarnings(as.POSIXct(raw[1], tz = "UTC"))
      if (!is.na(parsed_time)) {
        start_date <- as.Date(parsed_time, tz = "UTC") - backfill_days
      }
    }
  }

  min_date <- as.Date("2025-08-10")
  if (start_date < min_date) start_date <- min_date
  if (start_date > Sys.Date()) start_date <- Sys.Date()

  seq.Date(start_date, Sys.Date(), by = "day")
}

# Function to list files in FTP directory
list_ftp_files <- function(ftp_path) {
  url <- paste0("ftp://", FTP_HOST, ftp_path)
  tryCatch({
    handle <- curl::new_handle(userpwd = FTP_USERPWD)
    # Request names-only directory listings so regex checks (MM/DD/CSV) work reliably.
    curl::handle_setopt(handle, ftp_use_epsv = FALSE, dirlistonly = TRUE)
    contents <- curl::curl_fetch_memory(url, handle = handle)$content
    files <- rawToChar(contents)
    entries <- strsplit(files, "\n", fixed = TRUE)[[1]]
    entries <- trimws(gsub("\r", "", entries, fixed = TRUE))
    entries[nzchar(entries)]
  }, error = function(e) {
    cat("Error listing files in", ftp_path, ":", e$message, "\n")
    character(0)
  })
}

# Function to download CSV file (no filtering - app will handle filtering)
download_csv <- function(remote_file, local_file) {
  # Check if this file should be excluded
  filename <- basename(remote_file)
  if (should_exclude_csv(filename)) {
    return(FALSE)
  }
  # Skip if file already exists (incremental sync)
  if (file.exists(local_file)) {
    cat("Skipping existing file:", basename(local_file), "\n")
    return(FALSE)  # Return FALSE so we don't count it as newly downloaded
  }
  
  url <- paste0("ftp://", FTP_HOST, remote_file)
  
  tryCatch({
    # Download file to temporary location using curl with proper credentials
    temp_file <- tempfile(fileext = ".csv")
    handle <- curl::new_handle(userpwd = FTP_USERPWD)
    curl::handle_setopt(handle, ftp_use_epsv = FALSE)
    bin <- curl::curl_fetch_memory(url, handle = handle)$content
    writeBin(bin, temp_file)
    
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
  sync_dates <- get_sync_dates()
  downloaded_count <- 0

  for (current_date in sync_dates) {
    yr <- format(current_date, "%Y")
    month_dir <- format(current_date, "%m")
    day_dir <- format(current_date, "%d")
    day_path <- paste0("/practice/", yr, "/", month_dir, "/", day_dir, "/")

    files_in_day <- list_ftp_files(day_path)
    if (length(files_in_day) == 0) next

    cat("Processing practice date:", format(current_date, "%Y/%m/%d"), "\n")
    csv_files <- files_in_day[grepl("\\.csv$", files_in_day, ignore.case = TRUE)]

    # Filter out files with "playerpositioning" in the name (allow unverified)
    csv_files <- csv_files[!grepl("playerpositioning", csv_files, ignore.case = TRUE)]

    for (file in csv_files) {
      remote_path <- paste0(day_path, file)
      local_path <- file.path(LOCAL_PRACTICE_DIR, paste0("practice_", yr, "_", month_dir, "_", day_dir, "_", file))

      if (download_csv(remote_path, local_path)) {
        downloaded_count <- downloaded_count + 1
      }

      Sys.sleep(0.1)
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
  start_date <- as.Date("2025-08-10")
  
  # Include all data from August 1, 2025 onwards (no future year restrictions)
  return(file_date >= start_date)
}

# Function to sync v3 data with date filtering
sync_v3_data <- function() {
  cat("Syncing v3 data with date filtering...\n")
  sync_dates <- get_sync_dates()
  downloaded_count <- 0
  seen_v3_files <- character(0)

  for (current_date in sync_dates) {
    yr <- format(current_date, "%Y")
    month_dir <- format(current_date, "%m")
    day_dir <- format(current_date, "%d")
    day_path <- paste0("/v3/", yr, "/", month_dir, "/", day_dir, "/")
    full_date_path <- format(current_date, "%Y/%m/%d")

    if (!is_date_in_range(full_date_path)) {
      next
    }

    files_in_day <- list_ftp_files(day_path)
    if (length(files_in_day) == 0) next

    cat("Processing date:", full_date_path, "\n")

    if ("CSV" %in% files_in_day) {
      csv_path <- paste0(day_path, "CSV/")
      csv_files <- list_ftp_files(csv_path)
      csv_files <- csv_files[grepl("\\.csv$", csv_files, ignore.case = TRUE)]

      # Filter out files with "playerpositioning" or "unverified" in v3 folder
      csv_files <- csv_files[!grepl("playerpositioning", csv_files, ignore.case = TRUE)]

      for (file in csv_files) {
        if (!nzchar(file)) next
        remote_path <- paste0(csv_path, file)
        # De-dupe by full remote path, not basename, so same filename on new dates is still synced.
        file_key <- tolower(trimws(remote_path))
        if (file_key %in% seen_v3_files) {
          cat("Skipping duplicate v3 CSV suffix:", file, "(already processed)\n")
          next
        }
        seen_v3_files <- c(seen_v3_files, file_key)

        local_path <- file.path(LOCAL_V3_DIR, paste0("v3_", yr, "_", month_dir, "_", day_dir, "_", file))

        if (download_csv(remote_path, local_path)) {
          downloaded_count <- downloaded_count + 1
        }

        Sys.sleep(0.1)
      }
    } else {
      csv_files <- files_in_day[grepl("\\.csv$", files_in_day, ignore.case = TRUE)]

      csv_files <- csv_files[!grepl("playerpositioning|unverified", csv_files, ignore.case = TRUE)]

      for (file in csv_files) {
        if (!nzchar(file)) next
        remote_path <- paste0(day_path, file)
        # De-dupe by full remote path, not basename, so same filename on new dates is still synced.
        file_key <- tolower(trimws(remote_path))
        if (file_key %in% seen_v3_files) {
          cat("Skipping duplicate v3 CSV suffix:", file, "(already processed)\n")
          next
        }
        seen_v3_files <- c(seen_v3_files, file_key)

        local_path <- file.path(LOCAL_V3_DIR, paste0("v3_", yr, "_", month_dir, "_", day_dir, "_", file))

        if (download_csv(remote_path, local_path)) {
          downloaded_count <- downloaded_count + 1
        }

        Sys.sleep(0.1)
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
  csv_files <- list.files(CSV_CACHE_ROOT, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  
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
    old_files <- list.files(CSV_CACHE_ROOT, pattern = "\\.(csv|txt)$", full.names = TRUE, recursive = TRUE)
    if (length(old_files) > 0) {
      file.remove(old_files)
      cat("Cleaned", length(old_files), "old data files\n")
    }
  } else {
    cat("Incremental sync - keeping existing files\n")
  }
  
  # Sync both data sources
  t_practice_start <- Sys.time()
  practice_updated <- sync_practice_data()
  cat("TIMING sync_practice_data_sec:", round(as.numeric(difftime(Sys.time(), t_practice_start, units = "secs")), 2), "\n")

  t_v3_start <- Sys.time()
  v3_updated <- sync_v3_data()
  cat("TIMING sync_v3_data_sec:", round(as.numeric(difftime(Sys.time(), t_v3_start, units = "secs")), 2), "\n")
  
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "mins")
  
  cat("Data sync completed in", round(duration, 2), "minutes\n")
  
  # Deduplicate downloaded files if any data was updated
  if (practice_updated || v3_updated) {
    t_dedupe_start <- Sys.time()
    deduplicate_files()
    cat("TIMING deduplicate_files_sec:", round(as.numeric(difftime(Sys.time(), t_dedupe_start, units = "secs")), 2), "\n")
  }
  
  # Update last sync timestamp and modification notification
  writeLines(as.character(Sys.time()), file.path(LOCAL_DATA_DIR, "last_sync.txt"))
  
  # Create a flag file to indicate new data is available
  if (practice_updated || v3_updated) {
    writeLines(as.character(Sys.time()), file.path(LOCAL_DATA_DIR, "new_data_flag.txt"))
  }

  video_updated <- tryCatch(
    {
      t_video_start <- Sys.time()
      out <- sync_video_map_from_neon(file.path(LOCAL_DATA_DIR, "video_map.csv"))
      cat("TIMING sync_video_map_from_neon_sec:", round(as.numeric(difftime(Sys.time(), t_video_start, units = "secs")), 2), "\n")
      out
    },
    error = function(e) {
      cat("Skipping Neon video map sync:", e$message, "\n")
      FALSE
    }
  )
  if (video_updated) {
    cat("Regenerated data/video_map.csv from Neon video metadata\n")
  }

  push_to_neon <- tolower(trimws(Sys.getenv("SYNC_PITCH_DATA_TO_NEON", "true"))) %in% c("1", "true", "yes")
  if (push_to_neon && (practice_updated || v3_updated)) {
    neon_sync_script <- file.path("scripts", "sync_pitch_data_to_neon.R")
    if (file.exists(neon_sync_script)) {
      cat("Syncing pitch CSV history to Neon table...\n")
      t_neon_start <- Sys.time()
      neon_status <- tryCatch(
        system2(
          "Rscript",
          neon_sync_script,
          stdout = TRUE,
          stderr = TRUE,
          env = c(sprintf("PITCH_DATA_SYNC_DIR=%s", CSV_CACHE_ROOT))
        ),
        error = function(e) paste("Neon sync failed:", e$message)
      )
      cat(paste(neon_status, collapse = "\n"), "\n")
      cat("TIMING sync_pitch_data_to_neon_sec:", round(as.numeric(difftime(Sys.time(), t_neon_start, units = "secs")), 2), "\n")
    } else {
      cat("Skipping Neon pitch sync; missing script:", neon_sync_script, "\n")
    }
  }
  
  # Return TRUE if any data was updated
  return(practice_updated || v3_updated || video_updated)
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
