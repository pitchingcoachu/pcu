# automated_data_sync.R
# Baseball Data Automation Script (generic)
# Syncs data from TrackMan FTP, starting at 2024-01-01

library(RCurl)
library(curl)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# ------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------

FTP_HOST <- "ftp.trackmanbaseball.com"
FTP_USER <- "Jared Gaynor"
FTP_PASS <- "Wev4SdE2a8"

START_DATE <- as.Date("2024-01-01")

LOCAL_DATA_DIR     <- "data/"
LOCAL_PRACTICE_DIR <- file.path(LOCAL_DATA_DIR, "practice")
LOCAL_V3_DIR       <- file.path(LOCAL_DATA_DIR, "v3")

dir.create(LOCAL_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOCAL_PRACTICE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOCAL_V3_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------

ftp_credentials <- function() paste(FTP_USER, FTP_PASS, sep = ":")
ftp_url <- function(path = "") paste0("ftp://", FTP_HOST, path)

list_ftp_files <- function(ftp_path) {
  url <- ftp_url(ftp_path)
  max_attempts <- 3

  for (attempt in seq_len(max_attempts)) {
    handle <- curl::new_handle()
    curl::handle_setopt(
      handle,
      userpwd = ftp_credentials(),
      ftp_use_epsv = FALSE,
      dirlistonly = TRUE
    )

    result <- tryCatch({
      resp <- curl::curl_fetch_memory(url, handle = handle)
      files <- trimws(strsplit(rawToChar(resp$content), "\n", fixed = TRUE)[[1]])
      files[nzchar(files)]
    }, error = function(e) e)

    if (!inherits(result, "error")) {
      return(result)
    }

    if (attempt == max_attempts) {
      cat("Error listing files in", ftp_path, ":", result$message, "\n")
      return(character(0))
    }

    Sys.sleep(0.25 * attempt)
  }

  character(0)
}

within_range <- function(year, month, day) {
  file_date <- suppressWarnings(as.Date(sprintf("%s-%s-%s", year, month, day)))
  !is.na(file_date) && file_date >= START_DATE
}

download_csv <- function(remote_file, local_file) {
  if (file.exists(local_file)) {
    cat("Skipping existing file:", basename(local_file), "\n")
    return(FALSE)
  }

  tryCatch({
    tmp <- tempfile(fileext = ".csv")
    handle <- curl::new_handle()
    curl::handle_setopt(handle, userpwd = ftp_credentials(), ftp_use_epsv = FALSE)
    curl::curl_download(ftp_url(remote_file), tmp, handle = handle, quiet = TRUE)

    data <- read_csv(tmp, show_col_types = FALSE)
    if (nrow(data) > 0) {
      write_csv(data, local_file)
      cat("Downloaded", nrow(data), "rows to", local_file, "\n")
      unlink(tmp)
      TRUE
    } else {
      cat("No data found in", remote_file, "\n")
      unlink(tmp)
      FALSE
    }
  }, error = function(e) {
    cat("Error processing", remote_file, ":", e$message, "\n")
    FALSE
  })
}

list_year_dirs <- function(root_path) {
  years <- list_ftp_files(root_path)
  years <- years[grepl("^\\d{4}$", years)]
  years[as.integer(years) >= as.integer(format(START_DATE, "%Y"))]
}

# ------------------------------------------------------------------
# Sync practice data
# ------------------------------------------------------------------

sync_practice_data <- function() {
  cat("Syncing practice data (from", START_DATE, ")...\n")
  years <- sort(list_year_dirs("/practice/"))
  downloaded_count <- 0

  for (year_dir in years) {
    year_path <- paste0("/practice/", year_dir, "/")
    months <- list_ftp_files(year_path)
    month_dirs <- months[grepl("^\\d{2}$", months)]

    for (month_dir in month_dirs) {
      month_path <- paste0(year_path, month_dir, "/")
      days <- list_ftp_files(month_path)
      day_dirs <- days[grepl("^\\d{2}$", days)]

      for (day_dir in day_dirs) {
        if (!within_range(year_dir, month_dir, day_dir)) next

        day_path <- paste0(month_path, day_dir, "/")
        cat("Practice date:", year_dir, "/", month_dir, "/", day_dir, "\n")

        files_in_day <- list_ftp_files(day_path)
        csv_files <- files_in_day[grepl("\\.csv$", files_in_day, ignore.case = TRUE)]
        csv_files <- csv_files[!grepl("playerpositioning", csv_files, ignore.case = TRUE)]

        for (file in csv_files) {
          remote_path <- paste0(day_path, file)
          local_path <- file.path(
            LOCAL_PRACTICE_DIR,
            paste0("practice_", year_dir, "_", month_dir, "_", day_dir, "_", file)
          )
          if (download_csv(remote_path, local_path)) downloaded_count <- downloaded_count + 1
          Sys.sleep(0.1)
        }
      }
    }
  }

  cat("Practice sync complete:", downloaded_count, "files downloaded\n")
  downloaded_count > 0
}

# ------------------------------------------------------------------
# Sync V3 (live) data
# ------------------------------------------------------------------

sync_v3_data <- function() {
  cat("Syncing V3 data (from", START_DATE, ")...\n")
  years <- sort(list_year_dirs("/v3/"))
  downloaded_count <- 0

  for (year_dir in years) {
    year_path <- paste0("/v3/", year_dir, "/")
    months <- list_ftp_files(year_path)
    month_dirs <- months[grepl("^\\d{2}$", months)]

    for (month_dir in month_dirs) {
      month_path <- paste0(year_path, month_dir, "/")
      days <- list_ftp_files(month_path)
      day_dirs <- days[grepl("^\\d{2}$", days)]

      for (day_dir in day_dirs) {
        if (!within_range(year_dir, month_dir, day_dir)) next

        day_path <- paste0(month_path, day_dir, "/")
        cat("V3 date:", year_dir, "/", month_dir, "/", day_dir, "\n")

        files_in_day <- list_ftp_files(day_path)
        csv_dir <- files_in_day[tolower(files_in_day) == "csv"]

        process_csv_files <- function(csv_files, base_path) {
          csv_files <- csv_files[grepl("\\.csv$", csv_files, ignore.case = TRUE)]
          csv_files <- csv_files[!grepl("playerpositioning|unverified", csv_files, ignore.case = TRUE)]
          for (file in csv_files) {
            remote_path <- paste0(base_path, file)
            local_path <- file.path(
              LOCAL_V3_DIR,
              paste0("v3_", year_dir, "_", month_dir, "_", day_dir, "_", file)
            )
            if (download_csv(remote_path, local_path)) downloaded_count <<- downloaded_count + 1
            Sys.sleep(0.1)
          }
        }

        if (length(csv_dir)) {
          csv_path <- paste0(day_path, csv_dir[[1]], "/")
          process_csv_files(list_ftp_files(csv_path), csv_path)
        } else {
          process_csv_files(files_in_day, day_path)
        }
      }
    }
  }

  cat("V3 sync complete:", downloaded_count, "files downloaded\n")
  downloaded_count > 0
}

# ------------------------------------------------------------------
# Deduplicate local files
# ------------------------------------------------------------------

deduplicate_files <- function() {
  cat("Starting deduplication process...\n")
  csv_files <- list.files(LOCAL_DATA_DIR, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

  if (!length(csv_files)) {
    cat("No CSV files found for deduplication\n")
    return(FALSE)
  }

  all_data <- list()

  for (file in csv_files) {
    tryCatch({
      data <- read_csv(file, show_col_types = FALSE, col_types = cols(.default = "c"))
      if (nrow(data) > 0) {
        data$SourceFile <- basename(file)
        all_data[[length(all_data) + 1]] <- data
      }
    }, error = function(e) {
      cat("Error reading", file, ":", e$message, "\n")
    })
  }

  if (!length(all_data)) {
    cat("No valid data found in CSV files\n")
    return(FALSE)
  }

  combined <- bind_rows(all_data)

  key_cols <- c(
    "Date", "Pitcher", "Batter", "PitchNo", "PlateLocSide", "PlateLocHeight",
    "RelSpeed", "TaggedPitchType", "Balls", "Strikes"
  )
  available_keys <- intersect(key_cols, names(combined))
  if (!length(available_keys)) {
    cat("Warning: No key columns found for deduplication. Using all columns.\n")
    available_keys <- setdiff(names(combined), "SourceFile")
  }

  original_count <- nrow(combined)
  deduped <- combined %>% distinct(across(all_of(available_keys)), .keep_all = TRUE)
  removed <- original_count - nrow(deduped)

  if (removed > 0) {
    cat("Removed", removed, "duplicate rows\n")
    for (source_file in unique(deduped$SourceFile)) {
      out <- deduped %>%
        filter(SourceFile == source_file) %>%
        select(-SourceFile)
      if (nrow(out) > 0) {
        target <- csv_files[basename(csv_files) == source_file]
        if (length(target) == 1) {
          write_csv(out, target)
          cat("Rewrote", target, "with", nrow(out), "unique rows\n")
        }
      }
    }
  } else {
    cat("No duplicates found\n")
  }

  removed > 0
}

# ------------------------------------------------------------------
# Main sync
# ------------------------------------------------------------------

main_sync <- function() {
  cat("Starting data sync at", as.character(Sys.time()), "\n")
  start_time <- Sys.time()

  last_sync_file <- file.path(LOCAL_DATA_DIR, "last_sync.txt")
  if (!file.exists(last_sync_file)) {
    cat("First run detected - cleaning old data files\n")
    old_files <- list.files(LOCAL_DATA_DIR, pattern = "\\.(csv|txt)$", full.names = TRUE, recursive = TRUE)
    if (length(old_files)) {
      file.remove(old_files)
      cat("Cleaned", length(old_files), "old data files\n")
    }
  } else {
    cat("Incremental sync - keeping existing files\n")
  }

  practice_updated <- sync_practice_data()
  v3_updated <- sync_v3_data()

  duration <- difftime(Sys.time(), start_time, units = "mins")
  cat("Data sync completed in", round(duration, 2), "minutes\n")

  if (practice_updated || v3_updated) deduplicate_files()

  writeLines(as.character(Sys.time()), last_sync_file)

  practice_updated || v3_updated
}

# ------------------------------------------------------------------
# Execute if run directly
# ------------------------------------------------------------------

if (!interactive()) {
  updated <- main_sync()
  if (!updated) {
    cat("No new data found during sync - this is normal for incremental sync\n")
  } else {
    cat("New data was downloaded and processed\n")
  }
  cat("Sync completed successfully\n")
}
