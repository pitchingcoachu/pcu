# One-time backfill: re-downloads PCU's practice CSVs for May 1, 2026 through
# today from the TrackMan FTP and force-resyncs them into pitch_events, so
# the x0/y0/z0/vx0/vy0/vz0/ax0/ay0/az0 columns added in
# pitch_data_service.R get populated for pitches that were already synced
# before that fix landed (a normal incremental run would skip these files as
# unchanged, since the checksum/mtime fast-skip only looks at the file
# itself, not which DB columns have been populated from it).
#
# Scoped to May 1-today only (not the full FTP history) to avoid a slow,
# unnecessary full-year reprocess -- everything before May already shows up
# correctly via the pre-existing pitch_flight_backfill/pitch_data path.
#
# Run via a one-off GitHub Actions workflow_dispatch in this repo (has R,
# FTP network access, and the Neon DB connection already configured).

library(curl)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

source("csv_filter_utils.R")
source("video_map_helpers.R")
source("pitch_data_service.R")

FTP_HOST <- "ftp.trackmanbaseball.com"
FTP_USER <- "Jared Gaynor"
FTP_PASS <- "uVeo9vDRhX"
FTP_USERPWD <- paste0(FTP_USER, ":", FTP_PASS)

BACKFILL_START <- as.Date("2026-05-01")
BACKFILL_END <- Sys.Date()

LOCAL_DIR <- "backfill_data/practice"
dir.create(LOCAL_DIR, recursive = TRUE, showWarnings = FALSE)

list_ftp_files <- function(ftp_path) {
  url <- paste0("ftp://", FTP_HOST, ftp_path)
  tryCatch({
    handle <- curl::new_handle(userpwd = FTP_USERPWD)
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

download_csv <- function(remote_file, local_file) {
  filename <- basename(remote_file)
  if (grepl("fhc|json", filename, ignore.case = TRUE)) return(FALSE)
  url <- paste0("ftp://", FTP_HOST, remote_file)
  tryCatch({
    temp_file <- tempfile(fileext = ".csv")
    handle <- curl::new_handle(userpwd = FTP_USERPWD)
    curl::handle_setopt(handle, ftp_use_epsv = FALSE)
    curl::curl_download(url, destfile = temp_file, mode = "wb", handle = handle)
    file_size <- suppressWarnings(file.info(temp_file)$size)
    if (!is.finite(file_size) || file_size <= 0) {
      unlink(temp_file)
      return(FALSE)
    }
    dir.create(dirname(local_file), recursive = TRUE, showWarnings = FALSE)
    if (!file.rename(temp_file, local_file)) {
      ok <- file.copy(temp_file, local_file, overwrite = TRUE)
      unlink(temp_file)
      if (!isTRUE(ok)) stop("Failed to move downloaded file into place")
    }
    cat("Downloaded", basename(local_file), "-", format(file_size, big.mark = ","), "bytes\n")
    TRUE
  }, error = function(e) {
    cat("Error downloading", remote_file, ":", e$message, "\n")
    FALSE
  })
}

months_in_range <- function(start_date, end_date) {
  months <- seq(floor_date(start_date, "month"), floor_date(end_date, "month"), by = "month")
  data.frame(year = year(months), month = sprintf("%02d", month(months)))
}

downloaded_paths <- character(0)
month_range <- months_in_range(BACKFILL_START, BACKFILL_END)

for (i in seq_len(nrow(month_range))) {
  yr <- as.character(month_range$year[i])
  mo <- month_range$month[i]
  month_path <- paste0("/practice/", yr, "/", mo, "/")
  cat("Listing days in", yr, "/", mo, "\n")
  days <- list_ftp_files(month_path)
  day_dirs <- days[grepl("^\\d{2}$", days)]

  for (day_dir in day_dirs) {
    day_date <- suppressWarnings(as.Date(paste(yr, mo, day_dir, sep = "-")))
    if (is.na(day_date) || day_date < BACKFILL_START || day_date > BACKFILL_END) next

    day_path <- paste0(month_path, day_dir, "/")
    cat("Processing", yr, "/", mo, "/", day_dir, "\n")
    files_in_day <- list_ftp_files(day_path)
    csv_files <- files_in_day[grepl("\\.csv$", files_in_day, ignore.case = TRUE)]
    # Only pitching-session files carry the trajectory fields this backfill
    # cares about -- skip Hitting/other session CSVs to save time.
    csv_files <- csv_files[grepl("pitching", csv_files, ignore.case = TRUE)]

    for (file in csv_files) {
      remote_path <- paste0(day_path, file)
      local_path <- file.path(LOCAL_DIR, paste0("practice_", yr, "_", mo, "_", day_dir, "_", file))
      if (download_csv(remote_path, local_path)) {
        downloaded_paths <- c(downloaded_paths, local_path)
      }
    }
  }
}

cat("Downloaded", length(downloaded_paths), "pitching CSVs for backfill window\n")

if (!length(downloaded_paths)) {
  cat("Nothing to backfill -- exiting.\n")
  quit(status = 0)
}

Sys.setenv(PITCH_DATA_FORCE_RESYNC = "1")

con <- pitch_data_db_connect()
if (is.null(con)) stop("No Postgres backend configured for pitch data")
on.exit(tryCatch(DBI::dbDisconnect(con), error = function(...) NULL), add = TRUE)

ensure_pitch_data_schema(con)

total_rows <- 0L
failures <- character(0)
for (p in downloaded_paths) {
  n <- tryCatch(
    sync_csv_file_to_neon(con, p, school_code = "PCU"),
    error = function(e) {
      failures <<- c(failures, paste0(p, ": ", e$message))
      0L
    }
  )
  total_rows <- total_rows + as.integer(if (is.null(n)) 0L else n)
}

cat("Backfill complete. Files processed:", length(downloaded_paths), "| Rows synced:", total_rows, "| Failures:", length(failures), "\n")
if (length(failures)) {
  cat("Failures:\n")
  for (f in failures) cat(" -", f, "\n")
}
