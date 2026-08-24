# One-time backfill: re-downloads PCU's practice CSVs for May 1, 2026 through
# today from the TrackMan FTP and UPDATEs the x0/y0/z0/vx0/vy0/vz0/ax0/ay0/az0
# columns (added in pitch_data_service.R) on already-synced pitch_events rows,
# matched by pitch_key.
#
# Deliberately does NOT reuse sync_csv_file_to_neon's normal
# delete-by-file_id + insert-skip-if-pitch-key-exists path: that path is
# designed to add NEW pitches, and treats an existing pitch_key as "already
# have it, nothing to do" -- which is exactly wrong for a backfill whose
# whole point is to update columns on rows that already exist. A first
# attempt at this backfill hit exactly that failure mode (used a different
# local download directory than the production sync, which produced a new
# file_id per file and caused nearly every row's pitch_key to be silently
# treated as a pre-existing duplicate and skipped, updating only 905 of
# ~15,300 eligible rows). This version issues direct UPDATE statements keyed
# on pitch_key, which works regardless of file_id.
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
SCHOOL_CODE <- "PCU"

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
    csv_files <- csv_files[grepl("pitching", csv_files, ignore.case = TRUE)]

    for (file in csv_files) {
      remote_path <- paste0(day_path, file)
      local_path <- file.path(LOCAL_DIR, paste0("practice_", yr, "_", mo, "_", day_dir, "_", file))
      if (download_csv(remote_path, local_path)) {
        downloaded_paths <- c(downloaded_paths, local_path)
      } else if (file.exists(local_path)) {
        # Already downloaded this run (shouldn't normally happen since
        # LOCAL_DIR starts empty, but keeps behavior correct if it does).
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

con <- pitch_data_db_connect()
if (is.null(con)) stop("No Postgres backend configured for pitch data")
on.exit(tryCatch(DBI::dbDisconnect(con), error = function(...) NULL), add = TRUE)

ensure_pitch_data_schema(con)

etbl <- DBI::Id(schema = "public", table = Sys.getenv("PITCH_DATA_DB_TABLE", "pitch_events"))
PHYSICS_COLS <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax0", "ay0", "az0")

total_rows_read <- 0L
total_rows_updated <- 0L
total_rows_no_match <- 0L
file_failures <- character(0)

for (p in downloaded_paths) {
  result <- tryCatch({
    df <- suppressMessages(readr::read_csv(
      p,
      col_types = readr::cols(.default = readr::col_character()),
      progress = FALSE,
      show_col_types = FALSE
    ))
    missing_physics <- setdiff(PHYSICS_COLS, names(df))
    for (col in missing_physics) df[[col]] <- NA_character_

    pitch_key <- if ("PitchUID" %in% names(df)) trimws(as.character(df$PitchUID)) else rep("", nrow(df))
    pitch_key[is.na(pitch_key)] <- ""
    needs_key <- !nzchar(pitch_key)
    if (any(needs_key)) {
      pitch_key[needs_key] <- pitch_data_make_key(df[needs_key, , drop = FALSE])
    }

    n_read <- nrow(df)
    n_updated <- 0L

    keyed_rows <- which(nzchar(pitch_key))
    # Batched UPDATE ... FROM (VALUES ...) instead of one round-trip per row
    # -- a per-row loop against a pooled Neon connection for ~15,000 rows
    # would be far too slow (the row-by-row first attempt took ~4 minutes
    # for just 905 rows).
    batch_size <- 200L
    if (length(keyed_rows)) {
      for (start in seq(1L, length(keyed_rows), by = batch_size)) {
        batch_idx <- keyed_rows[start:min(start + batch_size - 1L, length(keyed_rows))]
        value_rows <- vapply(batch_idx, function(i) {
          col_vals <- vapply(PHYSICS_COLS, function(col) {
            val <- df[[col]][i]
            if (is.na(val) || !nzchar(trimws(as.character(val)))) {
              "NULL"
            } else {
              as.character(DBI::dbQuoteLiteral(con, as.character(val)))
            }
          }, character(1))
          key_sql <- as.character(DBI::dbQuoteLiteral(con, pitch_key[i]))
          sprintf("(%s, %s)", key_sql, paste(col_vals, collapse = ", "))
        }, character(1))

        set_clause <- paste(
          sprintf("%s = v.%s", PHYSICS_COLS, PHYSICS_COLS),
          collapse = ", "
        )
        col_list <- paste(c("pitch_key", PHYSICS_COLS), collapse = ", ")
        update_sql <- sprintf(
          "UPDATE %s AS t SET %s FROM (VALUES %s) AS v(%s) WHERE t.school_code = %s AND t.pitch_key = v.pitch_key AND (t.x0 IS NULL OR t.x0 = '')",
          as.character(DBI::dbQuoteIdentifier(con, etbl)),
          set_clause,
          paste(value_rows, collapse = ", "),
          col_list,
          as.character(DBI::dbQuoteLiteral(con, SCHOOL_CODE))
        )
        rows_affected <- pitch_data_db_execute(con, update_sql)
        if (!is.null(rows_affected) && is.finite(suppressWarnings(as.integer(rows_affected)))) {
          n_updated <- n_updated + as.integer(rows_affected)
        }
      }
    }

    list(read = n_read, updated = n_updated, ok = TRUE)
  }, error = function(e) {
    file_failures <<- c(file_failures, paste0(p, ": ", e$message))
    list(read = 0L, updated = 0L, ok = FALSE)
  })

  total_rows_read <- total_rows_read + result$read
  total_rows_updated <- total_rows_updated + result$updated
  cat("Processed", basename(p), "-", result$read, "rows read,", result$updated, "updated\n")
}

cat("Backfill complete. Files processed:", length(downloaded_paths),
    "| Rows read:", total_rows_read,
    "| Rows updated:", total_rows_updated,
    "| File failures:", length(file_failures), "\n")
if (length(file_failures)) {
  cat("File failures:\n")
  for (f in file_failures) cat(" -", f, "\n")
}
