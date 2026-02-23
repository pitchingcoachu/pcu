#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(readr)
  library(dplyr)
  library(stringr)
  library(digest)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

parse_postgres_uri <- function(uri) {
  if (!nzchar(uri)) return(NULL)
  cleaned <- sub("^postgres(?:ql)?://", "", uri, ignore.case = TRUE)
  parts <- strsplit(cleaned, "@", fixed = TRUE)[[1]]
  if (length(parts) != 2) return(NULL)

  creds <- parts[1]
  host_part <- parts[2]
  cred_parts <- strsplit(creds, ":", fixed = TRUE)[[1]]
  if (length(cred_parts) < 2) return(NULL)

  user <- utils::URLdecode(cred_parts[1])
  password <- utils::URLdecode(paste(cred_parts[-1], collapse = ":"))
  host_segments <- strsplit(host_part, "/", fixed = TRUE)[[1]]
  host_port <- host_segments[1]
  db_raw <- if (length(host_segments) > 1) paste(host_segments[-1], collapse = "/") else ""
  if (!nzchar(db_raw)) return(NULL)

  db_parts <- strsplit(db_raw, "?", fixed = TRUE)[[1]]
  dbname <- utils::URLdecode(db_parts[1])
  query_segments <- if (length(db_parts) > 1) db_parts[-1] else character(0)

  host_parts <- strsplit(host_port, ":", fixed = TRUE)[[1]]
  host <- host_parts[1]
  port <- if (length(host_parts) > 1) suppressWarnings(as.integer(host_parts[2])) else 5432
  if (is.na(port) || port <= 0) port <- 5432

  params <- list(sslmode = "require", channel_binding = "require")
  if (length(query_segments)) {
    for (segment in strsplit(paste(query_segments, collapse = "&"), "&", fixed = TRUE)[[1]]) {
      kv <- strsplit(segment, "=", fixed = TRUE)[[1]]
      if (!length(kv) || !nzchar(kv[1])) next
      key <- tolower(trimws(kv[1]))
      value <- if (length(kv) > 1) utils::URLdecode(kv[2]) else ""
      if (key %in% names(params)) params[[key]] <- value
    }
  }

  list(
    host = host,
    port = port,
    user = user,
    password = password,
    dbname = dbname,
    sslmode = params$sslmode,
    channel_binding = params$channel_binding
  )
}

read_simple_yaml <- function(path) {
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  out <- list()
  for (line in lines) {
    line <- trimws(line)
    if (!nzchar(line) || startsWith(line, "#")) next
    colon <- regexpr(":", line, fixed = TRUE)
    if (colon < 0) next
    key <- tolower(trimws(substring(line, 1, colon - 1)))
    value <- trimws(substring(line, colon + 1))
    out[[key]] <- value
  }
  out
}

get_db_config <- function() {
  url <- Sys.getenv("PITCH_DATA_DB_URL", "")
  if (nzchar(url)) {
    parsed <- parse_postgres_uri(url)
    if (!is.null(parsed)) return(parsed)
  }

  host <- Sys.getenv("PITCH_DATA_DB_HOST", "")
  user <- Sys.getenv("PITCH_DATA_DB_USER", "")
  password <- Sys.getenv("PITCH_DATA_DB_PASSWORD", "")
  dbname <- Sys.getenv("PITCH_DATA_DB_NAME", "")
  port <- suppressWarnings(as.integer(Sys.getenv("PITCH_DATA_DB_PORT", "")))
  sslmode <- Sys.getenv("PITCH_DATA_DB_SSLMODE", "require")
  channel_binding <- Sys.getenv("PITCH_DATA_DB_CHANNEL_BINDING", "require")

  if (nzchar(host) && nzchar(user) && nzchar(password) && nzchar(dbname)) {
    if (is.na(port) || port <= 0) port <- 5432
    return(list(
      host = host,
      port = port,
      user = user,
      password = password,
      dbname = dbname,
      sslmode = sslmode,
      channel_binding = channel_binding
    ))
  }

  cfg_path <- Sys.getenv("PITCH_DATA_DB_CONFIG", "auth_db_config.yml")
  cfg <- read_simple_yaml(cfg_path)
  if (is.null(cfg)) return(NULL)
  driver <- tolower(cfg$driver %||% "")
  if (!driver %in% c("postgres", "postgresql", "neon")) return(NULL)

  port_val <- suppressWarnings(as.integer(cfg$port %||% "5432"))
  if (is.na(port_val) || port_val <= 0) port_val <- 5432

  list(
    host = cfg$host %||% "",
    port = port_val,
    user = cfg$user %||% "",
    password = cfg$password %||% "",
    dbname = cfg$dbname %||% "",
    sslmode = cfg$sslmode %||% "require",
    channel_binding = cfg$channel_binding %||% "require"
  )
}

as_date_flex <- function(x) {
  x <- as.character(x)
  out <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  mdy <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
  out[is.na(out)] <- mdy[is.na(out)]
  mdy2 <- suppressWarnings(as.Date(x, format = "%m/%d/%y"))
  out[is.na(out)] <- mdy2[is.na(out)]

  nums <- suppressWarnings(as.numeric(x))
  serial_ok <- !is.na(nums) & nums > 20000 & nums < 70000
  if (any(serial_ok)) {
    out[serial_ok] <- as.Date(nums[serial_ok], origin = "1899-12-30")
  }
  out
}

list_pitch_csvs <- function(data_dir) {
  files <- list.files(data_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  files[grepl("([/\\\\]practice[/\\\\])|([/\\\\]v3[/\\\\])", tolower(files))]
}

read_pitch_csv <- function(path) {
  df <- suppressMessages(readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ))
  if (!nrow(df)) return(NULL)
  df$source_file <- path
  if (!"Date" %in% names(df)) df$Date <- NA_character_
  df$pitch_date <- as_date_flex(df$Date)

  key_cols <- intersect(c(
    "Date", "Pitcher", "PitchNo", "PlayID", "TaggedPitchType",
    "PlateLocSide", "PlateLocHeight", "RelSpeed", "source_file"
  ), names(df))

  if (!length(key_cols)) {
    key_cols <- names(df)
  }

  key_frame <- df[, key_cols, drop = FALSE]
  key_frame[is.na(key_frame)] <- ""
  df$ingest_key <- apply(key_frame, 1, function(row) digest(paste(row, collapse = "|"), algo = "xxhash64"))
  df
}

ensure_table <- function(con, table_ref, cols) {
  tbl_sql <- DBI::dbQuoteIdentifier(con, table_ref)
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE IF NOT EXISTS %s (ingest_key TEXT PRIMARY KEY, pitch_date DATE, source_file TEXT, ingested_at TIMESTAMPTZ NOT NULL DEFAULT NOW())",
    tbl_sql
  ))

  existing <- tolower(DBI::dbListFields(con, table_ref))
  for (col in cols) {
    if (tolower(col) %in% existing) next
    col_sql <- DBI::dbQuoteIdentifier(con, col)
    DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s TEXT", tbl_sql, col_sql))
  }
}

upsert_chunk <- function(con, table_ref, df_chunk) {
  tmp_name <- paste0("tmp_pitch_sync_", as.integer(stats::runif(1, 1, 1e9)))
  on.exit(try(DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", DBI::dbQuoteIdentifier(con, tmp_name))), silent = TRUE), add = TRUE)

  DBI::dbWriteTable(con, tmp_name, df_chunk, temporary = FALSE, overwrite = TRUE, row.names = FALSE)

  cols <- names(df_chunk)
  target_cols <- paste(DBI::dbQuoteIdentifier(con, cols), collapse = ", ")
  updates <- setdiff(cols, "ingest_key")
  update_sql <- paste(sprintf(
    "%s = EXCLUDED.%s",
    DBI::dbQuoteIdentifier(con, updates),
    DBI::dbQuoteIdentifier(con, updates)
  ), collapse = ", ")

  insert_sql <- sprintf(
    "INSERT INTO %s (%s) SELECT %s FROM %s ON CONFLICT (%s) DO UPDATE SET %s",
    DBI::dbQuoteIdentifier(con, table_ref),
    target_cols,
    target_cols,
    DBI::dbQuoteIdentifier(con, tmp_name),
    DBI::dbQuoteIdentifier(con, "ingest_key"),
    update_sql
  )
  DBI::dbExecute(con, insert_sql)
}

ensure_indexes <- function(con, table_ref) {
  tbl <- DBI::dbQuoteIdentifier(con, table_ref)
  idx <- function(name, expr) DBI::dbExecute(con, sprintf("CREATE INDEX IF NOT EXISTS %s ON %s %s", DBI::dbQuoteIdentifier(con, name), tbl, expr))

  idx("idx_pitch_events_pitch_date", "(pitch_date)")

  fields <- DBI::dbListFields(con, table_ref)
  field_l <- tolower(fields)
  maybe_idx <- function(col, idx_name) {
    if (!tolower(col) %in% field_l) return(invisible(NULL))
    idx(idx_name, sprintf("(%s)", DBI::dbQuoteIdentifier(con, fields[match(tolower(col), field_l)])))
  }

  maybe_idx("Pitcher", "idx_pitch_events_pitcher")
  maybe_idx("Batter", "idx_pitch_events_batter")
  maybe_idx("TaggedPitchType", "idx_pitch_events_pitch_type")
  maybe_idx("PlayID", "idx_pitch_events_play_id")
  maybe_idx("SessionType", "idx_pitch_events_session_type")
}

main <- function() {
  data_dir <- Sys.getenv("PITCH_DATA_SYNC_DIR", "data")
  table_name <- Sys.getenv("PITCH_DATA_DB_TABLE", "pitch_events")
  schema_name <- Sys.getenv("PITCH_DATA_DB_SCHEMA", "")
  batch_size <- suppressWarnings(as.integer(Sys.getenv("PITCH_DATA_SYNC_BATCH", "20000")))
  if (!is.finite(batch_size) || batch_size <= 0) batch_size <- 20000

  cfg <- get_db_config()
  if (is.null(cfg)) {
    stop("No Neon/Postgres config found. Set PITCH_DATA_DB_URL or PITCH_DATA_DB_HOST/...", call. = FALSE)
  }

  files <- list_pitch_csvs(data_dir)
  if (!length(files)) stop(sprintf("No practice/v3 CSVs found under %s", data_dir), call. = FALSE)

  message(sprintf("Found %d CSV files under %s", length(files), data_dir))
  frames <- lapply(files, function(path) {
    tryCatch(read_pitch_csv(path), error = function(e) {
      message(sprintf("Skipping %s: %s", path, e$message))
      NULL
    })
  })
  frames <- Filter(Negate(is.null), frames)
  if (!length(frames)) stop("No usable CSV data found.", call. = FALSE)

  df <- bind_rows(frames)
  df <- distinct(df, ingest_key, .keep_all = TRUE)

  table_ref <- if (nzchar(schema_name)) DBI::Id(schema = schema_name, table = table_name) else DBI::Id(table = table_name)

  params <- list(
    host = cfg$host,
    port = cfg$port,
    user = cfg$user,
    password = cfg$password,
    dbname = cfg$dbname,
    sslmode = cfg$sslmode
  )
  if (nzchar(cfg$channel_binding %||% "")) params$channel_binding <- cfg$channel_binding

  con <- do.call(DBI::dbConnect, c(list(RPostgres::Postgres()), params))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ensure_table(con, table_ref, setdiff(names(df), c("ingest_key", "pitch_date", "source_file", "ingested_at")))

  total <- nrow(df)
  starts <- seq(1, total, by = batch_size)
  for (i in seq_along(starts)) {
    start <- starts[i]
    end <- min(total, start + batch_size - 1)
    chunk <- df[start:end, , drop = FALSE]
    upsert_chunk(con, table_ref, chunk)
    message(sprintf("Upserted rows %d-%d of %d", start, end, total))
  }

  ensure_indexes(con, table_ref)
  message(sprintf("Neon sync complete. Upserted %d unique rows into %s", total, table_name))
}

main()
