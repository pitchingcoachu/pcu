library(readr)
library(dplyr)
library(lubridate)
library(glue)
library(rlang)
library(tibble)
library(DBI)

SUPPORTED_SESSION_COLS <- c(
  "session_id", "SessionID", "session", "Session", "GameUID", "GameID"
)
SELECTED_COLS <- c(
  SUPPORTED_SESSION_COLS, "PlayID", "PitchNo", "UTCDateTime", "Date", "UTCTime"
)

extract_public_id <- function(url) {
  if (!nzchar(url)) return(NA_character_)
  cleaned <- sub("\\?.*$", "", url)
  parts <- strsplit(cleaned, "/upload/", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(NA_character_)
  pid <- parts[[2]]
  sub("\\.[^.]*$", "", pid)
}

parse_session_timestamp <- function(x) {
  if (is.null(x)) return(rep(NA_real_, length(x)))
  x <- trimws(as.character(x))
  parsed <- suppressWarnings(lubridate::ymd_hms(x, tz = "UTC"))
  needs <- is.na(parsed) & nzchar(x)
  if (any(needs)) {
    parsed[needs] <- suppressWarnings(lubridate::mdy_hms(x[needs], tz = "UTC"))
  }
  parsed
}

list_session_csvs <- function(data_dir) {
  csvs <- list.files(
    path = data_dir,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  csvs[grepl("(?i)(/practice/|/v3/)", csvs)]
}

read_session_rows <- function(path, session_lookup) {
  df <- tryCatch({
    readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    )
  }, error = function(err) {
    message(glue("Skipping {path}: {err$message}"))
    return(tibble::tibble())
  })

  if (!nrow(df)) return(df)

  session_cols <- intersect(SUPPORTED_SESSION_COLS, names(df))
  if (!length(session_cols)) return(tibble::tibble())

  for (col in session_cols) {
    values <- df[[col]]
    norm <- tolower(trimws(as.character(values)))
    match_rows <- which(norm == session_lookup & nzchar(norm))
    if (length(match_rows)) {
      relevant <- df[match_rows, , drop = FALSE]
      relevant <- relevant %>%
        dplyr::mutate(
          session_id = relevant[[col]]  # keep original casing
        )
      required <- intersect(names(relevant), SELECTED_COLS)
      relevant <- relevant[, unique(c(required, "session_id")), drop = FALSE]
      return(relevant)
    }
  }

  tibble::tibble()
}

find_session_rows <- function(data_dir, session_id) {
  target <- tolower(trimws(session_id))
  csvs <- list_session_csvs(data_dir)
  if (!length(csvs)) {
    stop(glue("No CSV files found under {data_dir} matching /practice/ or /v3/"), call. = FALSE)
  }

  for (csv in csvs) {
    rows <- read_session_rows(csv, target)
    if (nrow(rows)) {
      return(rows)
    }
  }

  tibble::tibble()
}

order_session_rows <- function(df) {
  if (!nrow(df)) return(df)
  df %>%
    dplyr::mutate(
      PitchNo = suppressWarnings(as.integer(PitchNo)),
      timestamp = parse_session_timestamp(UTCDateTime),
      timestamp = dplyr::coalesce(timestamp, parse_session_timestamp(Date))
    ) %>%
    dplyr::arrange(
      is.na(PitchNo), PitchNo,
      is.na(timestamp), timestamp,
      PlayID
    )
}

VIDEO_MAP_TABLE_COLUMNS <- c(
  "session_id", "play_id", "camera_slot", "camera_name", "camera_target",
  "video_type", "azure_blob", "azure_md5", "cloudinary_url", "cloudinary_public_id",
  "uploaded_at", "school_code"
)

video_map_school_code <- function() {
  code <- toupper(trimws(Sys.getenv("TEAM_CODE", "OSU")))
  if (!nzchar(code)) code <- "OSU"
  code
}

parse_video_map_postgres_uri <- function(uri) {
  if (!nzchar(uri)) return(NULL)
  cleaned <- sub("^postgres(?:ql)?://", "", uri, ignore.case = TRUE)
  parts <- strsplit(cleaned, "@", fixed = TRUE)[[1]]
  if (length(parts) != 2) return(NULL)
  creds <- parts[[1]]
  host_part <- parts[[2]]
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
  query_segments <- if (length(db_parts) > 1) strsplit(db_parts[-1], "&", fixed = TRUE)[[1]] else character(0)
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
      if (key %in% names(params)) {
        params[[key]] <- value
      }
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

read_video_map_db_config <- function(path) {
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  entries <- list()
  for (line in lines) {
    line <- trimws(line)
    if (!nzchar(line) || startsWith(line, "#")) next
    colon <- regexpr(":", line, fixed = TRUE)
    if (colon < 0) next
    key <- tolower(trimws(substring(line, 1, colon - 1)))
    value <- trimws(substring(line, colon + 1))
    entries[[key]] <- value
  }
  entries
}

video_map_db_config <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    url <- Sys.getenv("VIDEO_MAP_DB_URL", "")
    if (!nzchar(url)) {
      # Default video-map storage to the same Neon instance as pitch data.
      url <- Sys.getenv("PITCH_DATA_DB_URL", "")
    }
    cfg <- NULL
    if (nzchar(url)) {
      cfg <- parse_video_map_postgres_uri(url)
    }
    if (is.null(cfg)) {
      host <- Sys.getenv("VIDEO_MAP_DB_HOST", "")
      user <- Sys.getenv("VIDEO_MAP_DB_USER", "")
      password <- Sys.getenv("VIDEO_MAP_DB_PASSWORD", "")
      dbname <- Sys.getenv("VIDEO_MAP_DB_NAME", "")
      port <- suppressWarnings(as.integer(Sys.getenv("VIDEO_MAP_DB_PORT", "")))
      sslmode <- Sys.getenv("VIDEO_MAP_DB_SSLMODE", "require")
      channel_binding <- Sys.getenv("VIDEO_MAP_DB_CHANNEL_BINDING", "require")
      if (nzchar(host) && nzchar(user) && nzchar(password) && nzchar(dbname)) {
        if (is.na(port) || port <= 0) port <- 5432
        cfg <- list(
          host = host,
          port = port,
          user = user,
          password = password,
          dbname = dbname,
          sslmode = sslmode,
          channel_binding = channel_binding
        )
      }
    }
    if (is.null(cfg)) {
      config_path <- Sys.getenv("VIDEO_MAP_DB_CONFIG", "auth_db_config.yml")
      env_cfg <- read_video_map_db_config(config_path)
      if (!is.null(env_cfg)) {
        driver <- tolower(env_cfg$driver %||% "")
        if (driver %in% c("postgres", "postgresql", "neon")) {
          port_val <- suppressWarnings(as.integer(env_cfg$port %||% "5432"))
          if (is.na(port_val) || port_val <= 0) port_val <- 5432
          cfg <- list(
            host = env_cfg$host %||% "",
            port = port_val,
            user = env_cfg$user %||% "",
            password = env_cfg$password %||% "",
            dbname = env_cfg$dbname %||% "",
            sslmode = env_cfg$sslmode %||% "require",
            channel_binding = env_cfg$channel_binding %||% "require"
          )
        }
      }
    }
    if (is.null(cfg)) {
      cache <<- NULL
      return(NULL)
    }
    cache <<- cfg
    cache
  }
})

video_map_db_connect <- function() {
  cfg <- video_map_db_config()
  if (is.null(cfg)) return(NULL)
  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    message("RPostgres is required for Neon video map storage.")
    return(NULL)
  }
  params <- list(
    host = cfg$host,
    port = cfg$port %||% 5432,
    user = cfg$user,
    password = cfg$password,
    dbname = cfg$dbname,
    sslmode = cfg$sslmode %||% "require"
  )
  if (nzchar(cfg$channel_binding %||% "")) {
    params$channel_binding <- cfg$channel_binding
  }
  tryCatch({
    do.call(DBI::dbConnect, c(list(RPostgres::Postgres()), params))
  }, error = function(e) {
    message("Unable to connect to video map database: ", e$message)
    NULL
  })
}

video_map_db_execute <- function(con, sql) {
  tryCatch(
    DBI::dbExecute(con, sql),
    error = function(e1) {
      msg <- conditionMessage(e1)
      recoverable <- grepl(
        "unnamed prepared statement does not exist|query needs to be bound before fetching",
        msg,
        ignore.case = TRUE
      )
      if (!recoverable) stop(e1)
      res <- DBI::dbSendStatement(con, sql)
      on.exit(tryCatch(DBI::dbClearResult(res), error = function(...) NULL), add = TRUE)
      DBI::dbGetRowsAffected(res)
    }
  )
}

video_map_db_get_query <- function(con, sql) {
  tryCatch(
    DBI::dbGetQuery(con, sql),
    error = function(e1) {
      msg <- conditionMessage(e1)
      recoverable <- grepl(
        "unnamed prepared statement does not exist|query needs to be bound before fetching",
        msg,
        ignore.case = TRUE
      )
      if (!recoverable) stop(e1)
      res <- DBI::dbSendQuery(con, sql)
      on.exit(tryCatch(DBI::dbClearResult(res), error = function(...) NULL), add = TRUE)
      DBI::dbFetch(res)
    }
  )
}

video_map_table_name <- function() {
  tbl <- Sys.getenv("VIDEO_MAP_DB_TABLE", "video_map")
  tbl <- gsub("[^A-Za-z0-9_]", "_", tbl, perl = TRUE)
  if (!nzchar(tbl)) tbl <- "video_map"
  tbl
}

video_map_ensure_table <- function(con, table) {
  if (is.null(con)) return(FALSE)
  tbl_id <- DBI::dbQuoteIdentifier(con, table)
  video_map_db_execute(con, glue("
    CREATE TABLE IF NOT EXISTS {tbl_id} (
      session_id TEXT NOT NULL,
      play_id TEXT NOT NULL,
      camera_slot TEXT,
      camera_name TEXT,
      camera_target TEXT,
      video_type TEXT,
      azure_blob TEXT,
      azure_md5 TEXT,
      cloudinary_url TEXT,
      cloudinary_public_id TEXT,
      uploaded_at TIMESTAMPTZ,
      school_code TEXT,
      PRIMARY KEY (session_id, camera_slot, play_id)
    )
  "))
  cols <- tryCatch(DBI::dbListFields(con, table), error = function(e) character(0))
  cols <- tolower(as.character(cols))
  if (!"school_code" %in% cols) {
    video_map_db_execute(con, sprintf(
      "ALTER TABLE %s ADD COLUMN school_code TEXT",
      as.character(tbl_id)
    ))
  }
  video_map_db_execute(con, sprintf(
    "CREATE INDEX IF NOT EXISTS %s ON %s (school_code, play_id, camera_slot)",
    as.character(DBI::dbQuoteIdentifier(con, paste0(table, "_school_play_idx"))),
    as.character(tbl_id)
  ))
  TRUE
}

video_map_read_all_neon <- function(play_ids = NULL, school_code = video_map_school_code()) {
  con <- video_map_db_connect()
  if (is.null(con)) return(tibble::tibble())
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tbl <- video_map_table_name()
  if (!DBI::dbExistsTable(con, tbl)) return(tibble::tibble())
  video_map_ensure_table(con, tbl)
  school_code <- toupper(trimws(as.character(school_code %||% "")))
  allow_null_fallback <- tolower(trimws(Sys.getenv("VIDEO_MAP_ALLOW_NULL_FALLBACK", "true"))) %in% c("1", "true", "yes", "y")
  school_pred <- if (nzchar(school_code)) {
    if (allow_null_fallback) {
      sprintf(
        "(school_code = %s OR school_code IS NULL OR btrim(school_code) = '')",
        as.character(DBI::dbQuoteString(con, school_code))
      )
    } else {
      sprintf("school_code = %s", as.character(DBI::dbQuoteString(con, school_code)))
    }
  } else {
    "TRUE"
  }
  if (!is.null(play_ids)) {
    play_ids <- tolower(trimws(as.character(play_ids)))
    play_ids <- unique(play_ids[nzchar(play_ids)])
    if (!length(play_ids)) return(tibble::tibble())
    out <- list()
    chunk_size <- 1000L
    for (i in seq(1L, length(play_ids), by = chunk_size)) {
      chunk <- play_ids[i:min(i + chunk_size - 1L, length(play_ids))]
      in_sql <- paste(vapply(chunk, function(x) as.character(DBI::dbQuoteString(con, x)), character(1)), collapse = ", ")
      sql <- sprintf(
        "SELECT DISTINCT ON (lower(play_id), camera_slot)
            %s
         FROM %s
         WHERE lower(play_id) IN (%s) AND %s
         ORDER BY lower(play_id), camera_slot,
           CASE WHEN upper(coalesce(school_code,'')) = %s THEN 0 ELSE 1 END,
           uploaded_at DESC NULLS LAST",
        paste(VIDEO_MAP_TABLE_COLUMNS, collapse = ", "),
        as.character(DBI::dbQuoteIdentifier(con, tbl)),
        in_sql,
        school_pred,
        as.character(DBI::dbQuoteString(con, school_code))
      )
      out[[length(out) + 1L]] <- tryCatch(video_map_db_get_query(con, sql), error = function(e) tibble::tibble())
    }
    df <- dplyr::bind_rows(out)
  } else {
    sql <- sprintf(
      "SELECT %s FROM %s WHERE %s",
      paste(VIDEO_MAP_TABLE_COLUMNS, collapse = ", "),
      as.character(DBI::dbQuoteIdentifier(con, tbl)),
      school_pred
    )
    df <- tryCatch(video_map_db_get_query(con, sql), error = function(e) tibble::tibble())
  }
  df["uploaded_at"] <- lapply(df["uploaded_at"], as.character)
  df
}

normalize_video_map_df <- function(df) {
  if (!is.data.frame(df)) return(tibble::tibble())
  for (nm in VIDEO_MAP_TABLE_COLUMNS) {
    if (!nm %in% names(df)) df[[nm]] <- NA_character_
  }
  if (!nrow(df)) return(df %>% dplyr::select(all_of(VIDEO_MAP_TABLE_COLUMNS)))
  df <- df %>% dplyr::select(all_of(VIDEO_MAP_TABLE_COLUMNS))
  df[] <- lapply(df, as.character)
  code <- video_map_school_code()
  df$school_code <- ifelse(
    is.na(df$school_code) | !nzchar(trimws(df$school_code)),
    code,
    toupper(trimws(df$school_code))
  )
  df
}

load_manifest <- function(manifest_path) {
  manifest_path <- path.expand(manifest_path)
  if (!file.exists(manifest_path)) {
    stop(glue("Manifest file not found: {manifest_path}"), call. = FALSE)
  }
  manifest <- readr::read_csv(
    manifest_path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
  if (!"cloudinary_url" %in% names(manifest)) {
    stop("Manifest CSV must include a 'cloudinary_url' column.", call. = FALSE)
  }
  manifest <- manifest %>%
    dplyr::mutate(
      cloudinary_url = trimws(cloudinary_url)
    ) %>%
    dplyr::filter(nzchar(cloudinary_url))

  if (!"cloudinary_public_id" %in% names(manifest)) {
    manifest$cloudinary_public_id <- NA_character_
  }

  manifest %>%
    dplyr::mutate(
      cloudinary_public_id = dplyr::coalesce(
        cloudinary_public_id,
        vapply(cloudinary_url, extract_public_id, FUN.VALUE = character(1))
      )
    )
}

load_existing_map <- function(map_path) {
  map_path <- path.expand(map_path)
  if (!file.exists(map_path)) {
    return(tibble::tibble(
      session_id = character(),
      play_id = character(),
      camera_slot = character(),
      camera_name = character(),
      camera_target = character(),
      video_type = character(),
      azure_blob = character(),
      azure_md5 = character(),
      cloudinary_url = character(),
      cloudinary_public_id = character(),
      uploaded_at = character()
    ))
  }
  readr::read_csv(
    map_path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
}

map_manifest_to_session <- function(session_rows,
                                   manifest,
                                   session_id,
                                   slot = "VideoClip2",
                                   name = "ManualCamera",
                                   target = "ManualUpload",
                                   type = "ManualVideo",
                                   map_path = "data/video_map.csv") {
  if (!nrow(session_rows)) {
    stop("No TrackMan rows found for the requested session.", call. = FALSE)
  }
  target_count <- min(nrow(session_rows), nrow(manifest))
  if (target_count == 0) {
    stop("No pitches or clips available to map.", call. = FALSE)
  }
  if (nrow(manifest) > nrow(session_rows)) {
    warning(glue(
      "Manifest has {nrow(manifest)} clips but the session only has {nrow(session_rows)} pitches. ",
      "Only the first {nrow(session_rows)} clips will be assigned."
    ))
  }
  if (nrow(session_rows) > nrow(manifest)) {
    message(glue(
      "Session has {nrow(session_rows)} pitches but manifest provided {nrow(manifest)} clips. ",
      "The first {nrow(manifest)} pitches will be mapped."
    ))
  }
  session_rows <- session_rows %>% dplyr::slice_head(n = target_count)
  manifest <- manifest %>% dplyr::slice_head(n = target_count)

  timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  media_rows <- tibble::tibble(
    session_id = session_rows$session_id,
    play_id = session_rows$PlayID,
    camera_slot = slot,
    camera_name = name,
    camera_target = target,
    video_type = type,
    azure_blob = NA_character_,
    azure_md5 = NA_character_,
    cloudinary_url = manifest$cloudinary_url,
    cloudinary_public_id = manifest$cloudinary_public_id,
    uploaded_at = timestamp
  )

  existing <- load_existing_map(map_path)
  deduped <- existing %>%
    dplyr::filter(!(
      session_id %in% media_rows$session_id &
        camera_slot == slot &
        play_id %in% media_rows$play_id
    ))

  combined <- dplyr::bind_rows(deduped, media_rows)
  readr::write_csv(combined, path.expand(map_path))

  message(glue(
    "Mapped {nrow(media_rows)} clips to session {session_id} and saved to {path.expand(map_path)}"
  ))
  tryCatch(
    video_map_write_rows_to_neon(media_rows),
    error = function(e) message("Neon map update skipped: ", e$message)
  )
  invisible(media_rows)
}

video_map_write_rows_to_neon <- function(rows) {
  rows <- normalize_video_map_df(rows)
  rows <- rows %>% dplyr::select(all_of(VIDEO_MAP_TABLE_COLUMNS))
  rows$uploaded_at <- ifelse(
    nzchar(as.character(rows$uploaded_at)),
    as.character(rows$uploaded_at),
    format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  con <- video_map_db_connect()
  if (is.null(con)) {
    message("Video map Neon configuration not detected; skipping Neon write.")
    return(FALSE)
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tbl <- video_map_table_name()
  video_map_ensure_table(con, tbl)

  tmp_tbl <- paste0("video_map_stage_", as.integer(as.numeric(Sys.time())), "_", sample.int(1e6, 1))
  tmp_id <- DBI::dbQuoteIdentifier(con, DBI::Id(table = tmp_tbl))
  on.exit(
    tryCatch(video_map_db_execute(con, sprintf("DROP TABLE IF EXISTS %s", as.character(tmp_id))), error = function(...) NULL),
    add = TRUE
  )
  DBI::dbWriteTable(con, DBI::Id(table = tmp_tbl), rows, temporary = TRUE, overwrite = TRUE)

  tbl_id <- DBI::dbQuoteIdentifier(con, tbl)
  sql <- glue("
    INSERT INTO {tbl_id}
    (session_id, play_id, camera_slot, camera_name, camera_target, video_type, azure_blob, azure_md5, cloudinary_url, cloudinary_public_id, uploaded_at, school_code)
    SELECT DISTINCT ON (session_id, camera_slot, play_id)
      session_id,
      play_id,
      camera_slot,
      camera_name,
      camera_target,
      video_type,
      azure_blob,
      azure_md5,
      cloudinary_url,
      cloudinary_public_id,
      NULLIF(uploaded_at, '')::timestamptz,
      upper(coalesce(NULLIF(school_code, ''), '{video_map_school_code()}'))
    FROM {tmp_id}
    ORDER BY
      session_id,
      camera_slot,
      play_id,
      NULLIF(uploaded_at, '')::timestamptz DESC NULLS LAST
    ON CONFLICT (session_id, camera_slot, play_id)
    DO UPDATE SET
      camera_name = EXCLUDED.camera_name,
      camera_target = EXCLUDED.camera_target,
      video_type = EXCLUDED.video_type,
      azure_blob = EXCLUDED.azure_blob,
      azure_md5 = EXCLUDED.azure_md5,
      cloudinary_url = EXCLUDED.cloudinary_url,
      cloudinary_public_id = EXCLUDED.cloudinary_public_id,
      uploaded_at = EXCLUDED.uploaded_at,
      school_code = EXCLUDED.school_code
  ")
  video_map_db_execute(con, sql)
  TRUE
}

video_map_sync_from_neon <- function(map_path = "data/video_map.csv") {
  df <- video_map_read_all_neon()
  if (!nrow(df)) return(FALSE)
  ordered <- normalize_video_map_df(df) %>%
    dplyr::arrange(session_id, camera_slot, play_id)
  map_path <- path.expand(map_path)
  existing <- tryCatch(
    readr::read_csv(map_path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE),
    error = function(e) tibble::tibble()
  )
  existing_norm <- normalize_video_map_df(existing)
  allow_shrink <- tolower(trimws(Sys.getenv("FORCE_NEON_VIDEO_MAP_SYNC", unset = "false"))) %in% c("1", "true", "yes")
  existing_n <- nrow(existing_norm)
  neon_n <- nrow(ordered)
  if (!allow_shrink && existing_n > 0 && neon_n < existing_n) {
    message(
      "Skipping Neon video map overwrite because Neon rows (", neon_n,
      ") are fewer than local rows (", existing_n,
      "). Set FORCE_NEON_VIDEO_MAP_SYNC=true to override."
    )
    return(FALSE)
  }
  if (nrow(existing_norm) && identical(existing_norm, ordered)) {
    return(FALSE)
  }
  readr::write_csv(ordered, map_path)
  TRUE
}

sync_video_map_from_neon <- function(...) {
  video_map_sync_from_neon(...)
}

video_map_backfill_local_to_neon <- function(map_path = "data/video_map.csv") {
  map_path <- path.expand(map_path)
  if (!file.exists(map_path)) return(FALSE)

  local_df <- tryCatch(
    readr::read_csv(map_path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE),
    error = function(e) {
      message("Unable to read local video map for Neon backfill: ", e$message)
      tibble::tibble()
    }
  )
  local_df <- normalize_video_map_df(local_df)
  if (!nrow(local_df)) return(FALSE)

  con <- video_map_db_connect()
  if (is.null(con)) return(FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tbl <- video_map_table_name()
  video_map_ensure_table(con, tbl)
  school_code <- video_map_school_code()
  allow_null_fallback <- tolower(trimws(Sys.getenv("VIDEO_MAP_ALLOW_NULL_FALLBACK", "true"))) %in% c("1", "true", "yes", "y")
  school_pred <- if (allow_null_fallback) {
    sprintf(
      "(school_code = %s OR school_code IS NULL OR btrim(school_code) = '')",
      as.character(DBI::dbQuoteString(con, school_code))
    )
  } else {
    sprintf("school_code = %s", as.character(DBI::dbQuoteString(con, school_code)))
  }

  neon_n <- tryCatch(
    as.integer(video_map_db_get_query(con, sprintf(
      "SELECT COUNT(*) AS n FROM %s WHERE %s",
      DBI::dbQuoteIdentifier(con, tbl),
      school_pred
    ))$n[[1]]),
    error = function(e) NA_integer_
  )
  local_n <- nrow(local_df)
  force_backfill <- tolower(trimws(Sys.getenv("FORCE_VIDEO_MAP_NEON_BACKFILL", "false"))) %in% c("1", "true", "yes")

  if (!force_backfill && is.finite(neon_n) && neon_n >= local_n) {
    message("Skipping local->Neon video map backfill: Neon already has ", neon_n, " rows (local=", local_n, ").")
    return(FALSE)
  }

  ok <- tryCatch(video_map_write_rows_to_neon(local_df), error = function(e) {
    message("Local->Neon video map backfill failed: ", e$message)
    FALSE
  })
  if (isTRUE(ok)) {
    message("Backfilled local video map to Neon rows: ", local_n)
  }
  ok
}
