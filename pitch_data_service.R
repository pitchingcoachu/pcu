# High-performance pitch data backend service for Neon/Postgres + CSV fallback.
# Keeps app-compatible columns while supporting chunked, parallel, cached DB loads.

if (!exists("pitch_data_or", mode = "function")) {
  pitch_data_or <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

pitch_data_parse_bool <- function(x, default = FALSE) {
  if (is.null(x) || !nzchar(as.character(x))) return(default)
  tolower(trimws(as.character(x))) %in% c("1", "true", "t", "yes", "y", "on")
}

pitch_data_default_columns <- function() {
  c(
    "Date", "Pitcher", "Email", "PitcherThrows", "TaggedPitchType",
    "InducedVertBreak", "HorzBreak", "RelSpeed", "ReleaseTilt", "BreakTilt",
    "SpinEfficiency", "SpinRate", "RelHeight", "RelSide", "Extension",
    "VertApprAngle", "HorzApprAngle", "PlateLocSide", "PlateLocHeight",
    "PitchCall", "KorBB", "Balls", "Strikes", "SessionType", "PlayID",
    "ExitSpeed", "Angle", "BatterSide", "PlayResult", "TaggedHitType", "OutsOnPlay",
    "Batter", "Catcher", "VideoClip", "VideoClip2", "VideoClip3",
    "PitchUID", "PitchID", "PitchGuid", "SourceFile", "PitchKey"
  )
}

pitch_data_storage_name_map <- function() {
  c(
    Date = "date",
    Pitcher = "pitcher",
    Email = "email",
    PitcherThrows = "pitcherthrows",
    TaggedPitchType = "taggedpitchtype",
    InducedVertBreak = "inducedvertbreak",
    HorzBreak = "horzbreak",
    RelSpeed = "relspeed",
    ReleaseTilt = "releasetilt",
    BreakTilt = "breaktilt",
    SpinEfficiency = "spinefficiency",
    SpinRate = "spinrate",
    RelHeight = "relheight",
    RelSide = "relside",
    Extension = "extension",
    VertApprAngle = "vertapprangle",
    HorzApprAngle = "horzapprangle",
    PlateLocSide = "platelocside",
    PlateLocHeight = "platelocheight",
    PitchCall = "pitchcall",
    KorBB = "korbb",
    Balls = "balls",
    Strikes = "strikes",
    SessionType = "sessiontype",
    PlayID = "playid",
    ExitSpeed = "exitspeed",
    Angle = "angle",
    BatterSide = "batterside",
    PlayResult = "playresult",
    TaggedHitType = "taggedhittype",
    OutsOnPlay = "outsonplay",
    Batter = "batter",
    Catcher = "catcher",
    VideoClip = "videoclip",
    VideoClip2 = "videoclip2",
    VideoClip3 = "videoclip3",
    PitchUID = "pitchuid",
    PitchID = "pitchid",
    PitchGuid = "pitchguid",
    SourceFile = "source_file",
    PitchKey = "pitch_key"
  )
}

pitch_data_parse_postgres_uri <- function(uri) {
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
    all_segments <- strsplit(paste(query_segments, collapse = "&"), "&", fixed = TRUE)[[1]]
    for (segment in all_segments) {
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

read_pitch_data_db_config <- function(path) {
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

get_pitch_data_postgres_config <- function() {
  url <- Sys.getenv("PITCH_DATA_DB_URL", "")
  if (!nzchar(url)) {
    # Fallback to pitch modifications DB URL if dedicated pitch-data URL is not set.
    url <- Sys.getenv("PITCH_MOD_DB_URL", "")
  }

  if (nzchar(url)) {
    parsed <- pitch_data_parse_postgres_uri(url)
    if (!is.null(parsed)) return(parsed)
  }

  host <- Sys.getenv("PITCH_DATA_DB_HOST", "")
  user <- Sys.getenv("PITCH_DATA_DB_USER", "")
  password <- Sys.getenv("PITCH_DATA_DB_PASSWORD", "")
  dbname <- Sys.getenv("PITCH_DATA_DB_NAME", "")
  port <- suppressWarnings(as.integer(Sys.getenv("PITCH_DATA_DB_PORT", "")))
  sslmode <- Sys.getenv("PITCH_DATA_DB_SSLMODE", "require")
  channel_binding <- Sys.getenv("PITCH_DATA_DB_CHANNEL_BINDING", "require")

  if (!nzchar(host) || !nzchar(user) || !nzchar(password) || !nzchar(dbname)) {
    # Fallback to pitch modifications split env vars.
    host <- Sys.getenv("PITCH_MOD_DB_HOST", "")
    user <- Sys.getenv("PITCH_MOD_DB_USER", "")
    password <- Sys.getenv("PITCH_MOD_DB_PASSWORD", "")
    dbname <- Sys.getenv("PITCH_MOD_DB_NAME", "")
    port <- suppressWarnings(as.integer(Sys.getenv("PITCH_MOD_DB_PORT", "")))
    sslmode <- Sys.getenv("PITCH_MOD_DB_SSLMODE", sslmode)
    channel_binding <- Sys.getenv("PITCH_MOD_DB_CHANNEL_BINDING", channel_binding)
  }

  if (!nzchar(host) || !nzchar(user) || !nzchar(password) || !nzchar(dbname)) {
    # Final fallback: shared YAML DB config (works on shinyapps without env var UI).
    cfg_path <- Sys.getenv("PITCH_DATA_DB_CONFIG", "auth_db_config.yml")
    cfg <- read_pitch_data_db_config(cfg_path)
    driver <- tolower(pitch_data_or(cfg$driver, ""))
    if (!is.null(cfg) && driver %in% c("postgres", "postgresql", "neon")) {
      host <- pitch_data_or(cfg$host, "")
      user <- pitch_data_or(cfg$user, "")
      password <- pitch_data_or(cfg$password, "")
      dbname <- pitch_data_or(cfg$dbname, "")
      port <- suppressWarnings(as.integer(pitch_data_or(cfg$port, "5432")))
      sslmode <- pitch_data_or(cfg$sslmode, "require")
      channel_binding <- pitch_data_or(cfg$channel_binding, "require")
    }
  }

  if (!nzchar(host) || !nzchar(user) || !nzchar(password) || !nzchar(dbname)) return(NULL)
  if (is.na(port) || port <= 0) port <- 5432

  list(
    host = host,
    port = port,
    user = user,
    password = password,
    dbname = dbname,
    sslmode = sslmode,
    channel_binding = channel_binding
  )
}

pitch_data_backend_config <- function() {
  backend <- tolower(trimws(Sys.getenv("PITCH_DATA_BACKEND", "auto")))
  cfg <- get_pitch_data_postgres_config()
  has_postgres <- !is.null(cfg)

  if (backend %in% c("postgres", "neon", "pg") && has_postgres) {
    return(list(type = "postgres", config = cfg))
  }
  if (backend %in% c("postgres", "neon", "pg") && !has_postgres) {
    return(list(type = "csv", reason = "PITCH_DATA_BACKEND requested postgres but DB config is missing"))
  }

  if (backend == "csv") return(list(type = "csv"))
  if (has_postgres) return(list(type = "postgres", config = cfg))
  list(type = "csv")
}

pitch_data_db_connect <- function() {
  cfg <- pitch_data_backend_config()
  if (!identical(cfg$type, "postgres")) return(NULL)
  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop("RPostgres is required for PITCH_DATA_BACKEND=postgres")
  }
  params <- list(
    host = cfg$config$host,
    port = cfg$config$port,
    user = cfg$config$user,
    password = cfg$config$password,
    dbname = cfg$config$dbname,
    sslmode = pitch_data_or(cfg$config$sslmode, "require")
  )
  if (nzchar(pitch_data_or(cfg$config$channel_binding, ""))) {
    params$channel_binding <- cfg$config$channel_binding
  }
  do.call(DBI::dbConnect, c(list(RPostgres::Postgres()), params))
}

pitch_data_table_ident <- function(con) {
  schema <- Sys.getenv("PITCH_DATA_DB_SCHEMA", "public")
  table <- Sys.getenv("PITCH_DATA_DB_TABLE", "pitch_events")
  schema <- gsub("[^A-Za-z0-9_]", "_", schema)
  table <- gsub("[^A-Za-z0-9_]", "_", table)
  DBI::Id(schema = schema, table = table)
}

pitch_data_table_columns <- function(con, tbl = pitch_data_table_ident(con)) {
  schema <- gsub("[^A-Za-z0-9_]", "_", Sys.getenv("PITCH_DATA_DB_SCHEMA", "public"))
  table <- gsub("[^A-Za-z0-9_]", "_", Sys.getenv("PITCH_DATA_DB_TABLE", "pitch_events"))
  sql <- sprintf(
    "SELECT column_name
     FROM information_schema.columns
     WHERE table_schema = %s AND table_name = %s
     ORDER BY ordinal_position",
    as.character(DBI::dbQuoteLiteral(con, schema)),
    as.character(DBI::dbQuoteLiteral(con, table))
  )
  cols <- tryCatch(pitch_data_db_get_query(con, sql)$column_name, error = function(e) character(0))
  cols <- as.character(cols)
  cols <- cols[nzchar(cols)]
  if (length(cols)) return(cols)
  tryCatch(DBI::dbListFields(con, tbl), error = function(e) character(0))
}

pitch_data_cache_file <- function(school_code = "") {
  dir <- Sys.getenv("PITCH_DATA_CACHE_DIR", "/tmp")
  dir <- path.expand(dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  cfg <- pitch_data_backend_config()
  fingerprint <- if (identical(cfg$type, "postgres")) {
    paste(cfg$config$host, cfg$config$dbname, Sys.getenv("PITCH_DATA_DB_SCHEMA", "public"), Sys.getenv("PITCH_DATA_DB_TABLE", "pitch_events"), school_code, sep = "|")
  } else {
    paste("csv", school_code, sep = "|")
  }
  suffix <- digest::digest(fingerprint)
  file.path(dir, sprintf("pitch_data_cache_%s_%s.rds", toupper(trimws(ifelse(is.null(school_code), "ALL", school_code))), suffix))
}

pitch_data_cache_ttl <- function() {
  ttl <- suppressWarnings(as.numeric(Sys.getenv("PITCH_DATA_CACHE_TTL_SEC", "900")))
  if (is.na(ttl) || ttl < 0) ttl <- 900
  ttl
}

pitch_data_logger <- function(logger, label) {
  if (is.function(logger)) {
    try(logger(label), silent = TRUE)
  } else {
    message(label)
  }
}

pitch_data_db_get_query <- function(con, sql) {
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

pitch_data_load_cached <- function(path, ttl_sec) {
  if (!file.exists(path)) return(NULL)
  age <- as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "secs"))
  if (!is.finite(age) || age > ttl_sec) return(NULL)
  tryCatch(readRDS(path), error = function(...) NULL)
}

pitch_data_load_latest_cache_any <- function(school_code = "") {
  dir <- Sys.getenv("PITCH_DATA_CACHE_DIR", "/tmp")
  dir <- path.expand(dir)
  if (!dir.exists(dir)) return(NULL)
  sc <- toupper(trimws(ifelse(is.null(school_code) || !nzchar(school_code), "ALL", school_code)))
  patt <- sprintf("^pitch_data_cache_%s_.*\\.rds$", sc)
  files <- list.files(dir, pattern = patt, full.names = TRUE)
  if (!length(files)) return(NULL)
  info <- file.info(files)
  files <- files[order(info$mtime, decreasing = TRUE)]
  for (f in files) {
    obj <- tryCatch(readRDS(f), error = function(...) NULL)
    if (is.list(obj) && !is.null(obj$data) && is.data.frame(obj$data) && nrow(obj$data) > 0) {
      return(obj)
    }
  }
  NULL
}

pitch_data_save_cache <- function(path, obj) {
  tmp <- paste0(path, ".tmp")
  ok <- tryCatch({
    saveRDS(obj, tmp)
    file.rename(tmp, path)
  }, error = function(...) FALSE)
  if (!isTRUE(ok) && file.exists(tmp)) unlink(tmp)
  invisible(ok)
}

pitch_data_build_select_sql <- function(con, cols_present, school_code, start_date = NULL, end_date = NULL, chunk_size = 100000L, key_state = NULL) {
  all_cols <- pitch_data_default_columns()
  name_map <- pitch_data_storage_name_map()
  if (!length(cols_present)) cols_present <- character(0)
  cols_lower <- tolower(cols_present)

  first_match_col <- function(name) {
    hits <- which(cols_lower == tolower(name))
    if (!length(hits)) return(NULL)
    exact_hits <- hits[cols_present[hits] == name]
    pick <- if (length(exact_hits)) exact_hits[[1]] else hits[[1]]
    cols_present[[pick]]
  }

  resolve_col <- function(name) {
    first_match_col(name)
  }

  school_col <- resolve_col("school_code")
  session_date_col <- resolve_col("session_date")
  id_col <- resolve_col("id")
  has_school <- !is.null(school_col)
  has_session_date <- !is.null(session_date_col)
  has_id <- !is.null(id_col)

  app_sel <- vapply(all_cols, function(col) {
    db_col <- resolve_col(col)
    mapped <- unname(name_map[col])
    if (is.null(db_col) && length(mapped) == 1L && !is.na(mapped) && nzchar(mapped)) db_col <- resolve_col(mapped)
    if (is.null(db_col) && identical(col, "SessionType")) db_col <- resolve_col("session_type")
    if (!is.null(db_col)) {
      sprintf(
        "%s AS %s",
        as.character(DBI::dbQuoteIdentifier(con, db_col)),
        as.character(DBI::dbQuoteIdentifier(con, col))
      )
    } else {
      sprintf("NULL::text AS %s", as.character(DBI::dbQuoteIdentifier(con, col)))
    }
  }, character(1))

  meta_sel <- c()
  if (has_id) {
    meta_sel <- c(meta_sel, sprintf("%s AS %s",
                                    as.character(DBI::dbQuoteIdentifier(con, id_col)),
                                    as.character(DBI::dbQuoteIdentifier(con, "id"))))
  }
  if (has_session_date) {
    meta_sel <- c(meta_sel, sprintf("%s AS %s",
                                    as.character(DBI::dbQuoteIdentifier(con, session_date_col)),
                                    as.character(DBI::dbQuoteIdentifier(con, "session_date"))))
  }
  sel <- c(meta_sel, app_sel)

  where <- c("TRUE")
  if (has_school && nzchar(school_code)) {
    where <- c(where, sprintf("%s = %s",
                              as.character(DBI::dbQuoteIdentifier(con, school_col)),
                              as.character(DBI::dbQuoteLiteral(con, school_code))))
  }
  if (has_session_date && !is.null(start_date)) {
    where <- c(where, sprintf("%s >= %s",
                              as.character(DBI::dbQuoteIdentifier(con, session_date_col)),
                              as.character(DBI::dbQuoteLiteral(con, as.character(start_date)))))
  }
  if (has_session_date && !is.null(end_date)) {
    where <- c(where, sprintf("%s < %s",
                              as.character(DBI::dbQuoteIdentifier(con, session_date_col)),
                              as.character(DBI::dbQuoteLiteral(con, as.character(end_date)))))
  }

  if (!is.null(key_state) && has_id) {
    if (has_session_date && !is.null(key_state$session_date)) {
      key_date <- as.character(DBI::dbQuoteLiteral(con, as.character(key_state$session_date)))
      where <- c(
        where,
        sprintf("(%s < %s OR (%s = %s AND %s < %d))",
                as.character(DBI::dbQuoteIdentifier(con, session_date_col)), key_date,
                as.character(DBI::dbQuoteIdentifier(con, session_date_col)), key_date,
                as.character(DBI::dbQuoteIdentifier(con, id_col)), as.integer(key_state$id))
      )
    } else if (!is.null(key_state$id)) {
      where <- c(where, sprintf("%s < %d", as.character(DBI::dbQuoteIdentifier(con, id_col)), as.integer(key_state$id)))
    }
  }

  order_clause <- if (has_session_date && has_id) {
    sprintf("ORDER BY %s DESC NULLS LAST, %s DESC",
            as.character(DBI::dbQuoteIdentifier(con, session_date_col)),
            as.character(DBI::dbQuoteIdentifier(con, id_col)))
  } else if (has_id) {
    sprintf("ORDER BY %s DESC", as.character(DBI::dbQuoteIdentifier(con, id_col)))
  } else if (has_session_date) {
    sprintf("ORDER BY %s DESC NULLS LAST", as.character(DBI::dbQuoteIdentifier(con, session_date_col)))
  } else {
    ""
  }

  sql <- sprintf(
    "SELECT %s FROM %s WHERE %s %s LIMIT %d",
    paste(sel, collapse = ", "),
    as.character(DBI::dbQuoteIdentifier(con, pitch_data_table_ident(con))),
    paste(where, collapse = " AND "),
    order_clause,
    as.integer(chunk_size)
  )

  list(sql = sql, has_id = has_id, has_session_date = has_session_date)
}

pitch_data_fetch_range <- function(cfg, school_code, start_date = NULL, end_date = NULL, chunk_size = 100000L) {
  con <- do.call(DBI::dbConnect, c(list(RPostgres::Postgres()), cfg))
  on.exit(tryCatch(DBI::dbDisconnect(con), error = function(...) NULL), add = TRUE)

  tbl <- pitch_data_table_ident(con)
  if (!DBI::dbExistsTable(con, tbl)) return(data.frame())

  cols_present <- pitch_data_table_columns(con, tbl)
  if (!length(cols_present)) return(data.frame())
  out <- list()
  key_state <- NULL

  repeat {
    q <- pitch_data_build_select_sql(
      con = con,
      cols_present = cols_present,
      school_code = school_code,
      start_date = start_date,
      end_date = end_date,
      chunk_size = chunk_size,
      key_state = key_state
    )

    chunk <- tryCatch(pitch_data_db_get_query(con, q$sql), error = function(e) NULL)
    if (is.null(chunk) || !nrow(chunk)) break

    out[[length(out) + 1L]] <- chunk

    if (q$has_id) {
      if (!"id" %in% names(chunk)) break
      key_state <- list(
        id = suppressWarnings(as.integer(chunk$id[nrow(chunk)])),
        session_date = if (q$has_session_date) chunk$session_date[nrow(chunk)] else NULL
      )
      if (!length(key_state$id) || is.na(key_state$id)) break
    } else {
      break
    }
  }

  if (!length(out)) return(data.frame())
  dplyr::bind_rows(out)
}

pitch_data_monthly_ranges <- function(con, school_code = "") {
  tbl <- pitch_data_table_ident(con)
  if (!DBI::dbExistsTable(con, tbl)) return(list())
  cols_present <- pitch_data_table_columns(con, tbl)
  cols_lower <- tolower(cols_present)
  find_col <- function(name) {
    hits <- which(cols_lower == tolower(name))
    if (!length(hits)) return(NULL)
    cols_present[[hits[[1]]]]
  }
  session_date_col <- find_col("session_date")
  school_col <- find_col("school_code")
  if (is.null(session_date_col)) return(list())

  where <- "TRUE"
  if (!is.null(school_col) && nzchar(school_code)) {
    where <- sprintf("%s = %s",
                     as.character(DBI::dbQuoteIdentifier(con, school_col)),
                     as.character(DBI::dbQuoteLiteral(con, school_code)))
  }

  sql <- sprintf(
    "SELECT min(%s) AS min_date, max(%s) AS max_date FROM %s WHERE %s",
    as.character(DBI::dbQuoteIdentifier(con, session_date_col)),
    as.character(DBI::dbQuoteIdentifier(con, session_date_col)),
    as.character(DBI::dbQuoteIdentifier(con, tbl)),
    where
  )
  mm <- tryCatch(pitch_data_db_get_query(con, sql), error = function(e) NULL)
  if (is.null(mm) || !nrow(mm) || !"min_date" %in% names(mm) || !"max_date" %in% names(mm)) return(list())
  min_date <- mm$min_date[1]
  max_date <- mm$max_date[1]
  if (is.na(min_date) || is.na(max_date)) return(list())

  start <- as.Date(min_date)
  end <- as.Date(max_date)
  if (!is.finite(as.numeric(start)) || !is.finite(as.numeric(end))) return(list())

  first_month <- as.Date(format(start, "%Y-%m-01"))
  last_month <- as.Date(format(end, "%Y-%m-01"))
  months <- seq(first_month, last_month, by = "month")

  ranges <- lapply(months, function(m) {
    list(start = m, end = seq(m, by = "month", length.out = 2)[2])
  })
  ranges
}

load_pitch_data_from_postgres <- function(school_code = "", startup_logger = NULL) {
  cfg_raw <- pitch_data_backend_config()
  if (!identical(cfg_raw$type, "postgres")) return(NULL)

  cfg <- list(
    host = cfg_raw$config$host,
    port = cfg_raw$config$port,
    user = cfg_raw$config$user,
    password = cfg_raw$config$password,
    dbname = cfg_raw$config$dbname,
    sslmode = pitch_data_or(cfg_raw$config$sslmode, "require")
  )
  if (nzchar(pitch_data_or(cfg_raw$config$channel_binding, ""))) {
    cfg$channel_binding <- cfg_raw$config$channel_binding
  }

  cache_file <- pitch_data_cache_file(school_code)
  ttl <- pitch_data_cache_ttl()
  cached <- pitch_data_load_cached(cache_file, ttl)
  if (!is.null(cached) && is.list(cached) && !is.null(cached$data)) {
    pitch_data_logger(startup_logger, sprintf("Loaded pitch_data from cache (%d rows)", nrow(cached$data)))
    return(cached)
  }

  con <- do.call(DBI::dbConnect, c(list(RPostgres::Postgres()), cfg))
  on.exit(tryCatch(DBI::dbDisconnect(con), error = function(...) NULL), add = TRUE)

  tbl <- pitch_data_table_ident(con)
  if (!DBI::dbExistsTable(con, tbl)) {
    pitch_data_logger(startup_logger, "Neon pitch table missing; falling back to CSV")
    return(NULL)
  }

  workers <- suppressWarnings(as.integer(Sys.getenv("PITCH_DATA_PARALLEL_WORKERS", "2")))
  if (is.na(workers) || workers < 1L) workers <- 1L
  chunk_size <- suppressWarnings(as.integer(Sys.getenv("PITCH_DATA_CHUNK_SIZE", "100000")))
  if (is.na(chunk_size) || chunk_size < 1000L) chunk_size <- 100000L

  pitch_data_logger(startup_logger, sprintf("Loading pitch data from Neon (workers=%d chunk=%d)", workers, chunk_size))

  # Use one sequential keyset scan; monthly partition fan-out adds significant connect/query overhead.
  df <- pitch_data_fetch_range(cfg = cfg, school_code = school_code, chunk_size = chunk_size)

  if (nrow(df)) {
    ord <- c()
    if ("session_date" %in% names(df)) ord <- c(ord, "session_date")
    if ("id" %in% names(df)) ord <- c(ord, "id")
    if (length(ord)) df <- df[do.call(order, c(lapply(df[ord], function(x) -xtfrm(x)), list(na.last = TRUE))), , drop = FALSE]
  }

  # Keep only app-facing columns; SourceFile/all_csvs remain available for diagnostics.
  app_cols <- pitch_data_default_columns()
  for (nm in app_cols) {
    if (!nm %in% names(df)) df[[nm]] <- rep(NA, nrow(df))
  }
  df <- df[, app_cols, drop = FALSE]

  source_files <- unique(stats::na.omit(as.character(df$SourceFile)))
  out <- list(
    data = df,
    source_files = source_files,
    backend = "postgres"
  )

  pitch_data_save_cache(cache_file, out)
  pitch_data_logger(startup_logger, sprintf("Loaded %d rows from Neon (%d source files)", nrow(df), length(source_files)))
  out
}

load_pitch_data_with_backend <- function(local_data_dir = file.path(getwd(), "data"), school_code = "", startup_logger = NULL) {
  cfg <- pitch_data_backend_config()
  if (!identical(cfg$type, "postgres")) return(NULL)
  max_attempts <- suppressWarnings(as.integer(Sys.getenv("PITCH_DATA_DB_RETRIES", "3")))
  if (is.na(max_attempts) || max_attempts < 1L) max_attempts <- 3L

  last_err <- NULL
  for (attempt in seq_len(max_attempts)) {
    res <- tryCatch(
      load_pitch_data_from_postgres(school_code = school_code, startup_logger = startup_logger),
      error = function(e) {
        last_err <<- e
        NULL
      }
    )
    if (!is.null(res)) return(res)
    if (attempt < max_attempts) {
      pitch_data_logger(startup_logger, sprintf("Neon load attempt %d/%d failed; retrying", attempt, max_attempts))
      Sys.sleep(min(1, 0.25 * attempt))
    }
  }

  if (!is.null(last_err)) {
    pitch_data_logger(startup_logger, paste("Neon load failed:", last_err$message))
  }
  stale <- pitch_data_load_latest_cache_any(school_code = school_code)
  if (!is.null(stale)) {
    pitch_data_logger(startup_logger, sprintf("Using stale cached pitch_data snapshot (%d rows) after Neon failure", nrow(stale$data)))
    return(stale)
  }
  NULL
}

ensure_pitch_data_schema <- function(con = NULL, schema_sql_path = file.path("db", "pitch_data_schema.sql")) {
  close_con <- FALSE
  if (is.null(con)) {
    con <- pitch_data_db_connect()
    if (is.null(con)) stop("No Postgres backend configured for pitch data")
    close_con <- TRUE
  }
  on.exit(if (close_con) tryCatch(DBI::dbDisconnect(con), error = function(...) NULL), add = TRUE)

  if (!file.exists(schema_sql_path)) {
    stop("Schema SQL not found: ", schema_sql_path)
  }

  sql <- paste(readLines(schema_sql_path, warn = FALSE), collapse = "\n")
  chunks <- strsplit(sql, ";", fixed = TRUE)[[1]]
  chunks <- trimws(chunks)
  chunks <- chunks[nzchar(chunks)]
  for (stmt in chunks) {
    DBI::dbExecute(con, stmt)
  }

  invisible(TRUE)
}

sync_csv_file_to_neon <- function(con, csv_path, school_code = "") {
  school_code <- toupper(trimws(as.character(school_code)))
  if (!nzchar(school_code)) school_code <- toupper(trimws(Sys.getenv("TEAM_CODE", "OSU")))

  schema <- gsub("[^A-Za-z0-9_]", "_", Sys.getenv("PITCH_DATA_DB_SCHEMA", "public"))
  mtbl <- DBI::Id(schema = schema, table = "pitch_data_files")
  etbl <- DBI::Id(schema = schema, table = Sys.getenv("PITCH_DATA_DB_TABLE", "pitch_events"))

  source_file <- normalizePath(csv_path, winslash = "/", mustWork = FALSE)
  local_mtime <- suppressWarnings(as.numeric(file.info(csv_path)$mtime))

  existing_sql <- sprintf(
    "SELECT file_id, file_checksum, row_count, EXTRACT(EPOCH FROM file_mtime) AS file_mtime_epoch
     FROM %s
     WHERE school_code = %s AND source_file = %s",
    as.character(DBI::dbQuoteIdentifier(con, mtbl)),
    as.character(DBI::dbQuoteLiteral(con, school_code)),
    as.character(DBI::dbQuoteLiteral(con, source_file))
  )
  existing <- tryCatch(pitch_data_db_get_query(con, existing_sql), error = function(e) data.frame())
  if (nrow(existing) == 1L) {
    file_id_existing <- suppressWarnings(as.integer(existing$file_id[[1]]))
    expected_rows <- suppressWarnings(as.integer(existing$row_count[[1]]))
    remote_mtime <- suppressWarnings(as.numeric(existing$file_mtime_epoch[[1]]))
    same_mtime <- is.finite(local_mtime) && is.finite(remote_mtime) && abs(local_mtime - remote_mtime) < 1
    if (same_mtime && is.finite(file_id_existing) && file_id_existing > 0 && is.finite(expected_rows) && expected_rows >= 0) {
      cnt_sql <- sprintf(
        "SELECT COUNT(*) AS n FROM %s WHERE school_code = %s AND file_id = %d",
        as.character(DBI::dbQuoteIdentifier(con, etbl)),
        as.character(DBI::dbQuoteLiteral(con, school_code)),
        file_id_existing
      )
      existing_rows <- tryCatch(pitch_data_db_get_query(con, cnt_sql)$n[[1]], error = function(e) NA_integer_)
      if (is.finite(existing_rows) && as.integer(existing_rows) == expected_rows) {
        out <- 0L
        attr(out, "skipped") <- TRUE
        return(out)
      }
    }
  }

  checksum <- digest::digest(file = csv_path, algo = "xxhash64")
  if (nrow(existing) == 1L) {
    same_checksum <- !is.na(existing$file_checksum[[1]]) &&
      identical(as.character(existing$file_checksum[[1]]), as.character(checksum))
    if (same_checksum) {
      file_id_existing <- suppressWarnings(as.integer(existing$file_id[[1]]))
      expected_rows <- suppressWarnings(as.integer(existing$row_count[[1]]))
      if (is.finite(file_id_existing) && file_id_existing > 0 && is.finite(expected_rows) && expected_rows >= 0) {
        cnt_sql <- sprintf(
          "SELECT COUNT(*) AS n FROM %s WHERE school_code = %s AND file_id = %d",
          as.character(DBI::dbQuoteIdentifier(con, etbl)),
          as.character(DBI::dbQuoteLiteral(con, school_code)),
          file_id_existing
        )
        existing_rows <- tryCatch(pitch_data_db_get_query(con, cnt_sql)$n[[1]], error = function(e) NA_integer_)
        if (is.finite(existing_rows) && as.integer(existing_rows) == expected_rows) {
          out <- 0L
          attr(out, "skipped") <- TRUE
          return(out)
        }
      }
    }
  }

  cols_needed <- pitch_data_default_columns()
  df <- suppressMessages(readr::read_csv(
    csv_path,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE,
    show_col_types = FALSE
  ))

  # Alias normalization for compatibility with app expectations.
  canon_aliases <- list(
    InducedVertBreak = c("IVB"),
    HorzBreak        = c("HB"),
    RelSpeed         = c("Velo"),
    ReleaseTilt      = c("ReleaseAngle", "SpinAxis3dTransverseAngle"),
    BreakTilt        = c("BreakAngle", "SpinAxis"),
    SpinEfficiency   = c("SpinEff", "SpinAxis3dSpinEfficiency"),
    SpinRate         = c("Spin"),
    RelHeight        = c("RelZ"),
    RelSide          = c("RelX"),
    VertApprAngle    = c("VAA"),
    HorzApprAngle    = c("HAA"),
    PlateLocSide     = c("PlateX"),
    PlateLocHeight   = c("PlateZ")
  )
  nm <- names(df)
  for (canon in names(canon_aliases)) {
    if (!(canon %in% nm)) {
      for (al in canon_aliases[[canon]]) {
        hit <- which(tolower(nm) == tolower(al))
        if (length(hit) == 1L) {
          names(df)[hit] <- canon
          nm <- names(df)
          break
        }
      }
    }
  }

  for (nm in cols_needed) {
    if (!nm %in% names(df)) df[[nm]] <- rep(NA_character_, nrow(df))
  }

  # Derive session/date fields used for indexing and keyset paging.
  file_lower <- tolower(csv_path)
  session_type <- if (grepl("[/\\\\]practice[/\\\\]", file_lower)) "Bullpen" else "Live"

  if (!"SessionType" %in% names(df) || all(is.na(df$SessionType) | !nzchar(df$SessionType))) {
    df$SessionType <- session_type
  }

  parsed_date <- suppressWarnings(as.Date(df$Date, format = "%Y-%m-%d"))
  fallback_date <- suppressWarnings(as.Date(df$Date, format = "%m/%d/%Y"))
  parsed_date[is.na(parsed_date)] <- fallback_date[is.na(parsed_date)]

  if (all(is.na(parsed_date))) {
    m <- stringr::str_match(csv_path, "(20\\d{2})_(0[1-9]|1[0-2])_(0[1-9]|[12]\\d|3[01])")
    guessed <- if (!all(is.na(m))) as.Date(paste(m[2], m[3], m[4], sep = "-")) else NA
    parsed_date <- rep(guessed, nrow(df))
  }

  # File manifest row.
  DBI::dbExecute(con, sprintf(
    "INSERT INTO %s.schools (school_code) VALUES (%s) ON CONFLICT (school_code) DO NOTHING",
    Sys.getenv("PITCH_DATA_DB_SCHEMA", "public"),
    as.character(DBI::dbQuoteLiteral(con, school_code))
  ))

  up_sql <- sprintf(
    "INSERT INTO %s (school_code, source_file, file_checksum, file_mtime, row_count)
     VALUES (%s, %s, %s, to_timestamp(%s), %d)
     ON CONFLICT (school_code, source_file)
     DO UPDATE SET file_checksum = EXCLUDED.file_checksum,
                   file_mtime = EXCLUDED.file_mtime,
                   row_count = EXCLUDED.row_count,
                   loaded_at = now()",
    as.character(DBI::dbQuoteIdentifier(con, mtbl)),
    as.character(DBI::dbQuoteLiteral(con, school_code)),
    as.character(DBI::dbQuoteLiteral(con, source_file)),
    as.character(DBI::dbQuoteLiteral(con, checksum)),
    local_mtime,
    nrow(df)
  )
  DBI::dbExecute(con, up_sql)

  file_id_sql <- sprintf(
    "SELECT file_id FROM %s WHERE school_code = %s AND source_file = %s",
    as.character(DBI::dbQuoteIdentifier(con, mtbl)),
    as.character(DBI::dbQuoteLiteral(con, school_code)),
    as.character(DBI::dbQuoteLiteral(con, source_file))
  )
  file_id <- pitch_data_db_get_query(con, file_id_sql)$file_id[[1]]

  # Replace current rows for this file then bulk-insert refreshed rows.
  del_sql <- sprintf(
    "DELETE FROM %s WHERE school_code = %s AND file_id = %d",
    as.character(DBI::dbQuoteIdentifier(con, etbl)),
    as.character(DBI::dbQuoteLiteral(con, school_code)),
    as.integer(file_id)
  )
  DBI::dbExecute(con, del_sql)

  df$school_code <- school_code
  df$file_id <- as.integer(file_id)
  df$session_date <- as.character(parsed_date)
  df$session_type <- as.character(df$SessionType)
  df$pitch_key <- as.character(if ("PitchKey" %in% names(df)) df$PitchKey else NA_character_)
  df$source_file <- source_file

  name_map <- pitch_data_storage_name_map()
  db_df <- data.frame(
    school_code = as.character(df$school_code),
    file_id = as.integer(df$file_id),
    session_date = as.character(df$session_date),
    session_type = as.character(df$session_type),
    source_file = as.character(df$source_file),
    pitch_key = as.character(df$pitch_key),
    stringsAsFactors = FALSE
  )

  app_cols <- setdiff(pitch_data_default_columns(), c("SourceFile", "PitchKey"))
  for (nm in app_cols) {
    db_nm <- unname(name_map[[nm]])
    if (is.na(db_nm) || !nzchar(db_nm)) next
    db_df[[db_nm]] <- if (nm %in% names(df)) as.character(df[[nm]]) else NA_character_
  }

  DBI::dbWriteTable(con, etbl, db_df, append = TRUE, row.names = FALSE)
  invisible(nrow(db_df))
}

sync_csv_tree_to_neon <- function(data_dir = file.path("data"), school_code = "", workers = 2L) {
  con <- pitch_data_db_connect()
  on.exit(tryCatch(DBI::dbDisconnect(con), error = function(...) NULL), add = TRUE)

  ensure_pitch_data_schema(con)

  csvs <- list.files(data_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  csvs <- csvs[grepl("([/\\\\]practice[/\\\\])|([/\\\\]v3[/\\\\])", tolower(csvs))]
  if (!length(csvs)) {
    message("No pitch CSV files found under ", data_dir)
    return(invisible(0L))
  }

  workers <- as.integer(workers)
  if (is.na(workers) || workers < 1L) workers <- 1L

  # Use per-file transaction for resilience and parallel workers where available.
  do_one <- function(p) {
    c <- pitch_data_db_connect()
    on.exit(tryCatch(DBI::dbDisconnect(c), error = function(...) NULL), add = TRUE)
    tryCatch({
      DBI::dbBegin(c)
      n <- sync_csv_file_to_neon(c, p, school_code = school_code)
      skipped <- isTRUE(attr(n, "skipped"))
      DBI::dbCommit(c)
      list(path = p, rows = n, ok = TRUE, skipped = skipped, err = "")
    }, error = function(e) {
      tryCatch(DBI::dbRollback(c), error = function(...) NULL)
      list(path = p, rows = 0L, ok = FALSE, skipped = FALSE, err = e$message)
    })
  }

  if (workers > 1L) {
    message("Using sequential file sync for DB safety (RPostgres fork parallel disabled).")
  }
  results <- vector("list", length(csvs))
  for (i in seq_along(csvs)) {
    results[[i]] <- do_one(csvs[[i]])
    if (i %% 25L == 0L || i == length(csvs)) {
      ok_now <- vapply(results[seq_len(i)], function(x) is.list(x) && isTRUE(x$ok), logical(1))
      rows_now <- sum(vapply(results[seq_len(i)][ok_now], function(x) as.integer(x$rows), integer(1)), na.rm = TRUE)
      skipped_now <- sum(vapply(results[seq_len(i)][ok_now], function(x) isTRUE(x$skipped), logical(1)), na.rm = TRUE)
      fail_now <- i - sum(ok_now)
      message(sprintf("Pitch sync progress: %d/%d files | rows=%d | skipped=%d | failed=%d",
                      i, length(csvs), rows_now, skipped_now, fail_now))
    }
  }

  ok <- vapply(results, function(x) isTRUE(x$ok), logical(1))
  failures <- results[!ok]
  if (length(failures)) {
    message("Failed file ingests: ", length(failures))
    for (f in failures) {
      message(" - ", f$path, " :: ", f$err)
    }
  }

  total_rows <- sum(vapply(results[ok], function(x) as.integer(x$rows), integer(1)), na.rm = TRUE)
  skipped_files <- sum(vapply(results[ok], function(x) isTRUE(x$skipped), logical(1)), na.rm = TRUE)
  synced_files <- sum(ok) - skipped_files
  message(sprintf("Neon sync complete: files=%d success=%d failed=%d synced=%d skipped=%d rows=%d",
                  length(csvs), sum(ok), sum(!ok), synced_files, skipped_files, total_rows))

  invisible(total_rows)
}
