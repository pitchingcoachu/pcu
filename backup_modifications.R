# Emergency backup script for pitch modifications
# This can be run manually to ensure modifications are properly backed up

library(DBI)
library(RSQLite)
library(readr)

`%||%` <- function(a, b) if (is.null(a) || !nzchar(a)) b else a

read_pitch_mod_db_config <- function(path) {
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

get_pitch_mod_postgres_config <- function() {
  config_path <- Sys.getenv("PITCH_MOD_DB_CONFIG", "auth_db_config.yml")
  cfg <- read_pitch_mod_db_config(config_path)
  if (is.null(cfg)) return(NULL)
  driver <- tolower(cfg$driver %||% "")
  if (!driver %in% c("postgres", "postgresql", "neon")) return(NULL)
  port <- suppressWarnings(as.integer(cfg$port %||% "5432"))
  if (is.na(port) || port <= 0) port <- 5432
  list(
    host = cfg$host %||% "",
    port = port,
    user = cfg$user %||% "",
    password = cfg$password %||% "",
    dbname = cfg$dbname %||% "",
    sslmode = cfg$sslmode %||% "require",
    channel_binding = cfg$channel_binding %||% "require"
  )
}

pitch_mod_table_name <- function() {
  tbl <- Sys.getenv("PITCH_MOD_DB_TABLE", "modifications")
  tbl <- gsub("[^A-Za-z0-9_]", "_", tbl, perl = TRUE)
  if (!nzchar(tbl)) tbl <- "modifications"
  tbl
}

pitch_mod_namespace <- function() {
  ns <- Sys.getenv("PITCH_MOD_NAMESPACE", "")
  if (!nzchar(ns)) ns <- basename(getwd())
  ns <- gsub("[^A-Za-z0-9_]", "_", ns, perl = TRUE)
  if (!nzchar(ns)) ns <- "default"
  ns
}

pitch_mod_table_clause <- function(con) {
  DBI::dbQuoteIdentifier(con, pitch_mod_table_name())
}

pitch_mod_namespace_clause <- function(con) {
  DBI::dbQuoteString(con, pitch_mod_namespace())
}

connect_pitch_mod_backend <- function() {
  cfg <- get_pitch_mod_postgres_config()
  if (!is.null(cfg) && nzchar(cfg$host) && nzchar(cfg$dbname) &&
      nzchar(cfg$user) && nzchar(cfg$password)) {
    if (!requireNamespace("RPostgres", quietly = TRUE)) {
      stop("RPostgres is required to connect to the Neon modifications backend.")
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
    return(do.call(DBI::dbConnect, c(list(RPostgres::Postgres()), params)))
  }
  override <- Sys.getenv("PITCH_MOD_DB_PATH", unset = "")
  if (nzchar(override)) {
    path <- path.expand(override)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    return(dbConnect(RSQLite::SQLite(), path))
  }
  if (file.access(".", 2) == 0) {
    return(dbConnect(RSQLite::SQLite(), "pitch_modifications.db"))
  }
  alt <- file.path(tools::R_user_dir("pcu_pitch_dashboard", which = "data"), "pitch_modifications.db")
  dir.create(dirname(alt), recursive = TRUE, showWarnings = FALSE)
  dbConnect(RSQLite::SQLite(), alt)
}

get_modifications_export_path <- function() {
  override <- Sys.getenv("PITCH_MOD_EXPORT_PATH", unset = "")
  if (nzchar(override)) {
    path <- path.expand(override)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    return(path)
  }
  file.path("data", "pitch_type_modifications_export.csv")
}

# Backup function
backup_modifications <- function() {
  export_path <- get_modifications_export_path()
  
  con <- tryCatch(connect_pitch_mod_backend(), error = function(e) e)
  if (inherits(con, "error") || is.null(con)) {
    cat("Could not connect to modifications backend:", conditionMessage(con), "\n")
    return(FALSE)
  }
  on.exit(dbDisconnect(con), add = TRUE)
  
  tryCatch({
    tbl <- as.character(pitch_mod_table_clause(con))
    ns_clause <- pitch_mod_namespace_clause(con)
    mods <- dbGetQuery(con, sprintf("SELECT * FROM %s WHERE namespace = %s ORDER BY created_at", tbl, ns_clause))
    
    if (nrow(mods) > 0) {
      dir.create(dirname(export_path), recursive = TRUE, showWarnings = FALSE)
      write_csv(mods, export_path)
      cat("Backed up", nrow(mods), "modifications to:", export_path, "\n")
      
      # Also create a timestamped backup
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      backup_path <- file.path("data", paste0("pitch_type_modifications_backup_", timestamp, ".csv"))
      write_csv(mods, backup_path)
      cat("Created timestamped backup:", backup_path, "\n")
      
      return(TRUE)
    } else {
      cat("No modifications found in database\n")
      return(FALSE)
    }
  }, error = function(e) {
    cat("Error backing up modifications:", e$message, "\n")
    return(FALSE)
  })
}

# Run backup if called directly
if (!interactive()) {
  result <- backup_modifications()
  if (result) {
    cat("Backup completed successfully\n")
  } else {
    cat("Backup failed or no data to backup\n")
  }
}
