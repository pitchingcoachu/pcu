#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
})

if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

source("pitch_data_service.R")

args <- commandArgs(trailingOnly = TRUE)
school_code <- if (length(args) >= 1L) args[[1]] else Sys.getenv("TEAM_CODE", "PCU")
school_code <- toupper(trimws(school_code))
if (!nzchar(school_code)) school_code <- "PCU"

cat("Starting Neon dedupe for school_code=", school_code, "\n", sep = "")
con <- pitch_data_db_connect()
on.exit(tryCatch(DBI::dbDisconnect(con), error = function(...) NULL), add = TRUE)
if (is.null(con)) stop("Unable to connect to Neon using PITCH_DATA_DB_URL")

schema <- gsub("[^A-Za-z0-9_]", "_", Sys.getenv("PITCH_DATA_DB_SCHEMA", "public"))
events_tbl <- Sys.getenv("PITCH_DATA_DB_TABLE", "pitch_events")
events_id <- as.character(DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = events_tbl)))
files_id <- as.character(DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = "pitch_data_files")))
school_lit <- as.character(DBI::dbQuoteLiteral(con, school_code))

count_sql <- function(where = "TRUE") {
  sprintf("SELECT COUNT(*) AS n FROM %s WHERE %s", events_id, where)
}

before_total <- DBI::dbGetQuery(con, count_sql(sprintf("school_code = %s", school_lit)))$n[[1]]
before_missing <- DBI::dbGetQuery(
  con,
  count_sql(sprintf("school_code = %s AND (pitch_key IS NULL OR btrim(pitch_key) = '')", school_lit))
)$n[[1]]
cat("Before: rows=", before_total, " missing_pitch_key=", before_missing, "\n", sep = "")

# 1) Backfill missing pitch_key values in-db.
backfill_sql <- sprintf(
  "UPDATE %s
   SET pitch_key = md5(concat_ws('|',
      coalesce(date::text, ''),
      coalesce(pitcher, ''),
      coalesce(batter, ''),
      coalesce(playid, ''),
      coalesce(pitchcall, ''),
      coalesce(playresult, ''),
      coalesce(taggedpitchtype, ''),
      coalesce(balls, ''),
      coalesce(strikes, ''),
      coalesce(relspeed, ''),
      coalesce(inducedvertbreak, ''),
      coalesce(horzbreak, ''),
      coalesce(extension, ''),
      coalesce(platelocside, ''),
      coalesce(platelocheight, '')
   ))
   WHERE school_code = %s
     AND (pitch_key IS NULL OR btrim(pitch_key) = '')",
  events_id, school_lit
)
backfilled <- DBI::dbExecute(con, backfill_sql)
cat("Backfilled pitch_key rows: ", backfilled, "\n", sep = "")

# 2) Delete duplicate pitch keys (keep newest by session_date/id).
dedupe_sql <- sprintf(
  "WITH ranked AS (
      SELECT id,
             row_number() OVER (
               PARTITION BY pitch_key
               ORDER BY session_date DESC NULLS LAST, id DESC
             ) AS rn
      FROM %s
      WHERE school_code = %s
        AND pitch_key IS NOT NULL
        AND btrim(pitch_key) <> ''
    )
    DELETE FROM %s e
    USING ranked r
    WHERE e.id = r.id
      AND r.rn > 1",
  events_id, school_lit, events_id
)
deleted <- DBI::dbExecute(con, dedupe_sql)
cat("Deleted duplicate rows: ", deleted, "\n", sep = "")

# 3) Reconcile manifest row_count so fast-skip stays valid and duplicates don't return.
manifest_sql <- sprintf(
  "UPDATE %s f
   SET row_count = COALESCE((
      SELECT COUNT(*)::int
      FROM %s e
      WHERE e.school_code = f.school_code
        AND e.file_id = f.file_id
    ), 0),
    loaded_at = now()
   WHERE f.school_code = %s",
  files_id, events_id, school_lit
)
updated_files <- DBI::dbExecute(con, manifest_sql)
cat("Updated manifest rows: ", updated_files, "\n", sep = "")

# 4) Add per-school unique guard to prevent duplicate PitchKey rows from reappearing.
idx_name <- sprintf(
  "idx_pitch_events_%s_pitch_key_unique",
  tolower(gsub("[^A-Za-z0-9_]", "_", school_code))
)
guard_sql <- sprintf(
  "CREATE UNIQUE INDEX IF NOT EXISTS %s ON %s (school_code, pitch_key)
   WHERE school_code = %s AND pitch_key IS NOT NULL AND btrim(pitch_key) <> ''",
  as.character(DBI::dbQuoteIdentifier(con, idx_name)),
  events_id,
  school_lit
)
DBI::dbExecute(con, guard_sql)
cat("Ensured unique guard index: ", idx_name, "\n", sep = "")

after_total <- DBI::dbGetQuery(con, count_sql(sprintf("school_code = %s", school_lit)))$n[[1]]
after_missing <- DBI::dbGetQuery(
  con,
  count_sql(sprintf("school_code = %s AND (pitch_key IS NULL OR btrim(pitch_key) = '')", school_lit))
)$n[[1]]
cat("After: rows=", after_total, " missing_pitch_key=", after_missing, "\n", sep = "")
cat("Done.\n")
