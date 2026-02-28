#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(httr2)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(tibble)
  library(stringr)
})

source("video_map_helpers.R")
source("pitch_data_service.R")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

team_code <- toupper(trimws(Sys.getenv("TEAM_CODE", "PCU")))
if (!nzchar(team_code)) team_code <- "PCU"

cloud_name <- Sys.getenv("CLOUDINARY_CLOUD_NAME", "")
api_key <- Sys.getenv("CLOUDINARY_API_KEY", "")
api_secret <- Sys.getenv("CLOUDINARY_API_SECRET", "")
folder_prefix <- Sys.getenv("CLOUDINARY_FOLDER", "trackman")
max_pages <- suppressWarnings(as.integer(Sys.getenv("CLOUDINARY_REBUILD_MAX_PAGES", "200")))
if (is.na(max_pages) || max_pages < 1L) max_pages <- 200L

if (!nzchar(cloud_name) || !nzchar(api_key) || !nzchar(api_secret)) {
  stop("Missing Cloudinary Admin API credentials. Set CLOUDINARY_CLOUD_NAME, CLOUDINARY_API_KEY, CLOUDINARY_API_SECRET.", call. = FALSE)
}

cfg <- get_pitch_data_postgres_config()
if (is.null(cfg)) {
  stop("Pitch-data Postgres config not found. Set PITCH_DATA_DB_URL (or split vars).", call. = FALSE)
}

con <- do.call(DBI::dbConnect, c(list(RPostgres::Postgres()), cfg))
on.exit(tryCatch(DBI::dbDisconnect(con), error = function(...) NULL), add = TRUE)

play_df <- DBI::dbGetQuery(
  con,
  sprintf(
    "SELECT DISTINCT lower(playid) AS play_id
     FROM %s
     WHERE school_code = %s
       AND playid IS NOT NULL
       AND btrim(playid) <> ''",
    as.character(DBI::dbQuoteIdentifier(con, pitch_data_table_ident(con))),
    as.character(DBI::dbQuoteLiteral(con, team_code))
  )
)
if (!nrow(play_df)) {
  stop("No play IDs found in pitch_events for school_code=", team_code, call. = FALSE)
}

play_df <- play_df %>%
  mutate(prefix = substr(play_id, 1, 12)) %>%
  group_by(prefix) %>%
  mutate(n_prefix = n()) %>%
  ungroup()

unique_prefix <- play_df %>% filter(n_prefix == 1L) %>% select(prefix, play_id)
if (!nrow(unique_prefix)) {
  stop("No unique play_id prefixes found; cannot safely map Cloudinary assets.", call. = FALSE)
}

fetch_page <- function(next_cursor = NULL) {
  endpoint <- sprintf("https://api.cloudinary.com/v1_1/%s/resources/video/upload", cloud_name)
  req <- request(endpoint) %>%
    req_auth_basic(api_key, api_secret) %>%
    req_url_query(
      prefix = folder_prefix,
      max_results = 500L
    ) %>%
    req_timeout(60)
  if (!is.null(next_cursor) && nzchar(next_cursor)) {
    req <- req %>% req_url_query(next_cursor = next_cursor)
  }
  resp <- req_perform(req)
  resp_body_json(resp, simplifyVector = TRUE)
}

rows <- list()
cursor <- NULL
page <- 0L
repeat {
  page <- page + 1L
  body <- fetch_page(cursor)
  res <- body$resources
  page_df <- tibble()
  if (is.data.frame(res) && nrow(res)) {
    pid <- as.character(res$public_id %||% character(0))
    surl <- as.character(res$secure_url %||% "")
    url <- as.character(res$url %||% "")
    created <- as.character(res$created_at %||% NA_character_)
    page_df <- tibble(
      cloudinary_public_id = pid,
      cloudinary_url = ifelse(nzchar(surl), surl, url),
      uploaded_at = created
    )
  } else if (is.list(res) && length(res)) {
    page_rows <- lapply(res, function(x) {
      if (!is.list(x)) return(NULL)
      tibble(
        cloudinary_public_id = as.character(x$public_id %||% ""),
        cloudinary_url = as.character(x$secure_url %||% x$url %||% ""),
        uploaded_at = as.character(x$created_at %||% NA_character_)
      )
    })
    page_df <- bind_rows(page_rows)
  }

  if (nrow(page_df)) {
    page_df <- page_df %>%
      mutate(
        cloudinary_public_id = trimws(as.character(cloudinary_public_id)),
        seg = strsplit(cloudinary_public_id, "/", fixed = TRUE),
        seg_len = vapply(seg, length, integer(1)),
        camera_slot = vapply(seg, function(v) if (length(v) >= 2L) v[[length(v) - 1L]] else "", character(1)),
        play_prefix = tolower(vapply(seg, function(v) if (length(v) >= 1L) v[[length(v)]] else "", character(1))),
        session_id = vapply(seg, function(v) if (length(v) >= 3L) v[[length(v) - 2L]] else "", character(1))
      ) %>%
      filter(
        seg_len >= 4L,
        camera_slot %in% c("VideoClip", "VideoClip2", "VideoClip3"),
        nzchar(play_prefix),
        nzchar(cloudinary_url)
      ) %>%
      select(session_id, camera_slot, play_prefix, cloudinary_url, cloudinary_public_id, uploaded_at)

    if (nrow(page_df)) {
      rows[[length(rows) + 1L]] <- page_df
    }
  }
  cursor <- body$next_cursor %||% NULL
  if (is.null(cursor) || !nzchar(cursor) || page >= max_pages) break
}

assets <- bind_rows(rows)
if (!nrow(assets)) {
  stop("No Cloudinary assets found under prefix=", folder_prefix, call. = FALSE)
}

mapped <- assets %>%
  inner_join(unique_prefix, by = c("play_prefix" = "prefix")) %>%
  transmute(
    session_id = session_id,
    play_id = play_id,
    camera_slot = camera_slot,
    camera_name = NA_character_,
    camera_target = NA_character_,
    video_type = "RecoveredFromCloudinary",
    azure_blob = NA_character_,
    azure_md5 = NA_character_,
    cloudinary_url = cloudinary_url,
    cloudinary_public_id = cloudinary_public_id,
    uploaded_at = uploaded_at,
    school_code = team_code
  ) %>%
  distinct(session_id, camera_slot, play_id, .keep_all = TRUE)

cat("Cloudinary assets scanned:", nrow(assets), "\n")
cat("Rows matched to", team_code, "play_ids:", nrow(mapped), "\n")

if (!nrow(mapped)) {
  stop("No recoverable rows matched team play IDs. Check CLOUDINARY_FOLDER and whether assets belong to this team.", call. = FALSE)
}

ok <- video_map_write_rows_to_neon(mapped)
if (!isTRUE(ok)) {
  stop("Failed writing recovered rows to Neon video_map table", call. = FALSE)
}

cat("Recovered rows written to Neon video map table:", nrow(mapped), "\n")
