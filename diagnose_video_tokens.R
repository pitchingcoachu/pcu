#!/usr/bin/env Rscript
# Read-only diagnostic: dumps the RAW TrackMan videotokens + videometadata
# responses for one session_id, unfiltered, so we can see exactly what
# TrackMan's API returns (token count, types, per-play metadata) before any
# of our own slot-inference logic touches it. Does not write anything.
#
# Usage: Rscript diagnose_video_tokens.R <session_id> [play_id_filter]

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(glue)
})

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

args <- commandArgs(trailingOnly = TRUE)
session_id <- if (length(args) >= 1) args[[1]] else "6f4d67db-c3b1-42b6-97ba-38ab8b949c1d"
play_filter <- if (length(args) >= 2) tolower(args[[2]]) else NA_character_

client_id <- Sys.getenv("TM_CLIENT_ID")
client_secret <- Sys.getenv("TM_CLIENT_SECRET")
if (!nzchar(client_id) || !nzchar(client_secret)) stop("Missing TM_CLIENT_ID/TM_CLIENT_SECRET")

message(glue(">> Requesting TrackMan token"))
tok_res <- request("https://login.trackmanbaseball.com/connect/token") |>
  req_body_form(client_id = client_id, client_secret = client_secret, grant_type = "client_credentials") |>
  req_timeout(20) |>
  req_error(is_error = ~ FALSE) |>
  req_perform()
if (resp_status(tok_res) >= 400) stop(glue("Token request failed: {resp_status(tok_res)} {resp_body_string(tok_res)}"))
access_token <- resp_body_json(tok_res, simplifyVector = TRUE)$access_token

fetch_raw <- function(kind, env) {
  url <- glue("https://dataapi.trackmanbaseball.com/api/v1/media/{env}/{kind}/{session_id}")
  message(glue(">> GET {url}"))
  res <- request(url) |>
    req_headers(Authorization = paste("Bearer", access_token), accept = "text/plain") |>
    req_timeout(20) |>
    req_error(is_error = ~ FALSE) |>
    req_perform()
  status <- resp_status(res)
  message(glue("   status={status}"))
  if (status >= 400) {
    return(NULL)
  }
  hdrs <- resp_headers(res)
  body_len <- as.integer(hdrs[["content-length"]] %||% NA_character_)
  if (!is.na(body_len) && body_len == 0) {
    message("   empty body, skipping")
    return(NULL)
  }
  raw_text <- tryCatch(resp_body_string(res), error = function(e) { message(glue("   body read error: {e$message}")); NULL })
  if (is.null(raw_text)) return(NULL)
  message(glue("   raw response length: {nchar(raw_text)} chars"))
  raw_text
}

env <- "practice"
message(glue("==== ENV: {env} ===="))

meta_raw <- fetch_raw("videometadata", env)
if (!is.null(meta_raw)) {
  parsed_meta <- tryCatch(fromJSON(meta_raw, simplifyVector = TRUE), error = function(e) NULL)
  if (!is.null(parsed_meta) && is.data.frame(parsed_meta)) {
    message(glue(">> videometadata columns: {paste(names(parsed_meta), collapse=', ')}"))
    message(glue(">> videometadata total rows: {nrow(parsed_meta)}"))

    if (!is.na(play_filter) && "playId" %in% names(parsed_meta)) {
      sub <- parsed_meta[tolower(parsed_meta$playId) == play_filter, , drop = FALSE]
      message(glue(">> Rows matching playId={play_filter}: {nrow(sub)}"))
      message(">> FULL rows for this play:")
      print(toJSON(sub, pretty = TRUE, auto_unbox = TRUE))
    }

    # General duplicate-playId diagnostic across ALL plays, to see how common
    # multi-row-per-play is and whether rows within a play are otherwise
    # distinguishable (videoClipId, createdAt, etc).
    if ("playId" %in% names(parsed_meta)) {
      dup_counts <- table(parsed_meta$playId)
      multi <- names(dup_counts[dup_counts > 1])
      message(glue(">> plays with >1 metadata row: {length(multi)}"))
      if (length(multi) > 0) {
        sample_play <- multi[[1]]
        message(glue(">> Sample multi-row play ({sample_play}):"))
        sample_sub <- parsed_meta[parsed_meta$playId == sample_play, , drop = FALSE]
        print(toJSON(sample_sub, pretty = TRUE, auto_unbox = TRUE))
      }
    }
  }
}

tokens_raw <- fetch_raw("videotokens", env)
if (!is.null(tokens_raw)) {
  parsed <- tryCatch(fromJSON(tokens_raw, simplifyVector = TRUE), error = function(e) NULL)
  if (!is.null(parsed) && is.data.frame(parsed)) {
    message(glue(">> videotokens columns: {paste(names(parsed), collapse=', ')}"))
    print(parsed)
  }
}
