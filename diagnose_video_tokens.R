#!/usr/bin/env Rscript
# Read-only diagnostic: dumps the RAW TrackMan videotokens + videometadata
# responses for one session_id, unfiltered, so we can see exactly what
# TrackMan's API returns (token count, types, per-play metadata) before any
# of our own slot-inference logic touches it. Does not write anything.
#
# Usage: Rscript diagnose_video_tokens.R <session_id>

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(glue)
})

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

args <- commandArgs(trailingOnly = TRUE)
session_id <- if (length(args) >= 1) args[[1]] else "6f4d67db-c3b1-42b6-97ba-38ab8b949c1d"

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
    message(glue("   body: {resp_body_string(res)}"))
    return(NULL)
  }
  raw_text <- resp_body_string(res)
  message(glue("   raw response length: {nchar(raw_text)} chars"))
  raw_text
}

for (env in c("practice", "game")) {
  message(glue("==== ENV: {env} ===="))
  tokens_raw <- fetch_raw("videotokens", env)
  if (!is.null(tokens_raw)) {
    parsed <- tryCatch(fromJSON(tokens_raw, simplifyVector = TRUE), error = function(e) NULL)
    if (!is.null(parsed)) {
      message(glue(">> videotokens: {if (is.data.frame(parsed)) nrow(parsed) else length(parsed)} entries"))
      if (is.data.frame(parsed) && "type" %in% names(parsed)) {
        print(table(parsed$type))
      }
      message(">> FULL RAW videotokens JSON:")
      print(toJSON(parsed, pretty = TRUE, auto_unbox = TRUE))
    }
  }

  meta_raw <- fetch_raw("videometadata", env)
  if (!is.null(meta_raw)) {
    parsed_meta <- tryCatch(fromJSON(meta_raw, simplifyVector = TRUE), error = function(e) NULL)
    if (!is.null(parsed_meta)) {
      n <- if (is.data.frame(parsed_meta)) nrow(parsed_meta) else length(parsed_meta)
      message(glue(">> videometadata: {n} entries"))
      if (is.data.frame(parsed_meta)) {
        message(glue(">> videometadata columns: {paste(names(parsed_meta), collapse=', ')}"))
        # Show first 5 rows in full, plus a play-id-duplicate count check.
        print(utils::head(parsed_meta, 5))
        if ("playId" %in% names(parsed_meta)) {
          dup_counts <- table(parsed_meta$playId)
          message(glue(">> plays with >1 metadata row: {sum(dup_counts > 1)}"))
        }
      }
      message(">> FULL RAW videometadata JSON (first 20 entries):")
      if (is.data.frame(parsed_meta)) {
        print(toJSON(utils::head(parsed_meta, 20), pretty = TRUE, auto_unbox = TRUE))
      } else {
        print(toJSON(parsed_meta, pretty = TRUE, auto_unbox = TRUE))
      }
    }
  }
}
