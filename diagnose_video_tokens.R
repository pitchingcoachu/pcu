#!/usr/bin/env Rscript
# Read-only diagnostic: dump ALL videometadata rows for a given playId, to
# check for any surprises (e.g. multiple Edgertronic rows, duplicate
# videoClipIds, or unexpected column values) that could explain a mismatch
# between the raw source data and what our sync script matched.
#
# Usage: Rscript diagnose_video_tokens.R <session_id> <play_id>

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(glue)
})

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

args <- commandArgs(trailingOnly = TRUE)
session_id <- args[[1]]
play_id <- tolower(args[[2]])

client_id <- Sys.getenv("TM_CLIENT_ID")
client_secret <- Sys.getenv("TM_CLIENT_SECRET")
if (!nzchar(client_id) || !nzchar(client_secret)) stop("Missing TM_CLIENT_ID/TM_CLIENT_SECRET")

tok_res <- request("https://login.trackmanbaseball.com/connect/token") |>
  req_body_form(client_id = client_id, client_secret = client_secret, grant_type = "client_credentials") |>
  req_timeout(20) |>
  req_error(is_error = ~ FALSE) |>
  req_perform()
access_token <- resp_body_json(tok_res, simplifyVector = TRUE)$access_token

url <- glue("https://dataapi.trackmanbaseball.com/api/v1/media/practice/videometadata/{session_id}")
res <- request(url) |>
  req_headers(Authorization = paste("Bearer", access_token), accept = "text/plain") |>
  req_timeout(30) |>
  req_error(is_error = ~ FALSE) |>
  req_perform()
parsed <- fromJSON(resp_body_string(res), simplifyVector = TRUE)

sub <- parsed[tolower(parsed$playId) == play_id, , drop = FALSE]
message(glue(">> playId={play_id}: {nrow(sub)} metadata row(s)"))
print(toJSON(sub, pretty = TRUE, auto_unbox = TRUE))

message(">> Checking for duplicate videoClipIds across ENTIRE session:")
dup_clip_ids <- table(tolower(parsed$videoClipId))
dups <- dup_clip_ids[dup_clip_ids > 1]
message(glue(">> duplicate videoClipIds found: {length(dups)}"))
if (length(dups) > 0) print(dups[1:min(5, length(dups))])
