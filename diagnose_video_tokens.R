#!/usr/bin/env Rscript
# Read-only diagnostic: look up specific videoClipIds in the raw
# videometadata response to check whether TrackMan itself mislabels
# Edgertronic clips as cameraType/cameraName "iPhone", or whether that
# mislabel is introduced by our own matching logic.
#
# Usage: Rscript diagnose_video_tokens.R <session_id> <clip_id_1> [clip_id_2 ...]

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(glue)
})

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

args <- commandArgs(trailingOnly = TRUE)
session_id <- args[[1]]
clip_ids <- tolower(args[-1])

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

for (cid in clip_ids) {
  sub <- parsed[tolower(parsed$videoClipId) == cid, , drop = FALSE]
  message(glue(">> videoClipId={cid}: {nrow(sub)} row(s)"))
  if (nrow(sub)) print(toJSON(sub, pretty = TRUE, auto_unbox = TRUE))
}
