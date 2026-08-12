#!/usr/bin/env Rscript
# Read-only diagnostic: dumps distinct cameraTarget/cameraType/cameraName
# value combinations from the RAW TrackMan videometadata response for one
# session_id, to check real-world camera_target vocabulary against our
# keyword-matching logic. Does not write anything.
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
raw_text <- resp_body_string(res)
parsed <- fromJSON(raw_text, simplifyVector = TRUE)

message(glue(">> total rows: {nrow(parsed)}"))
message(">> cameraType distribution:")
print(table(parsed$cameraType, useNA = "ifany"))
message(">> cameraTarget distribution (all rows):")
print(table(parsed$cameraTarget, useNA = "ifany"))
message(">> cameraTarget distribution (cameraType == iPhone only):")
iphone_rows <- parsed[tolower(parsed$cameraType %||% "") == "iphone", ]
print(table(iphone_rows$cameraTarget, useNA = "ifany"))

message(">> plays with exactly 2 iPhone-cameraType rows -- cameraTarget pairs:")
iphone_by_play <- split(iphone_rows$cameraTarget, iphone_rows$playId)
two_iphone_plays <- iphone_by_play[lengths(iphone_by_play) == 2]
message(glue(">> count of such plays: {length(two_iphone_plays)}"))
pairs <- sapply(two_iphone_plays, function(x) paste(sort(x), collapse=" | "))
print(table(pairs))
