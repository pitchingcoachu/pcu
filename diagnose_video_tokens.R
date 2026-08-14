#!/usr/bin/env Rscript
# Read-only diagnostic: dump ALL videometadata rows for specific playIds, to
# check whether TrackMan's API genuinely has all 3 camera clips available at
# the source, even though our video_map_pcu table is only showing 2 for
# these plays.
#
# Usage: Rscript diagnose_video_tokens.R <session_id> <play_id_1> [play_id_2 ...]

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(glue)
})

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

args <- commandArgs(trailingOnly = TRUE)
session_id <- args[[1]]
play_ids <- tolower(args[-1])

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
message(glue(">> videometadata status: {resp_status(res)}"))
parsed <- fromJSON(resp_body_string(res), simplifyVector = TRUE)
message(glue(">> total rows: {nrow(parsed)}"))

for (pid in play_ids) {
  sub <- parsed[tolower(parsed$playId) == pid, , drop = FALSE]
  message(glue(">> playId={pid}: {nrow(sub)} metadata row(s)"))
  print(toJSON(sub, pretty = TRUE, auto_unbox = TRUE))
}

# Also dump raw videotokens to confirm both containers exist for this session
tokens_url <- glue("https://dataapi.trackmanbaseball.com/api/v1/media/practice/videotokens/{session_id}")
tokens_res <- request(tokens_url) |>
  req_headers(Authorization = paste("Bearer", access_token), accept = "text/plain") |>
  req_timeout(20) |>
  req_error(is_error = ~ FALSE) |>
  req_perform()
message(glue(">> videotokens status: {resp_status(tokens_res)}"))
tokens_parsed <- tryCatch(fromJSON(resp_body_string(tokens_res), simplifyVector = TRUE), error = function(e) NULL)
if (!is.null(tokens_parsed) && is.data.frame(tokens_parsed)) {
  message(glue(">> videotokens types: {paste(tokens_parsed$type, collapse=', ')}"))
}
