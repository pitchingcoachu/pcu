#!/usr/bin/env Rscript
# Read-only diagnostic: lists ALL blobs in the Edgertronic (and iPhone)
# Azure containers for one session, following NextMarker pagination fully,
# to check (a) how many total blobs exist per container and whether
# pagination is actually in play, and (b) whether a specific play's
# Edgertronic blob is present in the full listing.
#
# Usage: Rscript diagnose_video_tokens.R <session_id> <play_id_substring>

suppressPackageStartupMessages({
  library(httr2)
  library(xml2)
  library(glue)
})

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

args <- commandArgs(trailingOnly = TRUE)
session_id <- args[[1]]
needle <- tolower(args[[2]])

client_id <- Sys.getenv("TM_CLIENT_ID")
client_secret <- Sys.getenv("TM_CLIENT_SECRET")
if (!nzchar(client_id) || !nzchar(client_secret)) stop("Missing TM_CLIENT_ID/TM_CLIENT_SECRET")

tok_res <- request("https://login.trackmanbaseball.com/connect/token") |>
  req_body_form(client_id = client_id, client_secret = client_secret, grant_type = "client_credentials") |>
  req_timeout(20) |>
  req_error(is_error = ~ FALSE) |>
  req_perform()
access_token <- resp_body_json(tok_res, simplifyVector = TRUE)$access_token

parse_sas_query <- function(token) {
  q <- sub("^\\?", "", token)
  pairs <- strsplit(q, "&")[[1]]
  out <- list()
  for (p in pairs) {
    kv <- strsplit(p, "=")[[1]]
    if (length(kv) == 2) out[[URLdecode(kv[1])]] <- URLdecode(kv[2])
  }
  out
}

url <- glue("https://dataapi.trackmanbaseball.com/api/v1/media/practice/videotokens/{session_id}")
res <- request(url) |>
  req_headers(Authorization = paste("Bearer", access_token), accept = "text/plain") |>
  req_timeout(20) |>
  req_error(is_error = ~ FALSE) |>
  req_perform()
tokens <- jsonlite::fromJSON(resp_body_string(res), simplifyVector = TRUE)
message(glue(">> token types: {paste(tokens$type, collapse=', ')}"))

for (i in seq_len(nrow(tokens))) {
  entity_path <- tokens$entityPath[i]
  endpoint <- tokens$endpoint[i]
  sas_token <- tokens$token[i]
  type_name <- tokens$type[i]
  message(glue(">> Listing container for {type_name} ({entity_path}/{endpoint})"))

  params <- parse_sas_query(sas_token)
  base_url <- glue("https://{entity_path}.blob.core.windows.net/{endpoint}")
  marker <- NULL
  all_blobs <- character()
  page <- 0
  repeat {
    page <- page + 1
    q <- params
    if (!is.null(marker)) q$marker <- marker
    r <- request(base_url) |>
      req_url_query(restype = "container", comp = "list") |>
      req_url_query(!!!q) |>
      req_timeout(30) |>
      req_error(is_error = ~ FALSE) |>
      req_perform()
    if (resp_status(r) >= 400) {
      message(glue("   page {page}: HTTP {resp_status(r)} -- {resp_body_string(r)}"))
      break
    }
    xml <- read_xml(resp_body_string(r))
    blobs <- xml_find_all(xml, ".//Blob")
    names <- xml_text(xml_find_first(blobs, "Name"))
    all_blobs <- c(all_blobs, names)
    next_marker <- xml_text(xml_find_first(xml, ".//NextMarker"))
    message(glue("   page {page}: {length(names)} blobs, NextMarker={if (nzchar(next_marker)) next_marker else '(none)'}"))
    if (!nzchar(next_marker)) break
    marker <- next_marker
    if (page > 20) { message("   aborting after 20 pages"); break }
  }
  message(glue(">> {type_name}: {length(all_blobs)} total blobs across {page} page(s)"))
  matches <- all_blobs[grepl(needle, tolower(all_blobs), fixed = TRUE)]
  message(glue(">> blobs matching '{needle}': {length(matches)}"))
  if (length(matches)) print(matches)
}
