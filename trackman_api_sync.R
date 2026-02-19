#!/usr/bin/env Rscript
#
# TrackMan → Cloudinary automation
# --------------------------------
#  * Discovers recent practice/game sessions from the TrackMan Data API
#  * Pulls Azure SAS tokens for each session's media containers
#  * Uploads new blobs directly to Cloudinary (using unsigned preset)
#  * Persists a video mapping table under data/video_map.csv
#
# Usage:
#   Rscript trackman_api_sync.R            # defaults to today UTC
#   Rscript trackman_api_sync.R 2025-10-11 # specific date
#   Rscript trackman_api_sync.R 2025-10-01 2025-10-07
#
# Required environment variables:
#   TM_CLIENT_ID / TM_CLIENT_SECRET   -> TrackMan API credentials
#   CLOUDINARY_CLOUD_NAME             -> Cloudinary cloud name
#   CLOUDINARY_UPLOAD_PRESET          -> Unsigned preset that allows remote uploads
#
# Optional environment variables:
#   TM_ENV           -> "practice" (default) or "game" to scope discovery
#   CLOUDINARY_FOLDER -> Folder prefix when creating Cloudinary public IDs
#
suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(xml2)
  library(glue)
})

# ---- Helpers ---------------------------------------------------------------

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  hit <- cmd_args[grepl(file_arg, cmd_args, fixed = TRUE)]
  if (length(hit)) {
    normalizePath(sub(file_arg, "", hit[1]), winslash = "/", mustWork = FALSE)
  } else {
    NA_character_
  }
}

now_iso <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

strip_leading_q <- function(x) sub("^\\?", "", x %||% "")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

cli_abort <- function(msg) {
  message("ERROR: ", msg)
  quit(status = 1)
}

ensure_env <- function(name) {
  val <- Sys.getenv(name, unset = "")
  if (!nzchar(val)) cli_abort(glue("Environment variable '{name}' must be set."))
  val
}

parse_args_dates <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    lookback <- suppressWarnings(as.integer(Sys.getenv("TM_LOOKBACK_DAYS", unset = "1")))
    if (is.na(lookback) || lookback < 0) lookback <- 0L
    d_to <- Sys.Date()
    d_from <- d_to - lookback
    return(list(from = d_from, to = d_to))
  }
  parse_one <- function(x) {
    out <- suppressWarnings(as.Date(x))
    if (is.na(out)) cli_abort(glue("Invalid date supplied: '{x}'"))
    out
  }
  from <- parse_one(args[1])
  to   <- if (length(args) >= 2) parse_one(args[2]) else from
  if (to < from) cli_abort("End date cannot be before start date.")
  list(from = from, to = to)
}

format_utc <- function(d, end = FALSE) {
  if (inherits(d, "POSIXt")) {
    format(with_tz(d, tzone = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  } else {
    stamp <- if (end) "T23:59:59Z" else "T00:00:00Z"
    paste0(format(d, "%Y-%m-%d"), stamp)
  }
}

build_project_paths <- function() {
  script_path <- get_script_path()
  if (is.na(script_path)) {
    root <- normalizePath(".", winslash = "/", mustWork = TRUE)
  } else {
    root <- normalizePath(file.path(dirname(script_path), "."), winslash = "/", mustWork = TRUE)
  }
  list(
    root = root,
    data_dir = normalizePath(file.path(root, "data"), winslash = "/", mustWork = TRUE),
    video_map = file.path(root, "data", "video_map.csv")
  )
}

request_trackman_token <- function(client_id, client_secret) {
  res <- request("https://login.trackmanbaseball.com/connect/token") |>
    req_body_form(
      client_id = client_id,
      client_secret = client_secret,
      grant_type = "client_credentials"
    ) |>
    req_timeout(20) |>
    req_error(is_error = ~ FALSE) |>
    req_perform()

  if (resp_status(res) >= 400) {
    detail <- tryCatch(resp_body_string(res), error = function(e) "")
    cli_abort(glue("TrackMan token request failed ({resp_status(res)}): {detail}"))
  }

  body <- resp_body_json(res, simplifyVector = TRUE)
  token <- body$access_token %||% ""
  if (!nzchar(token)) cli_abort("TrackMan response did not contain an access_token.")
  token
}

trackman_request <- function(token, url, method = "GET", body = NULL, retries = 2) {
  req <- request(url) |>
    req_method(method) |>
    req_headers(
      Authorization = paste("Bearer", token),
      accept = "text/plain"
    ) |>
    req_timeout(15) |>  # Reduced timeout to fail faster
    req_error(is_error = ~ FALSE)

  if (!is.null(body)) {
    req <- req_body_json(req, body, mode = "json", digits = NA)
    req <- req_headers(req, `Content-Type` = "application/json-patch+json")
  }

  # Retry logic for network issues
  for (attempt in 1:(retries + 1)) {
    res <- tryCatch({
      req_perform(req)
    }, error = function(e) {
      if (grepl("timeout|timed out", e$message, ignore.case = TRUE)) {
        message(glue("   Attempt {attempt}: Request timed out for {url}"))
        if (attempt <= retries) {
          message(glue("   Retrying in {attempt} seconds..."))
          Sys.sleep(attempt)
          return(NULL)  # Signal to retry
        } else {
          message("   All retry attempts exhausted")
          stop(e)
        }
      } else {
        stop(e)  # Re-throw non-timeout errors immediately
      }
    })
    
    # If we got a response, process it
    if (!is.null(res)) {
      if (resp_status(res) >= 400) {
        detail <- tryCatch(resp_body_string(res), error = function(e) "")
        # Don't abort on 404 errors - these are common when sessions don't exist
        if (resp_status(res) == 404) {
          warning(glue("TrackMan API call failed ({resp_status(res)}): {detail}"))
          return(NULL)
        } else {
          cli_abort(glue("TrackMan API call failed ({resp_status(res)}): {detail}"))
        }
      }
      return(res)
    }
  }
  
  # If we get here, all retries failed
  cli_abort(glue("TrackMan API request failed after {retries + 1} attempts due to timeouts"))
}

discover_sessions <- function(token, date_from, date_to, env = c("practice", "game")) {
  env <- match.arg(env)
  url <- switch(env,
                practice = "https://dataapi.trackmanbaseball.com/api/v1/discovery/practice/sessions",
                game     = "https://dataapi.trackmanbaseball.com/api/v1/discovery/game/sessions")
  body <- list(
    sessionType = "All",
    utcDateFrom = format_utc(date_from, end = FALSE),
    utcDateTo   = format_utc(date_to, end = TRUE)
  )
  
  message(glue("   Discovering sessions from {format_utc(date_from, end = FALSE)} to {format_utc(date_to, end = TRUE)}"))
  
  res <- tryCatch({
    trackman_request(token, url, method = "POST", body = body)
  }, error = function(e) {
    if (grepl("timeout|timed out", e$message, ignore.case = TRUE)) {
      warning(glue("Session discovery timed out - this may happen when no sessions exist for the date range"))
      return(NULL)
    } else {
      stop(e)
    }
  })
  
  if (is.null(res)) {
    message("   No sessions found or request timed out")
    return(tibble())
  }
  
  out <- tryCatch({
    resp_body_json(res, simplifyVector = TRUE)
  }, error = function(e) {
    message("   Warning: Could not parse session discovery response")
    return(list())
  })
  
  if (is.null(out) || length(out) == 0) {
    message("   No sessions returned from API")
    return(tibble())
  }
  
  result <- tibble::as_tibble(out)
  message(glue("   Found {nrow(result)} session(s)"))
  result
}

fetch_video_tokens <- function(token, session_id, env = c("practice", "game")) {
  env <- match.arg(env)
  url <- switch(env,
                practice = glue("https://dataapi.trackmanbaseball.com/api/v1/media/practice/videotokens/{session_id}"),
                game     = glue("https://dataapi.trackmanbaseball.com/api/v1/media/game/videotokens/{session_id}"))
  res <- trackman_request(token, url)
  if (is.null(res)) return(tibble())
  tibble::as_tibble(resp_body_json(res, simplifyVector = TRUE))
}

fetch_video_metadata <- function(token, session_id, env = c("practice", "game")) {
  env <- match.arg(env)
  url <- switch(env,
                practice = glue("https://dataapi.trackmanbaseball.com/api/v1/media/practice/videometadata/{session_id}"),
                game     = glue("https://dataapi.trackmanbaseball.com/api/v1/media/game/videometadata/{session_id}"))
  res <- trackman_request(token, url)
  if (is.null(res)) return(tibble())
  tibble::as_tibble(resp_body_json(res, simplifyVector = TRUE))
}

parse_sas_query <- function(token) {
  qs <- strip_leading_q(token)
  if (!nzchar(qs)) return(list())
  parts <- strsplit(qs, "&", fixed = TRUE)[[1]]
  kv <- strsplit(parts, "=", fixed = TRUE)
  out <- lapply(kv, function(x) {
    key <- URLdecode(x[1])
    val <- if (length(x) > 1) URLdecode(x[2]) else ""
    setNames(list(val), key)
  })
  do.call(c, out)
}

list_azure_blobs <- function(entity_path, endpoint, token) {
  base_url <- glue("https://{entity_path}.blob.core.windows.net/{endpoint}")
  params <- parse_sas_query(token)
  res <- request(base_url) |>
    req_url_query(restype = "container", comp = "list") |>
    req_url_query(!!!params) |>
    req_timeout(30) |>
    req_error(is_error = ~ FALSE) |>
    req_perform()
  if (resp_status(res) >= 400) {
    detail <- tryCatch(resp_body_string(res), error = function(e) "")
    cli_abort(glue("Azure list failed ({resp_status(res)}): {detail}"))
  }
  xml <- read_xml(resp_body_string(res))
  blobs <- xml_find_all(xml, ".//Blob")
  tibble(
    blob_name = xml_text(xml_find_first(blobs, "Name")),
    content_length = as.numeric(xml_text(xml_find_first(blobs, "Properties/Content-Length"))),
    content_md5 = xml_text(xml_find_first(blobs, "Properties/Content-MD5")),
    last_modified = xml_text(xml_find_first(blobs, "Properties/Last-Modified"))
  )
}

extract_play_id <- function(blob_name) {
  # Expecting Plays/<PLAY_ID>/...
  m <- str_match(blob_name, "^Plays/([0-9a-fA-F-]+)/")
  m[, 2] %||% NA_character_
}

infer_camera_slot <- function(camera_name, camera_target, video_type, blob_name) {
  fields <- tolower(paste(
    camera_name %||% "",
    camera_target %||% "",
    video_type %||% "",
    blob_name %||% ""
  ))
  pick_by_target <- function(target) {
    if (!nzchar(target)) return(NA_character_)
    t <- tolower(target)
    if (str_detect(t, "back")) return("VideoClip")
    if (str_detect(t, "1b") || str_detect(t, "first") || str_detect(t, "home") || str_detect(t, "center")) return("VideoClip2")
    if (str_detect(t, "3b") || str_detect(t, "third") || str_detect(t, "side")) return("VideoClip3")
    NA_character_
  }
  candidate <- pick_by_target(camera_target)
  if (!is.na(candidate)) return(candidate)
  if (str_detect(fields, "edger")) return("VideoClip")
  if (str_detect(fields, "1b") || str_detect(fields, "first") || str_detect(fields, "behind") || str_detect(fields, "center")) return("VideoClip2")
  if (str_detect(fields, "3b") || str_detect(fields, "third") || str_detect(fields, "side")) return("VideoClip3")
  "VideoClip"
}

build_blob_url <- function(entity_path, endpoint, blob_name, token) {
  encoded <- URLencode(blob_name, reserved = TRUE)
  glue("https://{entity_path}.blob.core.windows.net/{endpoint}/{encoded}{token}")
}

upload_cloudinary <- function(azure_url, preset, cloud_name, public_id = NULL) {
  endpoint <- glue("https://api.cloudinary.com/v1_1/{cloud_name}/auto/upload")
  body <- list(
    upload_preset = preset,
    file = azure_url,
    resource_type = "video"
  )
  if (!is.null(public_id) && nzchar(public_id)) {
    body$public_id <- public_id
  }
  res <- request(endpoint) |>
    req_body_multipart(!!!body) |>
    req_timeout(60) |>
    req_error(is_error = ~ FALSE) |>
    req_perform()
  if (resp_status(res) >= 400) {
    detail <- tryCatch(resp_body_string(res), error = function(e) "")
    cli_abort(glue("Cloudinary upload failed ({resp_status(res)}): {detail}"))
  }
  resp_body_json(res, simplifyVector = TRUE)
}

sanitize_public_id <- function(...) {
  parts <- c(...)
  parts <- parts[nzchar(parts)]
  id <- paste(parts, collapse = "/")
  id <- str_replace_all(id, "[^A-Za-z0-9_\\-\\/]", "_")
  substr(id, 1, 255)
}

load_video_map <- function(path) {
  if (!file.exists(path)) {
    tibble(
      session_id = character(),
      play_id = character(),
      camera_slot = character(),
      camera_name = character(),
      camera_target = character(),
      video_type = character(),
      azure_blob = character(),
      azure_md5 = character(),
      cloudinary_url = character(),
      cloudinary_public_id = character(),
      uploaded_at = character()
    )
  } else {
    df <- suppressMessages(read_csv(path, show_col_types = FALSE))
    # Force video map columns to character so bind_rows() never sees mixed types.
    str_cols <- c("session_id", "play_id", "camera_slot", "camera_name",
                  "camera_target", "video_type", "azure_blob", "azure_md5",
                  "cloudinary_url", "cloudinary_public_id", "uploaded_at")
    existing <- intersect(names(df), str_cols)
    if (length(existing)) {
      df <- df %>% mutate(across(all_of(existing), as.character))
    }
    # Ensure uploaded_at stays character for consistency.
    if ("uploaded_at" %in% names(df)) {
      df$uploaded_at <- as.character(df$uploaded_at)
    }
    df
  }
}

append_video_map <- function(df, path) {
  df <- distinct(df)
  write_csv(df, path)
}

# ---- Main ------------------------------------------------------------------

main <- function() {
  dates <- parse_args_dates()
  paths <- build_project_paths()

  tm_client_id <- ensure_env("TM_CLIENT_ID")
  tm_client_secret <- ensure_env("TM_CLIENT_SECRET")
  cloud_name <- ensure_env("CLOUDINARY_CLOUD_NAME")
  preset <- ensure_env("CLOUDINARY_UPLOAD_PRESET")
  folder <- Sys.getenv("CLOUDINARY_FOLDER", unset = "trackman")
  tm_env <- tolower(Sys.getenv("TM_ENV", unset = "practice"))
  if (!nzchar(tm_env)) tm_env <- "practice"
  tm_env <- match.arg(tm_env, c("practice","game"))

  # Cap date window to 10 days to avoid TrackMan range errors
  if (as.numeric(difftime(dates$to, dates$from, units = "days")) > 9) {
    dates$from <- dates$to - 9
  }

  message(glue(">> Syncing TrackMan {tm_env} sessions from {dates$from} to {dates$to}"))
  token <- request_trackman_token(tm_client_id, tm_client_secret)

  sessions <- tryCatch({
    discover_sessions(token, dates$from, dates$to, env = tm_env)
  }, error = function(e) {
    if (grepl("timeout|timed out", e$message, ignore.case = TRUE)) {
      message("Session discovery timed out - likely no sessions exist for this date range")
      message("This commonly happens when no camera was used during practice sessions")
      return(tibble())
    } else {
      message(glue("Failed to discover sessions: {e$message}"))
      return(tibble())
    }
  })
  
  if (!nrow(sessions)) {
    message("No sessions discovered in the requested window.")
    message("This is normal when no camera/video was used during practice sessions.")
    quit(status = 0)
  }

  video_map <- load_video_map(paths$video_map)
  new_rows <- list()

  for (i in seq_len(nrow(sessions))) {
    session_id_local <- sessions$sessionId[[i]]
    message(glue("-- Session {i}/{nrow(sessions)}: {session_id_local} ({sessions$sessionType[[i]] %||% \"\"})"))

    meta <- tryCatch(fetch_video_metadata(token, session_id_local, env = tm_env), error = function(e) tibble())
    tokens <- tryCatch(fetch_video_tokens(token, session_id_local, env = tm_env), error = function(e) tibble())

    if (!nrow(tokens)) {
      message("   No media tokens for this session, skipping.")
      next
    }

    if (!nrow(meta)) {
      meta <- tibble(playId = character(), cameraName = character(), cameraTarget = character())
    } else {
      if (!"cameraName" %in% names(meta))  meta$cameraName  <- NA_character_
      if (!"cameraTarget" %in% names(meta)) meta$cameraTarget <- NA_character_
      if (!"playId" %in% names(meta)) meta$playId <- NA_character_
    }

    meta_by_play <- meta %>%
      mutate(playId = tolower(playId %||% "")) %>%
      filter(nzchar(playId))

    for (j in seq_len(nrow(tokens))) {
      tok <- tokens[j, ]
      entity_path <- tok$entityPath %||% ""
      endpoint <- tok$endpoint %||% ""
      sas_token <- tok$token %||% ""
      video_type <- tok$type %||% ""
      blobs <- tryCatch(
        list_azure_blobs(entity_path, endpoint, sas_token),
        error = function(e) {
          message(glue("   Failed to list blobs for entity {entity_path}: {e$message}"))
          tibble()
        }
      )
      if (!nrow(blobs)) next

      for (k in seq_len(nrow(blobs))) {
        blob <- blobs[k, ]
        play_id <- tolower(extract_play_id(blob$blob_name))
        if (!nzchar(play_id)) next

        md <- meta_by_play %>% filter(playId == play_id) %>% slice_head(n = 1)
        camera_name <- md$cameraName %||% video_type %||% ""
        camera_target <- md$cameraTarget %||% ""

        slot <- infer_camera_slot(camera_name, camera_target, video_type, blob$blob_name)
        already <- video_map %>%
          filter(
            tolower(session_id) == tolower(session_id_local),
            tolower(play_id) == play_id,
            camera_slot == slot,
            (azure_md5 == blob$content_md5) | (azure_blob == blob$blob_name)
          )
        if (nrow(already)) next

        azure_url <- build_blob_url(entity_path, endpoint, blob$blob_name, sas_token)
        public_id <- sanitize_public_id(folder, session_id_local, slot, substr(play_id, 1, 12))
        message(glue("   Uploading play {play_id} [{slot}] ({camera_name})"))
        upload <- tryCatch(
          upload_cloudinary(azure_url, preset, cloud_name, public_id = public_id),
          error = function(e) {
            message(glue("     Upload failed: {e$message}"))
            NULL
          }
        )
        if (is.null(upload)) next

        new_rows[[length(new_rows) + 1]] <- tibble(
          session_id = session_id_local,
          play_id = play_id,
          camera_slot = slot,
          camera_name = camera_name %||% "",
          camera_target = camera_target %||% "",
          video_type = video_type,
          azure_blob = blob$blob_name,
          azure_md5 = blob$content_md5 %||% "",
          cloudinary_url = upload$secure_url %||% upload$url %||% "",
          cloudinary_public_id = upload$public_id %||% public_id,
          uploaded_at = now_iso()
        )
      }
    }
  }

  if (length(new_rows)) {
    additions <- bind_rows(new_rows)
    video_map <- bind_rows(video_map, additions) %>% distinct()
    dir.create(dirname(paths$video_map), recursive = TRUE, showWarnings = FALSE)
    append_video_map(video_map, paths$video_map)
    message(glue(">> Added {nrow(additions)} new video mapping rows."))
  } else {
    message(">> No new videos uploaded.")
  }
}

main()
