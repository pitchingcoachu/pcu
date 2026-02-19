#!/usr/bin/env Rscript
# Upload a batch of camera-2 files to Cloudinary and map them via TrackMan IDs.

suppressPackageStartupMessages({
  library(optparse)
  library(httr2)
  library(glue)
  library(readr)
  library(dplyr)
  library(curl)
  library(rlang)
  library(purrr)
  library(tibble)
})

source("video_map_helpers.R")

option_list <- list(
  make_option(c("-s", "--session-id"), dest = "session_id", type = "character", metavar = "SESSION",
              help = "TrackMan session id that the uploaded clips belong to (required)"),
  make_option(c("-d", "--clips-dir"), dest = "clips_dir", type = "character", metavar = "DIR",
              help = "Directory containing ordered camera-2 clips (required)"),
  make_option(c("--camera-slot"), dest = "camera_slot", type = "character", default = "VideoClip2",
              help = "camera slot to map (default: %default)"),
  make_option(c("--camera-name"), dest = "camera_name", type = "character", default = "ManualCamera",
              help = "Friendly camera name to store in video_map (default: %default)"),
  make_option(c("--camera-target"), dest = "camera_target", type = "character", default = "ManualUpload",
              help = "Camera target description (default: %default)"),
  make_option(c("--video-type"), dest = "video_type", type = "character", default = "ManualVideo",
              help = "Video type field for video_map (default: %default)"),
  make_option(c("--pattern"), dest = "pattern", type = "character", default = "(?i)\\\\.(mp4|mov|avi)$",
              help = "Regex for clip filenames (default: %default)"),
  make_option(c("--order-by"), dest = "order_by", type = "character", default = "name",
              help = "Ordering for uploads: 'name' or 'mtime' (default: %default)"),
  make_option(c("--cloudinary-folder"), dest = "cloudinary_folder", type = "character", default = "trackman",
              help = "Cloudinary folder prefix (default: %default)"),
  make_option(c("--manifest-out"), dest = "manifest_out", type = "character", default = "",
              help = "Where to write the generated manifest (optional)"),
  make_option(c("--data-dir"), dest = "data_dir", type = "character", default = "data",
              help = "Root directory containing the TrackMan CSVs (default: %default)"),
  make_option(c("--map-path"), dest = "map_path", type = "character", default = "data/video_map.csv",
              help = "Path to the video_map.csv file (default: %default)")
)

opt <- parse_args(OptionParser(option_list = option_list))

required_value <- function(val, name) {
  if (is.null(val) || !nzchar(val)) {
    stop(glue("The {name} option is required."), call. = FALSE)
  }
  val
}

session_id <- required_value(opt$session_id, "--session-id")
clips_dir <- required_value(opt$clips_dir, "--clips-dir")

if (!dir.exists(clips_dir)) stop(glue("Clips directory not found: {clips_dir}"))

cloud_name <- Sys.getenv("CLOUDINARY_CLOUD_NAME", unset = "")
upload_preset <- Sys.getenv("CLOUDINARY_UPLOAD_PRESET", unset = "")
if (!nzchar(cloud_name) || !nzchar(upload_preset)) {
  stop("Cloudinary config missing: set CLOUDINARY_CLOUD_NAME and CLOUDINARY_UPLOAD_PRESET", call. = FALSE)
}

list_clips <- function(path, pattern, order_by) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  if (!length(files)) return(character(0))
  info <- file.info(files)
  ordered <- switch(
    order_by,
    mtime = files[order(info$mtime)],
    name = files[order(tolower(basename(files)))],
    stop("--order-by must be 'name' or 'mtime'")
  )
  ordered
}

clips <- list_clips(clips_dir, opt$pattern, opt$order_by)
if (!length(clips)) stop(glue("No clips matched pattern '{opt$pattern}' in {clips_dir}"))

cloudinary_upload <- function(path, folder = NULL) {
  endpoint <- sprintf("https://api.cloudinary.com/v1_1/%s/auto/upload", cloud_name)
  fields <- list2(
    file = curl::form_file(path),
    upload_preset = upload_preset,
    !!!if (nzchar(folder)) list(folder = folder) else list()
  )

  res <- request(endpoint) |>
    req_body_multipart(!!!fields) |>
    req_timeout(90) |>
    req_error(is_error = ~ FALSE) |>
    req_perform()

  if (resp_status(res) >= 400) {
    detail <- tryCatch(resp_body_string(res), error = function(e) e$message)
    stop(glue("Cloudinary upload failed ({resp_status(res)}): {detail}"), call. = FALSE)
  }

  body <- resp_body_json(res, check_type = FALSE)
  list(
    url = body$secure_url %||% body$url %||% "",
    public_id = body$public_id %||% ""
  )
}

folder_components <- c(opt$cloudinary_folder, session_id, opt$camera_slot)
folder <- paste(folder_components[nzchar(folder_components)], collapse = "/")

records <- purrr::map_dfr(clips, function(path) {
  message(glue("Uploading {basename(path)}..."))
  info <- cloudinary_upload(path, folder)
  tibble::tibble(
    file_name = basename(path),
    cloudinary_url = info$url,
    cloudinary_public_id = info$public_id
  )
})

if (nzchar(opt$manifest_out)) {
  manifest_dir <- dirname(opt$manifest_out)
  if (!dir.exists(manifest_dir)) dir.create(manifest_dir, recursive = TRUE)
  readr::write_csv(records, path.expand(opt$manifest_out))
  message(glue("Manifest saved to {opt$manifest_out}"), appendLF = TRUE)
}

manifest <- records %>%
  dplyr::select(cloudinary_url, cloudinary_public_id)

session_rows <- find_session_rows(opt$data_dir, session_id)
if (!nrow(session_rows)) stop(glue("Unable to find session '{session_id}' in {opt$data_dir}"), call. = FALSE)
session_rows <- order_session_rows(session_rows)

map_manifest_to_session(
  session_rows = session_rows,
  manifest = manifest,
  session_id = session_id,
  slot = opt$camera_slot,
  name = opt$camera_name,
  target = opt$camera_target,
  type = opt$video_type,
  map_path = opt$map_path
)
