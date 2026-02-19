#!/usr/bin/env Rscript
# Map a manually uploaded Cloudinary manifest to TrackMan play IDs sequentially.
# Usage: Rscript map_manual_video_uploads.R --session-id <id> --manifest uploads.csv

suppressPackageStartupMessages({
  library(optparse)
  library(readr)
  library(dplyr)
  library(lubridate)
  library(glue)
})

source("video_map_helpers.R")

option_list <- list(
  make_option(c("-s", "--session-id"), dest = "session_id", type = "character", metavar = "SESSION",
              help = "TrackMan session_id to match the uploaded clips to (required)"),
  make_option(c("-m", "--manifest"), dest = "manifest", type = "character", metavar = "FILE",
              help = "CSV manifest listing the clips in upload order (required)"),
  make_option(c("--camera-slot"), dest = "camera_slot", type = "character", default = "VideoClip2",
              help = "Cloudinary slot to assign (default: %default)"),
  make_option(c("--camera-name"), dest = "camera_name", type = "character", default = "ManualCamera",
              help = "Friendly name for the camera feed (default: %default)"),
  make_option(c("--camera-target"), dest = "camera_target", type = "character", default = "ManualUpload",
              help = "Short description for the camera target (default: %default)"),
  make_option(c("--video-type"), dest = "video_type", type = "character", default = "ManualVideo",
              help = "Video type metadata that ends up in video_map.csv"),
  make_option(c("--data-dir"), dest = "data_dir", type = "character", default = "data",
              help = "Root directory containing the TrackMan CSVs (default: %default)"),
  make_option(c("--map-path"), dest = "map_path", type = "character", default = "data/video_map.csv",
              help = "Path to the video_map.csv file to append (default: %default)")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (is.null(opt$session_id) || !nzchar(opt$session_id)) {
  stop("The --session-id option is required.", call. = FALSE)
}
if (is.null(opt$manifest) || !nzchar(opt$manifest)) {
  stop("The --manifest option is required.", call. = FALSE)
}

manifest <- load_manifest(opt$manifest)
session_rows <- find_session_rows(opt$data_dir, opt$session_id)
if (!nrow(session_rows)) {
  stop(glue("Unable to find session '{opt$session_id}' inside the CSV data directory."), call. = FALSE)
}
if (!"PlayID" %in% names(session_rows)) {
  stop("TrackMan CSV rows for this session do not contain a PlayID column.", call. = FALSE)
}
session_rows <- order_session_rows(session_rows)
map_manifest_to_session(
  session_rows = session_rows,
  manifest = manifest,
  session_id = opt$session_id,
  slot = opt$camera_slot,
  name = opt$camera_name,
  target = opt$camera_target,
  type = opt$video_type,
  map_path = opt$map_path
)
