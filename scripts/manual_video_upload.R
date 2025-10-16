#!/usr/bin/env Rscript
#
# Manual Video Upload Script
# --------------------------
# Usage: Rscript scripts/manual_video_upload.R <session_id> <play_id> <video_path> [camera_slot]
#
# Example:
#   Rscript scripts/manual_video_upload.R "d527cb2a-39c9-47b4-a25d-b58bc4772f10" "custom-play-1" "~/phone_video.mp4" "VideoClip2"

suppressPackageStartupMessages({
  library(httr2)
  library(readr)
  library(dplyr)
  library(glue)
})

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 3) {
  stop("Usage: Rscript manual_video_upload.R <session_id> <play_id> <video_path> [camera_slot]")
}

session_id <- args[1]
play_id <- args[2]
video_path <- args[3]
camera_slot <- if (length(args) >= 4) args[4] else "VideoClip2"

# Check if video file exists
if (!file.exists(video_path)) {
  stop(glue("Video file not found: {video_path}"))
}

# Get environment variables
cloud_name <- Sys.getenv("CLOUDINARY_CLOUD_NAME")
preset <- Sys.getenv("CLOUDINARY_UPLOAD_PRESET")

if (!nzchar(cloud_name) || !nzchar(preset)) {
  stop("Please set CLOUDINARY_CLOUD_NAME and CLOUDINARY_UPLOAD_PRESET environment variables")
}

# Upload to Cloudinary
cat(glue("Uploading {video_path} to Cloudinary...\n"))

public_id <- glue("trackman/{session_id}/{camera_slot}/{substr(tolower(play_id), 1, 12)}")
upload_url <- glue("https://api.cloudinary.com/v1_1/{cloud_name}/video/upload")

response <- request(upload_url) |>
  req_body_multipart(
    upload_preset = preset,
    file = curl::form_file(video_path),
    public_id = public_id,
    resource_type = "video"
  ) |>
  req_timeout(120) |>
  req_perform()

if (resp_status(response) >= 400) {
  stop(glue("Upload failed: {resp_body_string(response)}"))
}

upload_result <- resp_body_json(response)
cat(glue("✅ Upload successful: {upload_result$secure_url}\n"))

# Update video_map.csv
video_map_path <- "data/video_map.csv"

if (file.exists(video_map_path)) {
  video_map <- read_csv(video_map_path, show_col_types = FALSE)
  # Ensure uploaded_at is character
  if ("uploaded_at" %in% names(video_map)) {
    video_map$uploaded_at <- as.character(video_map$uploaded_at)
  }
} else {
  dir.create("data", showWarnings = FALSE, recursive = TRUE)
  video_map <- tibble()
}

new_row <- tibble(
  session_id = session_id,
  play_id = tolower(play_id),
  camera_slot = camera_slot,
  camera_name = "Phone",
  camera_target = "",
  video_type = "Manual",
  azure_blob = "",
  azure_md5 = "",
  cloudinary_url = upload_result$secure_url,
  cloudinary_public_id = upload_result$public_id,
  uploaded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
)

video_map <- bind_rows(video_map, new_row)
write_csv(video_map, video_map_path)

cat(glue("✅ Updated {video_map_path}\n"))
cat(glue("Session: {session_id}\n"))
cat(glue("Play: {play_id}\n"))
cat(glue("Camera: {camera_slot}\n"))