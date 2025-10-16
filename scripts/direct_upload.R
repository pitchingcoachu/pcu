#!/usr/bin/env Rscrip  # Get environment variables
  cloud_name <- ensure_env("CLOUDINARY_CLOUD_NAME")
  preset <- ensure_env("CLOUDINARY_UPLOAD_PRESET")
  video_map_path <- "data/video_map_manual.csv"  # Separate file for manual uploads# Direct Video Upload Script
# --------------------------
# Upload videos directly to Cloudinary without going through GitHub
# Useful for large video files or bulk uploads

suppressPackageStartupMessages({
  library(httr2)
  library(readr)
  library(dplyr)
  library(glue)
  library(stringr)
})

source("scripts/process_videos.R")

# Upload videos from a local directory directly
upload_local_videos <- function(local_video_dir) {
  cat(glue("📱 Direct upload from: {local_video_dir}\n\n"))
  
  # Get environment variables
  cloud_name <- ensure_env("CLOUDINARY_CLOUD_NAME")
  preset <- ensure_env("CLOUDINARY_UPLOAD_PRESET")
  video_map_path <- "data/video_map.csv"
  
  if (!dir.exists(local_video_dir)) {
    stop(glue("Directory not found: {local_video_dir}"))
  }
  
  # Find session folders
  session_folders <- list.dirs(local_video_dir, recursive = FALSE)
  
  for (session_folder in session_folders) {
    folder_name <- basename(session_folder)
    cat(glue("📁 Processing: {folder_name}\n"))
    
    # Find VideoClip2 and VideoClip3 subfolders
    camera_folders <- list.dirs(session_folder, recursive = FALSE)
    camera_folders <- camera_folders[basename(camera_folders) %in% c("VideoClip2", "VideoClip3")]
    
    for (camera_folder in camera_folders) {
      camera_slot <- basename(camera_folder)
      
      # Find video files
      video_files <- list.files(
        camera_folder, 
        pattern = "\\.(mp4|mov|avi|mkv|webm)$", 
        ignore.case = TRUE,
        full.names = TRUE
      )
      
      for (video_file in video_files) {
        tryCatch({
          parsed <- parse_video_filename(basename(video_file), folder_name)
          if (is.null(parsed)) next
          
          cat(glue("  🎥 Uploading: {basename(video_file)}\n"))
          
          # Upload to Cloudinary
          cloudinary_result <- upload_to_cloudinary(
            video_file, 
            parsed$session_id, 
            parsed$play_id, 
            camera_slot, 
            cloud_name, 
            preset
          )
          
          # Update video map and CSV
          update_video_map(
            parsed$session_id, 
            parsed$play_id, 
            camera_slot, 
            cloudinary_result, 
            video_map_path
          )
          
          if (!is.null(parsed$csv_file)) {
            update_practice_csv(
              parsed$csv_file,
              parsed$session_id,
              parsed$play_id,
              camera_slot,
              cloudinary_result$secure_url
            )
          }
          
          cat(glue("  ✅ Success!\n\n"))
          
        }, error = function(e) {
          cat(glue("  ❌ Failed: {e$message}\n\n"))
        })
      }
    }
  }
}

# Run with: Rscript scripts/direct_upload.R /path/to/your/videos
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 0) {
    upload_local_videos(args[1])
  } else {
    cat("Usage: Rscript scripts/direct_upload.R /path/to/video/folder\n")
  }
}