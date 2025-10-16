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
      
      # Parse and sort videos first
      video_info <- list()
      for (video_file in video_files) {
        parsed <- parse_video_filename(basename(video_file), folder_name)
        if (!is.null(parsed)) {
          parsed$full_path <- video_file
          video_info[[length(video_info) + 1]] <- parsed
        }
      }
      
      # Sort by iPhone number (lowest to highest)
      if (length(video_info) > 0) {
        video_info <- video_info[order(sapply(video_info, function(x) x$sort_number))]
        
        # Process videos in sequence
        for (i in seq_along(video_info)) {
          info <- video_info[[i]]
          tryCatch({
            cat(glue("  🎥 Uploading: {info$filename} (sequence {i})\n"))
            
            # Upload to Cloudinary
            cloudinary_result <- upload_to_cloudinary(
              info$full_path, 
              info$session_id, 
              info$play_id, 
              camera_slot, 
              cloud_name, 
              preset
            )
            
            # Update video map and CSV
            update_video_map(
              info$session_id, 
              info$play_id, 
              camera_slot, 
              cloudinary_result, 
              video_map_path
            )
            
            if (!is.null(info$csv_file)) {
              update_practice_csv(
                info$csv_file,
                info$session_id,
                info$play_id,
                camera_slot,
                cloudinary_result$secure_url,
                i  # Pass sequence number
              )
            }
            
            cat(glue("  ✅ Success! Video {i} → CSV row {i}\n\n"))
          
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
