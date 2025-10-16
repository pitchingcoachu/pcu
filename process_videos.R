#!/usr/bin/env Rscript
#
# Video Processing Script
# -----------------------
# Processes videos from video_processing/ folder structure:
# video_processing/videos_YYYY-MM-DD/VideoClip2|VideoClip3/session_<id>_<play>.mp4
#
# Uploads to Cloudinary, updates CSV, and cleans up files

suppressPackageStartupMessages({
  library(httr2)
  library(readr)
  library(dplyr)
  library(glue)
  library(stringr)
  library(purrr)
})

# ---- Helper Functions -------------------------------------------------------

now_iso <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

ensure_env <- function(name) {
  val <- Sys.getenv(name, unset = "")
  if (!nzchar(val)) stop(glue("Environment variable '{name}' must be set."))
  val
}

parse_video_filename <- function(filename, folder_name) {
  # iPhone videos are named like IMG_7634.MOV
  # Extract the number for ordering
  number_match <- str_extract(filename, "\\d+")
  
  if (is.na(number_match)) {
    warning(glue("Could not extract number from iPhone video filename: {filename}"))
    return(NULL)
  }
  
  # Use folder name to find matching CSV
  csv_file <- find_matching_csv_by_folder(folder_name)
  
  list(
    session_id = folder_name,  # Use folder name as session ID
    play_id = paste0("pitch_", number_match),  # Create play ID from number
    filename = filename,
    csv_file = csv_file,
    sort_number = as.numeric(number_match)  # For sorting
  )
}

find_matching_csv_by_folder <- function(folder_name) {
  # Folder name example: Pitching_2025-10-14T190857
  # Should match CSV: Pitching_2025-10-14T190857_verified.csv
  
  # Look for CSV files in data/ directory
  csv_files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)
  
  # Remove video_map.csv from consideration
  csv_files <- csv_files[!grepl("video_map\\.csv$", csv_files)]
  
  if (length(csv_files) == 0) {
    warning("No CSV files found in data/ directory")
    return(NULL)
  }
  
  # Look for exact match with folder name
  exact_match <- csv_files[grepl(paste0("^", folder_name), basename(csv_files))]
  
  if (length(exact_match) > 0) {
    cat(glue("  📅 Matched folder '{folder_name}' to CSV: {basename(exact_match[1])}\n"))
    return(exact_match[1])
  }
  
  # If no exact match, show available options
  cat(glue("  ⚠️  Could not find CSV matching folder '{folder_name}'\n"))
  cat(glue("     Available CSV files:\n"))
  for (csv in csv_files) {
    cat(glue("       - {basename(csv)}\n"))
  }
  
  return(NULL)
}

upload_to_cloudinary <- function(file_path, session_id, play_id, camera_slot, cloud_name, preset) {
  cat(glue("  📤 Uploading {basename(file_path)} to Cloudinary...\n"))
  
  # Create public ID following the same pattern as TrackMan videos
  public_id <- glue("trackman/{session_id}/{camera_slot}/{substr(tolower(play_id), 1, 12)}")
  
  upload_url <- glue("https://api.cloudinary.com/v1_1/{cloud_name}/video/upload")
  
  response <- request(upload_url) |>
    req_body_multipart(
      upload_preset = preset,
      file = curl::form_file(file_path),
      public_id = public_id,
      resource_type = "video"
    ) |>
    req_timeout(300) |>  # 5 minutes for large video files
    req_perform()
  
  if (resp_status(response) >= 400) {
    stop(glue("Cloudinary upload failed: {resp_body_string(response)}"))
  }
  
  result <- resp_body_json(response)
  cat(glue("  ✅ Uploaded: {result$secure_url}\n"))
  
  return(result)
}

update_video_map <- function(session_id, play_id, camera_slot, cloudinary_result, video_map_path) {
  # Load existing video map
  if (file.exists(video_map_path)) {
    video_map <- read_csv(video_map_path, show_col_types = FALSE)
    # Ensure uploaded_at is character for consistency
    if ("uploaded_at" %in% names(video_map)) {
      video_map$uploaded_at <- as.character(video_map$uploaded_at)
    }
  } else {
    dir.create(dirname(video_map_path), showWarnings = FALSE, recursive = TRUE)
    video_map <- tibble()
  }
  
  # Create new row
  new_row <- tibble(
    session_id = session_id,
    play_id = tolower(play_id),
    camera_slot = camera_slot,
    camera_name = "Phone",
    camera_target = "",
    video_type = "Manual",
    azure_blob = "",
    azure_md5 = "",
    cloudinary_url = cloudinary_result$secure_url,
    cloudinary_public_id = cloudinary_result$public_id,
    uploaded_at = now_iso()
  )
  
  # Add new row and save
  video_map <- bind_rows(video_map, new_row) %>% distinct()
  write_csv(video_map, video_map_path)
  
  cat(glue("  📝 Updated video_map.csv\n"))
}

process_video_folder <- function(session_folder, cloud_name, preset, video_map_path) {
  folder_name <- basename(session_folder)
  cat(glue("📁 Processing session folder: {folder_name}\n"))
  
  # Find VideoClip2 and VideoClip3 subfolders
  camera_folders <- list.dirs(session_folder, recursive = FALSE)
  camera_folders <- camera_folders[basename(camera_folders) %in% c("VideoClip2", "VideoClip3")]
  
  if (length(camera_folders) == 0) {
    cat("  ⚠️  No VideoClip2 or VideoClip3 folders found\n")
    return(character())
  }
  
  processed_files <- character()
  
  for (camera_folder in camera_folders) {
    camera_slot <- basename(camera_folder)
    cat(glue("  📹 Processing {camera_slot} videos...\n"))
    
    # Find video files (iPhone format: IMG_####.MOV)
    video_files <- list.files(
      camera_folder, 
      pattern = "\\.(mp4|mov|avi|mkv|webm)$", 
      ignore.case = TRUE,
      full.names = TRUE
    )
    
    if (length(video_files) == 0) {
      cat(glue("    ℹ️  No video files found in {camera_slot}\n"))
      next
    }
    
    # Parse all videos first to get sorting info
    video_info <- list()
    for (video_file in video_files) {
      parsed <- parse_video_filename(basename(video_file), folder_name)
      if (!is.null(parsed)) {
        parsed$full_path <- video_file
        video_info[[length(video_info) + 1]] <- parsed
      }
    }
    
    if (length(video_info) == 0) {
      cat(glue("    ⚠️  No valid video files found in {camera_slot}\n"))
      next
    }
    
    # Sort videos by number (IMG_7634.MOV comes before IMG_7635.MOV)
    video_info <- video_info[order(sapply(video_info, function(x) x$sort_number))]
    
    cat(glue("    📱 Found {length(video_info)} iPhone videos, processing in order:\n"))
    for (info in video_info) {
      cat(glue("       {info$filename} → {info$play_id}\n"))
    }
    
    for (info in video_info) {
      tryCatch({
        cat(glue("  🎥 Processing: {info$filename}\n"))
        cat(glue("    Session: {info$session_id}\n"))
        cat(glue("    Play: {info$play_id}\n"))
        cat(glue("    Camera: {camera_slot}\n"))
        if (!is.null(info$csv_file)) {
          cat(glue("    Target CSV: {basename(info$csv_file)}\n"))
        }
        
        # Upload to Cloudinary
        cloudinary_result <- upload_to_cloudinary(
          info$full_path, 
          info$session_id, 
          info$play_id, 
          camera_slot, 
          cloud_name, 
          preset
        )
        
        # Update both video_map.csv and the specific practice CSV
        update_video_map(
          info$session_id, 
          info$play_id, 
          camera_slot, 
          cloudinary_result, 
          video_map_path
        )
        
        # Also update the specific practice CSV if found
        if (!is.null(info$csv_file)) {
          update_practice_csv(
            info$csv_file,
            info$session_id,
            info$play_id,
            camera_slot,
            cloudinary_result$secure_url
          )
        }
        
        processed_files <- c(processed_files, info$full_path)
        cat(glue("  ✅ Successfully processed {info$filename}\n\n"))
        
      }, error = function(e) {
        cat(glue("  ❌ Failed to process {info$filename}: {e$message}\n\n"))
      })
    }
  }
  
  return(processed_files)
}

update_practice_csv <- function(csv_file, session_id, play_id, camera_slot, cloudinary_url) {
  if (!file.exists(csv_file)) {
    warning(glue("CSV file not found: {csv_file}"))
    return()
  }
  
  tryCatch({
    # Read the practice CSV
    practice_data <- read_csv(csv_file, show_col_types = FALSE)
    
    # Look for a row that matches this play
    # This is flexible - it could match by play number, pitch number, etc.
    # You might want to customize this logic based on your CSV structure
    
    # VideoClip2 updates VideoClip2 column, VideoClip3 updates VideoClip3 column
    # DO NOT touch VideoClip column - that's for EdgeR videos from TrackMan
    video_col <- camera_slot  # VideoClip2 or VideoClip3
    
    if (!video_col %in% names(practice_data)) {
      # Create the column if it doesn't exist
      practice_data[[video_col]] <- NA_character_
    }
    
    # Find rows where this video column is empty (first come, first served)
    empty_rows <- which(is.na(practice_data[[video_col]]) | practice_data[[video_col]] == "")
    
    if (length(empty_rows) > 0) {
      # Use the first empty row
      row_to_update <- empty_rows[1]
      practice_data[[video_col]][row_to_update] <- cloudinary_url
      
      cat(glue("  📝 Updated {basename(csv_file)} row {row_to_update} with {camera_slot} video\n"))
      
      # Write back to CSV
      write_csv(practice_data, csv_file)
    } else {
      cat(glue("  ⚠️  No empty {camera_slot} slots found in {basename(csv_file)}\n"))
    }
    
  }, error = function(e) {
    cat(glue("  ❌ Failed to update {basename(csv_file)}: {e$message}\n"))
  })
}

cleanup_processed_files <- function(processed_files) {
  if (length(processed_files) == 0) return()
  
  cat(glue("🧹 Cleaning up {length(processed_files)} processed files...\n"))
  
  for (file_path in processed_files) {
    if (file.exists(file_path)) {
      unlink(file_path)
      cat(glue("  🗑️  Deleted: {basename(file_path)}\n"))
    }
  }
  
  # Clean up empty directories
  processed_dirs <- unique(dirname(processed_files))
  for (dir_path in processed_dirs) {
    if (length(list.files(dir_path)) == 0) {
      unlink(dir_path, recursive = TRUE)
      cat(glue("  🗑️  Deleted empty directory: {basename(dir_path)}\n"))
    }
  }
}

# ---- Main Function -----------------------------------------------------------

main <- function() {
  cat("🎬 Starting video processing...\n\n")
  
  # Get environment variables
  cloud_name <- ensure_env("CLOUDINARY_CLOUD_NAME")
  preset <- ensure_env("CLOUDINARY_UPLOAD_PRESET")
  
  # Set paths
  video_processing_dir <- "video_processing"
  video_map_path <- "data/video_map.csv"
  
  if (!dir.exists(video_processing_dir)) {
    cat("ℹ️  No video_processing directory found. Nothing to process.\n")
    return()
  }
  
  # Find date folders (videos_YYYY-MM-DD)
  date_folders <- list.dirs(video_processing_dir, recursive = FALSE)
  date_folders <- date_folders[grepl("videos_\\d{4}-\\d{2}-\\d{2}$", basename(date_folders))]
  
  if (length(date_folders) == 0) {
    cat("ℹ️  No video date folders found. Nothing to process.\n")
    return()
  }
  
  # Sort folders by date (oldest first)
  date_folders <- date_folders[order(basename(date_folders))]
  
  all_processed_files <- character()
  
  for (date_folder in date_folders) {
    processed_files <- process_video_folder(date_folder, cloud_name, preset, video_map_path)
    all_processed_files <- c(all_processed_files, processed_files)
  }
  
  # Clean up processed files
  if (length(all_processed_files) > 0) {
    cleanup_processed_files(all_processed_files)
    cat(glue("\n🎉 Successfully processed {length(all_processed_files)} videos!\n"))
  } else {
    cat("\nℹ️  No videos were processed.\n")
  }
  
  # Clean up empty date folders
  for (date_folder in date_folders) {
    if (dir.exists(date_folder) && length(list.files(date_folder, recursive = TRUE)) == 0) {
      unlink(date_folder, recursive = TRUE)
      cat(glue("🗑️  Deleted empty date folder: {basename(date_folder)}\n"))
    }
  }
}

# Run if called directly
if (!interactive()) {
  main()
}
