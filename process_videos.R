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

parse_video_filename <- function(filename) {
  # Expected format: session_<session-id>_<play-id>.<ext>
  # Example: session_d527cb2a-39c9-47b4-a25d-b58bc4772f10_pitch1.mp4
  # Or: session_practice_2024_01_02_Pitching_2024-01-02T173502_pitch1.mp4
  
  # Remove extension
  name_no_ext <- tools::file_path_sans_ext(filename)
  
  # Match pattern
  pattern <- "^session_(.+)_(.+)$"
  match <- str_match(name_no_ext, pattern)
  
  if (is.na(match[1])) {
    warning(glue("Filename '{filename}' doesn't match expected pattern: session_<session-id>_<play-id>.<ext>"))
    return(NULL)
  }
  
  session_part <- match[2]
  play_id <- match[3]
  
  # Try to find matching CSV file based on session identifier
  csv_file <- find_matching_csv(session_part)
  
  list(
    session_id = session_part,
    play_id = play_id,
    filename = filename,
    csv_file = csv_file
  )
}

find_matching_csv <- function(session_identifier) {
  # Look for CSV files in data/ directory
  csv_files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)
  
  # Remove video_map.csv from consideration
  csv_files <- csv_files[!grepl("video_map\\.csv$", csv_files)]
  
  if (length(csv_files) == 0) {
    warning("No CSV files found in data/ directory")
    return(NULL)
  }
  
  # Strategy 1: Exact match with session identifier
  exact_match <- csv_files[grepl(session_identifier, basename(csv_files), fixed = TRUE)]
  if (length(exact_match) > 0) {
    return(exact_match[1])  # Return first match
  }
  
  # Strategy 2: Try to extract date from session identifier and match
  # Look for date patterns in session_identifier (YYYY-MM-DD or YYYY_MM_DD)
  date_patterns <- c(
    "\\d{4}-\\d{2}-\\d{2}",  # 2024-01-02
    "\\d{4}_\\d{2}_\\d{2}"   # 2024_01_02
  )
  
  for (pattern in date_patterns) {
    extracted_date <- str_extract(session_identifier, pattern)
    if (!is.na(extracted_date)) {
      # Convert underscores to hyphens for standardization
      extracted_date <- gsub("_", "-", extracted_date)
      
      # Look for CSV files containing this date
      date_match <- csv_files[grepl(extracted_date, basename(csv_files))]
      if (length(date_match) > 0) {
        cat(glue("  📅 Matched video to CSV based on date: {basename(date_match[1])}\n"))
        return(date_match[1])
      }
    }
  }
  
  # Strategy 3: If no date match, let user know and return most recent CSV
  cat(glue("  ⚠️  Could not auto-match session '{session_identifier}' to a specific CSV\n"))
  cat(glue("     Available CSV files:\n"))
  for (csv in csv_files) {
    cat(glue("       - {basename(csv)}\n"))
  }
  cat(glue("     Using most recent CSV file\n"))
  
  # Return most recent CSV file
  file_info <- file.info(csv_files)
  most_recent <- csv_files[which.max(file_info$mtime)]
  return(most_recent)
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

process_video_folder <- function(date_folder, cloud_name, preset, video_map_path) {
  cat(glue("📁 Processing folder: {basename(date_folder)}\n"))
  
  # Find VideoClip2 and VideoClip3 subfolders
  camera_folders <- list.dirs(date_folder, recursive = FALSE)
  camera_folders <- camera_folders[basename(camera_folders) %in% c("VideoClip2", "VideoClip3")]
  
  if (length(camera_folders) == 0) {
    cat("  ⚠️  No VideoClip2 or VideoClip3 folders found\n")
    return(character())
  }
  
  processed_files <- character()
  
  for (camera_folder in camera_folders) {
    camera_slot <- basename(camera_folder)
    cat(glue("  📹 Processing {camera_slot} videos...\n"))
    
    # Find video files
    video_files <- list.files(
      camera_folder, 
      pattern = "\\.(mp4|mov|avi|mkv|webm)$", 
      ignore.case = TRUE,
      full.names = TRUE
    )
    
    # Sort video files alphabetically to ensure consistent processing order
    video_files <- sort(video_files)
    
    if (length(video_files) == 0) {
      cat(glue("    ℹ️  No video files found in {camera_slot}\n"))
      next
    }
    
    for (video_file in video_files) {
      tryCatch({
        # Parse filename
        parsed <- parse_video_filename(basename(video_file))
        if (is.null(parsed)) next
        
        cat(glue("  🎥 Processing: {parsed$filename}\n"))
        cat(glue("    Session: {parsed$session_id}\n"))
        cat(glue("    Play: {parsed$play_id}\n"))
        cat(glue("    Camera: {camera_slot}\n"))
        if (!is.null(parsed$csv_file)) {
          cat(glue("    Target CSV: {basename(parsed$csv_file)}\n"))
        }
        
        # Upload to Cloudinary
        cloudinary_result <- upload_to_cloudinary(
          video_file, 
          parsed$session_id, 
          parsed$play_id, 
          camera_slot, 
          cloud_name, 
          preset
        )
        
        # Update both video_map.csv and the specific practice CSV
        update_video_map(
          parsed$session_id, 
          parsed$play_id, 
          camera_slot, 
          cloudinary_result, 
          video_map_path
        )
        
        # Also update the specific practice CSV if found
        if (!is.null(parsed$csv_file)) {
          update_practice_csv(
            parsed$csv_file,
            parsed$session_id,
            parsed$play_id,
            camera_slot,
            cloudinary_result$secure_url
          )
        }
        
        processed_files <- c(processed_files, video_file)
        cat(glue("  ✅ Successfully processed {parsed$filename}\n\n"))
        
      }, error = function(e) {
        cat(glue("  ❌ Failed to process {basename(video_file)}: {e$message}\n\n"))
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
    
    # Strategy 1: Look for empty VideoClip2 or VideoClip3 columns
    video_col <- paste0(camera_slot)  # VideoClip2 or VideoClip3
    
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
