#!/usr/bin/env Rscript
# Fix Video Mapping Script
# Update video_map_manual.csv to use correct PlayIDs from CSV

library(readr)
library(dplyr)

# Read the CSV file
csv_file <- "data/practice/practice_2025_10_11_Pitching_2025-10-11T173550_verified.csv"
video_map_file <- "data/video_map_manual.csv"

cat("🔧 Fixing video mapping...\n")

# Read CSV data
csv_data <- read_csv(csv_file, show_col_types = FALSE)
cat("📊 CSV has", nrow(csv_data), "rows\n")

# Read current video map
video_map <- read_csv(video_map_file, show_col_types = FALSE)
cat("🎥 Video map has", nrow(video_map), "entries\n")

# Filter for this session
session_videos <- video_map %>%
  filter(session_id == "Pitching_2025-10-11T173550") %>%
  arrange(play_id)  # This will sort by pitch_8317, pitch_8318, etc.

cat("📱 Found", nrow(session_videos), "videos for this session\n")

if (nrow(session_videos) > 0 && nrow(csv_data) >= nrow(session_videos)) {
  # Update video map with correct PlayIDs
  for (i in seq_len(nrow(session_videos))) {
    if (i <= nrow(csv_data)) {
      correct_play_id <- csv_data$PlayID[i]
      old_play_id <- session_videos$play_id[i]
      
      # Update in the full video map
      video_map <- video_map %>%
        mutate(play_id = ifelse(
          session_id == "Pitching_2025-10-11T173550" & play_id == old_play_id,
          correct_play_id,
          play_id
        ))
      
      cat("✅ Video", i, ":", old_play_id, "→", correct_play_id, "\n")
    }
  }
  
  # Write updated video map
  write_csv(video_map, video_map_file)
  cat("💾 Updated video map saved!\n")
  
} else {
  cat("❌ Mismatch: CSV has", nrow(csv_data), "rows, videos:", nrow(session_videos), "\n")
}