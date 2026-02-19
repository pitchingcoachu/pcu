#!/usr/bin/env Rscript
# CSV Exclusion Management Script for GCU TrackMan Data
# 
# This script provides an easy way to manage CSV exclusions without manually editing files
# 
# Usage:
#   Rscript manage_exclusions.R                    # Interactive mode
#   Rscript manage_exclusions.R list               # List current exclusions
#   Rscript manage_exclusions.R add "pattern"      # Add exclusion pattern
#   Rscript manage_exclusions.R remove "pattern"   # Remove exclusion pattern
#   Rscript manage_exclusions.R recent             # Show recent files (last 24 hours)

source("csv_filter_utils.R")

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  # Interactive mode
  manage_csv_exclusions()
} else {
  command <- args[1]
  
  switch(command,
    "list" = {
      cat("=== Current CSV Exclusions ===\n")
      list_csv_exclusions()
    },
    "add" = {
      if (length(args) < 2) {
        cat("Error: Pattern required for add command\n")
        cat("Usage: Rscript manage_exclusions.R add \"pattern\"\n")
        quit(status = 1)
      }
      pattern <- args[2]
      comment <- if (length(args) >= 3) args[3] else NULL
      add_csv_exclusion(pattern, comment = comment)
    },
    "remove" = {
      if (length(args) < 2) {
        cat("Error: Pattern required for remove command\n")
        cat("Usage: Rscript manage_exclusions.R remove \"pattern\"\n")
        quit(status = 1)
      }
      pattern <- args[2]
      remove_csv_exclusion(pattern)
    },
    "recent" = {
      hours <- if (length(args) >= 2) as.numeric(args[2]) else 24
      if (is.na(hours)) hours <- 24
      check_recent_files(hours)
    },
    "help" = {
      cat("CSV Exclusion Management Script\n")
      cat("===============================\n\n")
      cat("Commands:\n")
      cat("  list                     - List current exclusions\n")
      cat("  add \"pattern\" [comment]   - Add exclusion pattern\n")
      cat("  remove \"pattern\"         - Remove exclusion pattern\n")
      cat("  recent [hours]           - Show recent files (default: 24 hours)\n")
      cat("  help                     - Show this help\n\n")
      cat("Interactive mode (no arguments) provides a menu interface.\n\n")
      cat("Examples:\n")
      cat("  Rscript manage_exclusions.R add \"BadData_2025-10-27\" \"Incorrect pitcher assignments\"\n")
      cat("  Rscript manage_exclusions.R remove \"BadData_2025-10-27\"\n")
      cat("  Rscript manage_exclusions.R recent 48\n")
    },
    {
      cat("Unknown command:", command, "\n")
      cat("Use 'help' for available commands\n")
      quit(status = 1)
    }
  )
}
