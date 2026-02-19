# Pitch Modifications Troubleshooting Utility
# Use this script to diagnose and fix pitch modification persistence issues

library(DBI)
library(RSQLite)
library(readr)
library(dplyr)

# Source the app functions we need
source("app.R", local = TRUE)

check_modifications_status <- function() {
  cat("=== Pitch Modifications Status Check ===\n\n")

  backend_type <- pitch_mod_backend_type()
  cat("Pitch modification backend:", backend_type, "\n")
  if (identical(backend_type, "sqlite")) {
    db_path <- get_modifications_db_path()
    cat("Database path:", db_path, "\n")
    cat("Database exists:", file.exists(db_path), "\n")
  } else {
    cfg <- pitch_mod_backend_config()
    cat("Postgres host:", cfg$host, "database:", cfg$dbname, "\n")
  }

  con <- mod_db_connect()
  if (inherits(con, "error") || is.null(con)) {
    cat("Could not connect to pitch modifications store:", conditionMessage(con), "\n")
  } else {
    on.exit(dbDisconnect(con), add = TRUE)
    tryCatch({
      tbl <- as.character(pitch_mod_table_clause(con))
      ns_clause <- pitch_mod_namespace_clause(con)
      mods <- dbGetQuery(con, sprintf("SELECT COUNT(*) as count FROM %s WHERE namespace = %s", tbl, ns_clause))
      cat("Database modifications count:", mods$count, "\n")

      if (mods$count > 0) {
        recent <- dbGetQuery(con, sprintf(
          "SELECT pitcher, date, original_pitch_type, new_pitch_type, modified_at
           FROM %s WHERE namespace = %s ORDER BY created_at DESC LIMIT 5",
          tbl, ns_clause
        ))
        cat("\nMost recent modifications:\n")
        print(recent)
      }
    }, error = function(e) {
      cat("Error reading database:", e$message, "\n")
    })
  }
  
  # Check export CSV
  export_path <- get_modifications_export_path()
  cat("\nExport CSV path:", export_path, "\n")
  cat("Export CSV exists:", file.exists(export_path), "\n")
  
  if (file.exists(export_path)) {
    tryCatch({
      mods_csv <- read_csv(export_path, show_col_types = FALSE)
      cat("Export CSV modifications count:", nrow(mods_csv), "\n")
      
      if (nrow(mods_csv) > 0) {
        recent_csv <- mods_csv %>% 
          arrange(desc(created_at)) %>% 
          slice_head(n = 5) %>%
          select(pitcher, date, original_pitch_type, new_pitch_type, modified_at)
        cat("\nMost recent modifications in CSV:\n")
        print(recent_csv)
      }
    }, error = function(e) {
      cat("Error reading export CSV:", e$message, "\n")
    })
  }
  
  # Check the regular CSV file
  regular_csv_path <- file.path("data", "pitch_type_modifications.csv")
  cat("\nRegular CSV path:", regular_csv_path, "\n")
  cat("Regular CSV exists:", file.exists(regular_csv_path), "\n")
  
  if (file.exists(regular_csv_path)) {
    tryCatch({
      regular_mods <- read_csv(regular_csv_path, show_col_types = FALSE)
      cat("Regular CSV modifications count:", nrow(regular_mods), "\n")
    }, error = function(e) {
      cat("Error reading regular CSV:", e$message, "\n")
    })
  }
}

# Function to sync database to export CSV
sync_db_to_export <- function() {
  cat("=== Syncing Database to Export CSV ===\n")
  
  init_modifications_db()
  con <- mod_db_connect()
  if (inherits(con, "error") || is.null(con)) {
    cat("Could not connect to pitch modifications store:", conditionMessage(con), "\n")
    return(FALSE)
  }
  on.exit(dbDisconnect(con), add = TRUE)
  
  tryCatch({
    write_modifications_snapshot(con)
    cat("Database synced to export CSV successfully\n")
    return(TRUE)
  }, error = function(e) {
    cat("Error syncing database to export:", e$message, "\n")
    return(FALSE)
  })
}

# Function to import from export CSV to database
import_from_export <- function() {
  cat("=== Importing from Export CSV to Database ===\n")
  
  export_path <- get_modifications_export_path()
  if (!file.exists(export_path)) {
    cat("No export CSV found - nothing to import\n")
    return(FALSE)
  }
  
  tryCatch({
    init_modifications_db()
    con <- mod_db_connect()
    if (inherits(con, "error") || is.null(con)) {
      cat("Could not connect to pitch modifications store:", conditionMessage(con), "\n")
      return(FALSE)
    }
    on.exit(dbDisconnect(con), add = TRUE)
    
    import_modifications_from_export(con, NULL)
    cat("Export CSV imported to database successfully\n")
    return(TRUE)
  }, error = function(e) {
    cat("Error importing from export:", e$message, "\n")
    return(FALSE)
  })
}

# Function to copy regular CSV to export CSV
copy_regular_to_export <- function() {
  cat("=== Copying Regular CSV to Export CSV ===\n")
  
  regular_path <- file.path("data", "pitch_type_modifications.csv")
  export_path <- get_modifications_export_path()
  
  if (!file.exists(regular_path)) {
    cat("No regular CSV found\n")
    return(FALSE)
  }
  
  tryCatch({
    # Clean up the regular CSV data
    regular_data <- read_csv(regular_path, show_col_types = FALSE)
    
    # Fix date formatting issues
    if ("date" %in% names(regular_data)) {
      # Convert any numeric dates to proper format
      regular_data$date <- as.character(as.Date(as.numeric(regular_data$date), origin = "1899-12-30"))
      regular_data$date[is.na(regular_data$date)] <- regular_data$date[!is.na(regular_data$date)][1] # fallback
    }
    
    # Fix timestamp formatting
    if ("modified_at" %in% names(regular_data)) {
      # Clean up malformed timestamps
      regular_data$modified_at <- gsub("^(\\d+):(\\d+\\.\\d+)$", "2025-10-23 \\1:\\2", regular_data$modified_at)
    }
    
    if ("created_at" %in% names(regular_data)) {
      regular_data$created_at <- gsub("^(\\d{4})$", "2025-10-23 12:00:00", regular_data$created_at)
    }
    
    dir.create(dirname(export_path), recursive = TRUE, showWarnings = FALSE)
    write_csv(regular_data, export_path)
    cat("Copied", nrow(regular_data), "modifications from regular CSV to export CSV\n")
    return(TRUE)
  }, error = function(e) {
    cat("Error copying regular to export:", e$message, "\n")
    return(FALSE)
  })
}

# Main menu
main_menu <- function() {
  cat("\n=== Pitch Modifications Troubleshooting ===\n")
  cat("1. Check status of all modification files\n")
  cat("2. Sync database to export CSV\n")
  cat("3. Import export CSV to database\n")
  cat("4. Copy regular CSV to export CSV\n")
  cat("5. Exit\n")
  cat("Choose an option (1-5): ")
  
  choice <- readline()
  
  switch(choice,
    "1" = check_modifications_status(),
    "2" = sync_db_to_export(),
    "3" = import_from_export(),
    "4" = copy_regular_to_export(),
    "5" = return(),
    cat("Invalid choice\n")
  )
}

# Run if called directly
if (!interactive()) {
  # If arguments provided, run specific function
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 0) {
    if (args[1] == "status") {
      check_modifications_status()
    } else if (args[1] == "sync") {
      sync_db_to_export()
    } else if (args[1] == "import") {
      import_from_export()
    } else if (args[1] == "copy") {
      copy_regular_to_export()
    } else {
      cat("Usage: Rscript troubleshoot_modifications.R [status|sync|import|copy]\n")
    }
  } else {
    # Interactive mode
    while(TRUE) {
      main_menu()
      cat("\n")
    }
  }
}
