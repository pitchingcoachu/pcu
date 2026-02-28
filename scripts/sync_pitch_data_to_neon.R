#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(readr)
  library(dplyr)
  library(stringr)
  library(digest)
})

source("pitch_data_service.R")

args <- commandArgs(trailingOnly = TRUE)
root_data_dir <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else file.path("data")
workers <- if (length(args) >= 2) suppressWarnings(as.integer(args[[2]])) else suppressWarnings(as.integer(Sys.getenv("PITCH_DATA_SYNC_WORKERS", "2")))
if (is.na(workers) || workers < 1L) workers <- 2L
team_code_default <- "PCU"
if (file.exists(file.path("config", "school_config.R"))) {
  try(source(file.path("config", "school_config.R")), silent = TRUE)
  if (exists("school_config") && is.list(school_config) &&
      !is.null(school_config$team_code) && nzchar(as.character(school_config$team_code))) {
    team_code_default <- as.character(school_config$team_code)
  }
}
school_code <- Sys.getenv("TEAM_CODE", team_code_default)

cat("Pitch data sync starting\n")
cat("data_dir:", root_data_dir, "\n")
cat("workers:", workers, "\n")
cat("school_code:", school_code, "\n")

result <- sync_csv_tree_to_neon(
  data_dir = root_data_dir,
  school_code = school_code,
  workers = workers
)

cat("Rows synced:", as.integer(result), "\n")
cat("Pitch data sync complete\n")
