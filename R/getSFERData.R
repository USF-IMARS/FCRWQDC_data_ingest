# Required packages
library(dplyr)
library(here)
library(glue)
source(here("R/getFpath.R"))

# Get data from SFER CSV format files
getSFERData <- function(programNam=null, fpath=null) {
  fpath <- getFpath(
    programName, 
    fpath, 
    "data/SFER_data.csv"
  )

  df <- read.csv(fpath)
  
  # Store the original column count
  original_cols <- ncol(df)
  original_col_names <- names(df)
  cat("\n")
  cat("=== SFER Data Loading ===\n")
  cat(glue::glue("Source: {basename(fpath)}\n"))
  cat(glue::glue("Loaded {original_cols} columns\n"))
  
  # modify df to align with WIN standards
  source(here("R/align_sfer_df.R"))
  # Using explicit call to the function with the namespace to avoid lint errors
  df <- align_sfer_df(df)
  
  # Calculate dropped columns
  final_cols <- ncol(df)
  final_col_names <- names(df)
  dropped_cols <- setdiff(original_col_names, final_col_names)
  dropped_count <- length(dropped_cols)
  
  cat("--- Column statistics ---\n")
  if (dropped_count > 0) {
    cat(glue::glue("Dropped {dropped_count} columns during processing:\n"))
    for (col in dropped_cols) {
      cat(glue::glue("  - {col}\n"))
    }
  } else {
    cat("No columns were dropped during processing\n")
  }
  cat(glue::glue("Final column count: {final_cols}\n"))
  cat("------------------------\n")
  
  return(df)
}
