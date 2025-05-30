# Required packages
library(dplyr)
library(here)
library(glue)
source(here("R/getFpath.R"))

STORETFileToDataFrame <- function(fpath){
  # read dataframe from pipe-delimited file
  # print(glue('reading file {fpath}...'))
  df <- read.delim(
    file           = fpath,
    sep            = "|",
    header         = TRUE,
    stringsAsFactors = FALSE,
    na.strings     = c("", "NA")
  )
  
  # Store the original column count
  original_cols <- ncol(df)
  original_col_names <- names(df)
  cat("\n")
  cat("=== STORET Data Loading ===\n")
  cat(glue("Source: {basename(fpath)}\n"))
  cat(glue("Loaded {original_cols} columns\n"))
  
  # Load and apply the STORET column alignment function
  source(here("R/align_storet_df.R"))
  df <- align_storet_df(df)

  # Calculate dropped columns
  final_cols <- ncol(df)
  final_col_names <- names(df)
  dropped_cols <- setdiff(original_col_names, final_col_names)
  dropped_count <- length(dropped_cols)
  
  cat("--- Column statistics ---\n")
  if (dropped_count > 0) {
    cat(glue("Dropped {dropped_count} columns during processing:\n"))
    for (col in dropped_cols) {
      cat(glue("  - {col}\n"))
    }
  } else {
    cat("No columns were dropped during processing\n")
  }
  cat(glue("Final column count: {final_cols}\n"))
  cat("------------------------\n")

  return(df)
}

# Get data from STORET historical format files (pipe-delimited)
getSTORETData <- function(programNam=NULL, fpath=NULL) {
  fpath <- getFpath(
    programName, 
    fpath, 
    "data/STORET_historical/STORET_Water_Quality_Results_{programName}.txt")
  df <- STORETFileToDataFrame(fpath)
  
  if (programName == 'DERM_BBWQ') {
    # also include the 1970–1995 legacy file
    fpath2 <- glue('{storetPath}/STORET_Water_Quality_Results_{programName}_1970_1995.txt')
    df2 <- STORETFileToDataFrame(fpath2)
    
    # merge them by row
    df <- dplyr::bind_rows(df, df2)
  }
  return(df)
}
