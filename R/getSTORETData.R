# Required packages
library(dplyr)
library(here)
library(glue)
source(here("R/getFpath.R"))

getSTORETData <- function(programName, fpath=NULL){
  if(!is.null(fpath)){
    df <- read.csv(here::here(fpath),
      colClasses='character'
    )
  } else {
    pName <- stringr::str_replace(programName, "_STORET", "")
    df <- read.csv(here::here(
      glue("data/STORET_historical/{pName}_STORET_ALL.csv")),
      colClasses='character'
    )
  }
  
  
  # align hist_data to WIN format
  df <- df %>% mutate(
    Organization.ID = programName,
    Sampling.Agency.Name = programName,
    Monitoring.Location.ID = as.character(Station),
    Activity.Start.Date.Time = as.Date(gsub("'", NA, Date), format = "%m/%d/%y"),
    # special exception for DERM_BBWQ (missing depth)
    Activity.Depth = if ("Depth" %in% colnames(.)) .data$Depth else NA_real_,
    DEP.Analyte.Name = Parameter,
    DEP.Result.Value.Number = Value,
    DEP.Result.Unit = Unit,
    Value.Qualifier = VQ,
    RowID = NA,
    .keep = 'unused',
    DEP.Analyte.Name = recode(
      DEP.Analyte.Name,
      "Chlorophyll a, corrected for pheophytin" = "Chlorophyll a, Corrected for Pheophytin",
      "Chlorophyll a, free of pheophytin" = "Chlorophyll a, Corrected for Pheophytin",
      "Chlorophyll a, uncorrected for pheophytin" = "Chlorophyll a, Uncorrected for Pheophytin",
      "Dissolved oxygen (DO)" = "Dissolved Oxygen",
      "Nitrogen, Kjeldahl" = "Total Kjeldahl Nitrogen",
      "Nitrogen, Nitrate (NO3) as NO3" = "Nitrate (NO3)",
      "Nitrogen, Nitrite (NO2) as NO2" = "Nitrite (NO2)",
      "Nitrogen, ammonia (NH3) as NH3" = "Ammonia, Un-ionized (NH3)",
      "Phosphorus, orthophosphate as PO4" = "Phosphate, Filtered (PO4)",
      "Temperature, water" = "Water Temperature"
    )
  )
  return(df)
}
# ===================================================
# old STORET format loaders:
# ===================================================
# STORETFileToDataFrame <- function(fpath){
#   # read dataframe from pipe-delimited file
#   # print(glue('reading file {fpath}...'))
#   df <- read.delim(
#     file           = fpath,
#     sep            = "|",
#     header         = TRUE,
#     stringsAsFactors = FALSE,
#     na.strings     = c(NA, "NA")
#   )
#   
#   # Store the original column count
#   original_cols <- ncol(df)
#   original_col_names <- names(df)
#   # cat("\n")
#   # cat("=== STORET Data Loading ===\n")
#   # cat(glue("Source: {basename(fpath)}\n"))
#   # cat(glue("Loaded {original_cols} columns\n"))
#   
#   # Load and apply the STORET column alignment function
#   source(here("R/align_storet_df.R"))
#   df <- align_storet_df(df)
# 
#   # Calculate dropped columns
#   final_cols <- ncol(df)
#   final_col_names <- names(df)
#   dropped_cols <- setdiff(original_col_names, final_col_names)
#   dropped_count <- length(dropped_cols)
#   
#   # cat("--- Column statistics ---\n")
#   # if (dropped_count > 0) {
#   #   cat(glue("Dropped {dropped_count} columns during processing:\n"))
#   #   for (col in dropped_cols) {
#   #     cat(glue("  - {col}\n"))
#   #   }
#   # } else {
#   #   cat("No columns were dropped during processing\n")
#   # }
#   # cat(glue("Final column count: {final_cols}\n"))
#   # cat("------------------------\n")
# 
#   return(df)
# }
# 
# # Get data from STORET historical format files (pipe-delimited)
# getSTORETData <- function(programName=NULL, fpath=NULL) {
#   fpath <- getFpath(
#     programName, 
#     fpath, 
#     here::here("data/STORET_historical/STORET_Water_Quality_Results_{programName}.txt"))
#   df <- STORETFileToDataFrame(fpath)
# 
#   return(df)
# }
