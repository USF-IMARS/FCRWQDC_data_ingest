# Required packages
library(dplyr)
library(here)
library(glue)
source(here("R/getFpath.R"))
library(lubridate)

# Get data from WIN format files
getWINData <- function(programName=NULL, fpath=NULL){
  fpath <- getFpath(programName, fpath, here::here("data/WIN/_WIN_WAVES_OTIS_{programName}.txt"))

  all_lines <- readLines(here(fpath))
  
  # Locate the header line (assumes it starts with "Organization ID")
  header_index <- grep('^"Organization ID"', all_lines)[1]
  
  # Print the skipped lines (everything before the header)
  # cat("Skipped lines:\n")
  # cat(all_lines[1:(header_index - 1)], sep = "\n")
  # cat("\n\n")
  
  # Extract the header line
  header_line <- all_lines[header_index]
  
  # Determine the expected number of columns based on the header line
  expected_cols <- length(strsplit(header_line, "\\|")[[1]])
  
  # Extract all remaining lines (which may contain multi-line records)
  raw_data_lines <- all_lines[(header_index + 1):length(all_lines)]
  
  # Reassemble rows by combining lines until the number of delimiters (pipes) matches expectation.
  combined_rows <- character(0)
  temp_row <- NA
  
  for (line in raw_data_lines) {
    # Start a new temporary row or append to the existing one
    temp_row <- if (is.na(temp_row)) line else paste(temp_row, line, sep = "\n")
    
    # Count the number of pipe delimiters in temp_row
    n_delim <- length(gregexpr("\\|", temp_row)[[1]])
    
    # If the row has the expected number of delimiters (one less than columns), it's complete.
    if (n_delim == (expected_cols - 1)) {
      combined_rows <- c(combined_rows, temp_row)
      temp_row <- NA  # Reset for the next record
    }
  }
  
  # In case any data remains in temp_row, add it as a record
  if (!is.na(temp_row)) {
    combined_rows <- c(combined_rows, temp_row)
  }
  
  # Reassemble the complete text with header and data rows
  full_text <- paste(c(header_line, combined_rows), collapse = "\n")
  
  # Read the data from the reassembled text
  result_df <- read.table(text = full_text,
                   sep = "|",
                   header = TRUE,
                   quote = "\"",
                   fill = TRUE,
                   colClasses = 'character')
                   
  # Store the original column count
  original_cols <- ncol(result_df)
  original_col_names <- names(result_df)
  # cat("\n")
  # cat("=== WIN Data Loading ===\n")
  # cat(glue("Source: {basename(fpath)}\n"))
  # cat(glue("Loaded {original_cols} columns\n"))
  
  # Apply the alignment function to ensure consistency with other data sources
  source(here("R/align_win_df.R"))
  result_df <- align_win_df(result_df)
  
  # Calculate dropped columns
  final_cols <- ncol(result_df)
  final_col_names <- names(result_df)
  dropped_cols <- setdiff(original_col_names, final_col_names)
  dropped_count <- length(dropped_cols)
  
  # cat("--- Column statistics ---\n")
  # if (dropped_count > 0) {
  #   cat(glue("Dropped {dropped_count} columns during processing:\n"))
  #   for (col in dropped_cols) {
  #     cat(glue("  - {col}\n"))
  #   }
  # } else {
  #   cat("No columns were dropped during processing\n")
  # }
  # cat(glue("Final column count: {final_cols}\n"))
  # cat("------------------------\n")
  
  result_df$program = programName
  result_df$source = programName
  
  
  result_df <-result_df %>%
    mutate(
      Monitoring.Location.ID = as.character(Monitoring.Location.ID),
      Activity.Start.Date.Time = as.Date(Activity.Start.Date.Time, format="%m/%d/%y"),
      Year                      = year(Activity.Start.Date.Time),
      Month                     = month(Activity.Start.Date.Time),
      # === additional columns for STORET mapping
      Habitat = Matrix,
      RowID = NA,
      ProgramID = NA,
      IndicatorID = NA,
      IndicatorName = NA,
      ParameterID = NA,
      AreaID = NA,
      ManagedAreaName = NA,
      Region = NA,
      RelativeDepth = NA,
      TotalDepth_m = NA,
      MDL = DEP.MDL,
      PQL = DEP.PQL,
      DetectionUnit = NA,
      ValueQualifierSource = NA,
      SEACAR_QAQCFlagCode = NA,
      SEACAR_QAQC_Description = NA,
      Include = NA,
      MADup = NA,
      ExportVersion = NA,
      .keep = 'unused',
      
      DEP.Analyte.Name = recode(
        DEP.Analyte.Name,
        "Ammonia (N)"             = "Total Ammonia",
        "Chlorophyll a- corrected" = "Chlorophyll a, Corrected for Pheophytin",
        "Chlorophyll a, free of pheophytin" = "Chlorophyll a, Corrected for Pheophytin",
        "Chlorophyll a- uncorrected" = "Chlorophyll a, Uncorrected for Pheophytin",
        "Nitrate (N)" = "Nitrate (NO3)",
        "Nitrate-Nitrite (N)" = "NO2+3, Filtered",
        "Nitrite (N)" = "Nitrite (NO2)",
        "Nitrogen- Total" = "Total Nitrogen",
        "Nitrogen- Total Kjeldahl" = "Total Kjeldahl Nitrogen",
        "Phosphorus- Total" = "Total Phosphorus",
        "Temperature, Water" = "Water Temperature"
      )
    )
      
  # drop rows with missing critical values
  # NOTE: could we instead filter for Activity.Type == "Sample"?
  result_df <- result_df %>%
    # found in BBAP dataset
    filter(`Activity.Type` != "Matrix Spike") %>%
    filter(`Activity.Type` != "Matrix Spike Replicate") %>%
    filter(`Activity.Type` != "Laboratory Control Sample") %>%
    filter(`Activity.Type` != "Lab Replicate") %>%
    filter(`Activity.Type` != "Laboratory Control Sample Replicate") %>%
    filter(`Activity.Type` != "Method Blank") %>%
    filter(`Activity.Type` != "Field Blank") %>%
    # found from BROWARD
    filter(`Activity.Type` != "Equipment Blank")
  
    
    
    
  # Return the dataframe
  return(result_df)
}
