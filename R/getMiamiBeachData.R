#' Get data from Miami Beach water quality monitoring program
#'
#' @return A dataframe with Miami Beach water quality data
#' @importFrom readxl read_excel
#' @importFrom dplyr bind_rows mutate rename select filter
#' @importFrom purrr map_df
#' @importFrom here here
#' @importFrom glue glue
#' @export
getMiamiBeachData <- function() {
  # Load required packages
  library(readxl)
  library(dplyr)
  library(purrr)
  library(here)
  library(glue)
  
  # TODO: (?) filter to key columns of interest for efficiency
  # NOTE: currently unused
  # key_columns <- c(
  #   # Station ID column
  #   "CLIENT SAMPLE ID",
    
  #   # Primary parameters of interest
  #   "Turbidity",
  #   "Nitrogen, Ammonia",
  #   "Nitrogen, Kjeldahl, Total",
  #   "Nitrogen, NO2 plus NO3",
  #   "Phosphorus, Total (as P) LL",
    
  #   # Secondary parameters
  #   "Salinity",
  #   "Oxygen, Dissolved",
  #   "Field pH",
  #   "Field Temperature",
    
  #   # Date/time and other metadata columns
  #   "SAMPLE COLLECTION DATE",
  #   "SAMPLE COLLECTION TIME",
  #   "PARAMETER NAME",
  #   "RESULT",
  #   "UNITS"
  # )  
  cat("\n=== Processing Miami Beach Water Quality Data ===\n")
  
  # === Load historical data from pipe-delimited file
  historical_file <- here("data/MiamiBeach/Discrete WQ - 4058.txt")
  cat(glue("Loading historical data from {basename(historical_file)}\n"))
  
  if (file.exists(historical_file)) {
    # Read the pipe-delimited file
    historical_df <- read.delim(
      file = historical_file,
      sep = "|",  # Pipe delimiter
      header = TRUE,
      stringsAsFactors = FALSE,
      na.strings = c("", "NA")
    )
    
    cat(glue("Loaded {nrow(historical_df)} rows and {ncol(historical_df)} columns from historical file\n"))
  } else {
    cat("Historical data file not found. Starting with empty dataframe.\n")
    historical_df <- data.frame()
  }

  # align historical data to WIN column names
  # historical columns:
  # RowID|ProgramID|ProgramName|Habitat|IndicatorID|IndicatorName|
  #ParameterID|ParameterName|ParameterUnits|ProgramLocationID|AreaID|
  #ManagedAreaName|Region|ActivityType|SampleDate|ResultValue|Year|Month|
  #ActivityDepth_m|RelativeDepth|TotalDepth_m|MDL|PQL|DetectionUnit|
  #ValueQualifier|ValueQualifierSource|SampleFraction|ResultComments|
  #OriginalLatitude|OriginalLongitude|SEACAR_QAQCFlagCode|
  #SEACAR_QAQC_Description|Include|SEACAR_EventID|MADup|ExportVersion

  # WIN columns:
  # Monitoring.Location.ID Activity.Start.Date.Time DEP.Analyte.Name
  # DEP.Result.Value.Number DEP.Result.Unit

  historical_df <- historical_df %>%
    dplyr::rename(
      Monitoring.Location.ID = `ProgramLocationID`,
      Activity.Start.Date.Time = `SampleDate`,
      DEP.Analyte.Name = `ParameterName`,
      DEP.Result.Value.Number = `ResultValue`,
      DEP.Result.Unit = `ParameterUnits`
    )



  
  # === Load 2024 data from Excel files
  xls_directory <- here("data/MiamiBeach/2024")
  cat(glue("Searching for Excel files in {xls_directory}\n"))
  
  # Find all Excel files recursively
  excel_files <- list.files(
    path = xls_directory,
    pattern = "\\.xls$|\\.xlsx$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  cat(glue("Found {length(excel_files)} Excel files\n"))
  
  # Function to read an Excel file and process it
  read_excel_file <- function(file_path) {
    cat(glue("Processing {basename(file_path)}\n"))
    
    # Extract date from filename for files without date columns
    file_date <- gsub(".*Results ([0-9\\-]+).*\\.xls.*", "\\1", basename(file_path))
    if (file_date == basename(file_path)) {
      # Try another pattern for files like "12-30-2024 Data.xls"
      file_date <- gsub(".*([0-9\\-]+) Data\\.xls.*", "\\1", basename(file_path))
    }
    
    tryCatch({
      # Read the Excel file
      df <- readxl::read_excel(file_path, sheet = 1)
      
      # Add source file info
      df$source_file <- basename(file_path)
      
      # If file doesn't have date column, use filename date
      if (!"SAMPLE COLLECTION DATE" %in% names(df) && file_date != basename(file_path)) {
        df$`SAMPLE COLLECTION DATE` <- file_date
      }
      
      return(df)
    }, error = function(e) {
      cat(glue("Error reading {basename(file_path)}: {e$message}\n"))
      return(data.frame())
    })
  }
  
  # Process all Excel files and combine them
  if (length(excel_files) > 0) {
    excel_data_list <- lapply(excel_files, read_excel_file)
    excel_df <- dplyr::bind_rows(excel_data_list)
    cat(glue("Combined {nrow(excel_df)} rows from Excel files\n"))
  } else {
    excel_df <- data.frame()
    cat("No Excel files found\n")
  }
  
  # Process data differently based on source to avoid duplicate column issues
  
  # Function to standardize column names across different data sources
  standardize_columns <- function(df, source_type) {
    # First check if dataframe is empty
    if (nrow(df) == 0) {
      cat(glue("No data found for {source_type}\n"))
      return(data.frame())
    }
    
    # Get current column names
    current_cols <- names(df)
    cat(glue("Column names in {source_type} data: {paste(current_cols, collapse=', ')}\n"))
    
    # Create a fresh dataframe with the same number of rows as the input
    result_df <- data.frame(row_id = 1:nrow(df))
    
    # Map source-specific columns to standardized column names
    if (source_type == "historical") {
      # Only add columns if they exist in the source data
      if ("ProgramLocationID" %in% current_cols) {
        result_df$Monitoring.Location.ID <- df$ProgramLocationID
      }
      if ("SampleDate" %in% current_cols) {
        result_df$Activity.Start.Date.Time <- df$SampleDate
      }
      if ("ParameterName" %in% current_cols) {
        result_df$DEP.Analyte.Name <- df$ParameterName
      }
      if ("ResultValue" %in% current_cols) {
        result_df$DEP.Result.Value.Number <- df$ResultValue
      }
      if ("ParameterUnits" %in% current_cols) {
        result_df$DEP.Result.Unit <- df$ParameterUnits
      }
    } else if (source_type == "excel") {
      # Only add columns if they exist in the source data
      if ("CLIENT SAMPLE ID" %in% current_cols) {
        result_df$Monitoring.Location.ID <- df$`CLIENT SAMPLE ID`
      }
      if ("COLLECTED" %in% current_cols) {
        result_df$Activity.Start.Date.Time <- df$COLLECTED
      }
      if ("ANALYTE" %in% current_cols) {
        result_df$DEP.Analyte.Name <- df$ANALYTE
      }
      if ("SAMPLE RESULT" %in% current_cols) {
        result_df$DEP.Result.Value.Number <- df$`SAMPLE RESULT`
      }
      if ("UNITS" %in% current_cols) {
        result_df$DEP.Result.Unit <- df$UNITS
      }
    }
    
    # Add source identifier
    result_df$data_source <- source_type
    
    # Add additional metadata columns that might be useful for analysis
    # Copy over source file info if it exists
    if ("source_file" %in% current_cols) {
      result_df$source_file <- df$source_file
    }
    
    # Remove the temporary row_id column
    result_df$row_id <- NULL
    
    # Check if we've actually found any useful data
    if (ncol(result_df) <= 1) {  # If only data_source column exists
      cat(glue("No usable columns found in {source_type} data\n"))
      return(data.frame())  # Return empty dataframe
    }
    
    cat(glue("Successfully converted {source_type} data to {ncol(result_df)} standardized columns\n"))
    return(result_df)
  }
  
  # Apply standardization to both datasets
  historical_standardized <- standardize_columns(historical_df, "historical")
  excel_standardized <- standardize_columns(excel_df, "excel")
  
  # Combine standardized datasets
  if (nrow(historical_standardized) > 0 && nrow(excel_standardized) > 0) {
    merged_df <- dplyr::bind_rows(historical_standardized, excel_standardized)
    cat(glue("Merged dataframe has {nrow(merged_df)} rows and {ncol(merged_df)} columns\n"))
  } else if (nrow(historical_standardized) > 0) {
    merged_df <- historical_standardized
    cat("Using only historical data\n")
  } else if (nrow(excel_standardized) > 0) {
    merged_df <- excel_standardized
    cat("Using only Excel data\n")
  } else {
    cat("No data found. Returning empty dataframe.\n")
    return(data.frame())
  }
  
  # Add program identifier and standardize data types
  merged_df <- merged_df %>%
    dplyr::mutate(
      # Convert date/time if needed
      Activity.Start.Date.Time = as.character(Activity.Start.Date.Time),
      # Convert result value to numeric
      DEP.Result.Value.Number = as.numeric(as.character(DEP.Result.Value.Number)),
      # Add program identifier
      program = "MIAMIBEACH"
    )
  
  cat(glue("Final Miami Beach dataset has {nrow(merged_df)} rows for {length(unique(merged_df$Monitoring.Location.ID))} monitoring locations\n"))
  cat("================================================\n")
  
  return(merged_df)
}