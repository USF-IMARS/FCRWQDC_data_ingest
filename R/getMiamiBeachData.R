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
  
  cat("\n=== Processing Miami Beach Water Quality Data ===\n")
  
  # Define column mappings for different data sources
  # historical columns:
  # RowID|ProgramID|ProgramName|Habitat|IndicatorID|IndicatorName|
  #ParameterID|ParameterName|ParameterUnits|ProgramLocationID|AreaID|
  #ManagedAreaName|Region|ActivityType|SampleDate|ResultValue|Year|Month|
  #ActivityDepth_m|RelativeDepth|TotalDepth_m|MDL|PQL|DetectionUnit|
  #ValueQualifier|ValueQualifierSource|SampleFraction|ResultComments|
  #OriginalLatitude|OriginalLongitude|SEACAR_QAQCFlagCode|
  #SEACAR_QAQC_Description|Include|SEACAR_EventID|MADup|ExportVersion
  column_mappings <- list(
    historical = c(
      Monitoring.Location.ID = "ProgramLocationID",
      Activity.Start.Date.Time = "SampleDate",
      DEP.Analyte.Name = "ParameterName",
      DEP.Result.Value.Number = "ResultValue",
      DEP.Result.Unit = "ParameterUnits",
      Org.Decimal.Latitude = "OriginalLatitude",
      Org.Decimal.Longitude = "OriginalLongitude"
    ),
    excel = c(
      Monitoring.Location.ID = "CLIENT SAMPLE ID",
      Activity.Start.Date.Time = "SAMPLE COLLECTION DATE",
      DEP.Analyte.Name = "ANALYTE", 
      DEP.Result.Value.Number = "SAMPLE RESULT",
      DEP.Result.Unit = "UNITS"
      # NOTE: excel files do not have lat,lon !
    )
  )
  
  # Function to load historical data
  load_historical_data <- function(file_path) {
    if (!file.exists(file_path)) {
      cat("Historical data file not found. Starting with empty dataframe.\n")
      return(data.frame())
    }
    
    # Read the pipe-delimited file
    df <- read.delim(
      file = file_path,
      sep = "|",  # Pipe delimiter
      header = TRUE,
      stringsAsFactors = FALSE,
      na.strings = c("", "NA")
    )
    
    cat(glue("Loaded {nrow(df)} rows and {ncol(df)} columns from historical file\n"))
    return(df)
  }
  
  # Function to find Excel files
  find_excel_files <- function(directory) {
    files <- list.files(
      path = directory,
      pattern = "\\.xls$|\\.xlsx$",
      recursive = TRUE,
      full.names = TRUE
    )
    cat(glue("Found {length(files)} Excel files\n"))
    return(files)
  }
  
  # Function to extract date from filename
  extract_date_from_filename <- function(filename) {
    patterns <- c(
      "Results ([0-9\\-]+)",
      "([0-9\\-]+) Data\\.xls"
    )
    
    for (pattern in patterns) {
      date_match <- regexpr(pattern, filename)
      if (date_match > 0) {
        extracted <- regmatches(filename, date_match)
        # Extract the date portion from the match
        return(gsub(pattern, "\\1", extracted))
      }
    }
    return(NA)
  }
  
  # Function to read an Excel file and process it
  read_excel_file <- function(file_path) {
    cat(glue("Processing {basename(file_path)}\n"))
    
    # Extract date from filename
    file_date <- extract_date_from_filename(basename(file_path))
    
    tryCatch({
      # Read the Excel file
      df <- readxl::read_excel(file_path, sheet = 1)
      
      # Add source file info
      df$source_file <- basename(file_path)
      
      # If file doesn't have date column, use filename date
      if (!"SAMPLE COLLECTION DATE" %in% names(df) && !is.na(file_date)) {
        df$`SAMPLE COLLECTION DATE` <- file_date
      }
      
      return(df)
    }, error = function(e) {
      cat(glue("Error reading {basename(file_path)}: {e$message}\n"))
      return(data.frame())
    })
  }
  
  # Load historical data
  historical_file <- here("data/MiamiBeach/Discrete WQ - 4058.txt")
  cat(glue("Loading historical data from {basename(historical_file)}\n"))
  historical_df <- load_historical_data(historical_file)
  
  # Find and load Excel files
  xls_directory <- here("data/MiamiBeach/2024")
  cat(glue("Searching for Excel files in {xls_directory}\n"))
  excel_files <- find_excel_files(xls_directory)
  
  # Process all Excel files and combine them
  if (length(excel_files) > 0) {
    excel_data_list <- lapply(excel_files, read_excel_file)
    excel_df <- dplyr::bind_rows(excel_data_list)
    cat(glue("Combined {nrow(excel_df)} rows from Excel files\n"))
  } else {
    excel_df <- data.frame()
    cat("No Excel files found\n")
  }
  
  # Function to map columns while preserving original columns
  map_columns <- function(df, source_type) {
    # First check if dataframe is empty
    if (nrow(df) == 0) {
      cat(glue("No data found for {source_type}\n"))
      return(data.frame())
    }
    
    # Get current column names and mapping for this source type
    current_cols <- names(df)
    mapping <- column_mappings[[source_type]]
    
    cat(glue("Column names in {source_type} data: {paste(current_cols, collapse=', ')}\n"))
    
    # Create a result dataframe with all original columns
    result_df <- df
    
    # Add standardized columns by copying from original columns
    for (target_col in names(mapping)) {
      source_col <- mapping[target_col]
      if (source_col %in% current_cols) {
        # Copy the column data to the new standardized column name
        result_df[[target_col]] <- result_df[[source_col]]
      }
    }
    
    # Add source identifier
    result_df$data_source <- source_type
    
    # Check if we've actually found any useful data
    required_cols <- c("Monitoring.Location.ID", "Activity.Start.Date.Time", "DEP.Analyte.Name", "DEP.Result.Value.Number")
    missing_cols <- required_cols[!required_cols %in% names(result_df)]
    
    if (length(missing_cols) > 0) {
      cat(glue("Missing required columns in {source_type} data: {paste(missing_cols, collapse=', ')}\n"))
      cat(glue("Available columns: {paste(names(result_df), collapse=', ')}\n"))
    } else {
      cat(glue("Successfully converted {source_type} data to standard format\n"))
    }
    
    return(result_df)
  }
  
  # Apply column mapping to both datasets (preserving original columns)
  historical_standardized <- map_columns(historical_df, "historical")
  excel_standardized <- map_columns(excel_df, "excel")
  
  # Convert data types to ensure compatibility when merging
  prepare_for_merge <- function(df) {
    if (nrow(df) == 0) return(df)
    
    # First convert all columns that should be numeric to character
    # This is to handle cases where some values might not be convertible to numeric
    if ("DEP.Result.Value.Number" %in% names(df)) {
      df$DEP.Result.Value.Number <- as.character(df$DEP.Result.Value.Number)
    }
    
    # Ensure all columns that need to match between datasets have the same type
    column_types <- list(
      Monitoring.Location.ID = as.character,
      Activity.Start.Date.Time = as.character,
      DEP.Analyte.Name = as.character,
      DEP.Result.Unit = as.character
    )
    
    # Apply the type conversions
    for (col in names(column_types)) {
      if (col %in% names(df)) {
        df[[col]] <- column_types[[col]](df[[col]])
      }
    }
    
    # Now convert numeric columns to numeric after ensuring all are character first
    if ("DEP.Result.Value.Number" %in% names(df)) {
      df$DEP.Result.Value.Number <- as.numeric(df$DEP.Result.Value.Number)
      # Handle conversion failures by replacing NAs with 0
      na_count <- sum(is.na(df$DEP.Result.Value.Number))
      if (na_count > 0) {
        cat(glue("Warning: {na_count} values could not be converted to numeric and were set to NA\n"))
      }
    }
    
    return(df)
  }
  
  # Apply type conversions to both datasets
  historical_standardized <- prepare_for_merge(historical_standardized)
  excel_standardized <- prepare_for_merge(excel_standardized)
  
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
      Activity.Start.Date.Time = as.character(.data$Activity.Start.Date.Time),
      # Convert result value to numeric
      DEP.Result.Value.Number = as.numeric(as.character(.data$DEP.Result.Value.Number)),
      # Add program identifier
      program = "MIAMIBEACH"
    )
  
  cat(glue("Final Miami Beach dataset has {nrow(merged_df)} rows for {length(unique(merged_df$Monitoring.Location.ID))} monitoring locations\n"))
  cat("================================================\n")
  
  return(merged_df)
}