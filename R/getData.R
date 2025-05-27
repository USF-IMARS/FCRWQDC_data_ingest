# Required packages
# Adding explicit imports at the top to ensure they're loaded
library(dplyr)
library(here)
library(glue)

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
  df <- mapSTORETToWIN(df)

  return(df)
}

mapSTORETToWIN <- function(df){
  # === maps STORET columns to WIN column names
  # WIN columns:
  #  Organization.ID Monitoring.Location.ID Org.Decimal.Latitude
  #  Org.Latitude..DD.MM.SS.SSSS. Org.Decimal.Longitude
  #  Org.Longitude..DD.MM.SS.SSSS.   WBID Activity.ID Activity.Type
  #  Activity.Start.Date.Time Matrix Sample.Collection.Type
  #  Sampling.Agency.Name Activity.Depth
  #  Activity.Depth.Unit Activity.Top.Depth Activity.Bottom.Depth
  #  Activity.Depth.Top.Bottom.Unit DEP.Result.ID   DEP.Analyte.Name
  #  DEP.Result.Value.Number DEP.Result.Value.Text DEP.Result.Unit DEP.MDL
  #  DEP.PQL Value.Qualifier Sample.Fraction Lab.ID Result.Comments
  #  Audit.Censored.Decisions 
  
  # STORET Columns:
  #  Org.ID Org.Name Station.ID Act.Date Act.Time
  #  Act.Type Act.Category Act.Depth Depth.Units Relative.Depth Characteristic
  #  Result.Value Result.Units   VQ Analysis.Date Analysis.Time Procedure.Name
  #  Comment MDL MDL.Units PQL Medium

  # Using standard evaluation with dplyr to avoid "no visible binding" lints
  df <- df %>%
    # 1) rename known STORET → WIN columns - using proper standard evaluation
    dplyr::mutate(
      Organization.ID            = .data[['Org.ID']],
      Sampling.Agency.Name       = .data[['Org.Name']],
      Monitoring.Location.ID     = .data[['Station.ID']],
      Activity.Type              = .data[['Act.Type']],
      Activity.Depth             = .data[['Act.Depth']],
      Activity.Depth.Unit        = .data[['Depth.Units']],
      DEP.Analyte.Name           = .data[['Characteristic']],
      DEP.Result.Value.Number    = .data[['Result.Value']],
      DEP.Result.Unit            = .data[['Result.Units']],
      DEP.MDL                    = .data[['MDL']],
      DEP.PQL                    = .data[['PQL']],
      Value.Qualifier            = .data[['VQ']],
      Result.Comments            = .data[['Comment']]
    )
  
  # Continue transformation, ensuring we capture the result
  df <- df %>%
    # 2) build the DateTime - using proper standard evaluation
    dplyr::mutate(
      Activity.Start.Date.Time = as.POSIXct(
        paste(.data[['Act.Date']], .data[['Act.Time']]),
        format = "%m/%d/%Y %H:%M:%S",
        tz     = "UTC"
      )
    )
  return(df)
}

getHistoricalData <- function(programName){
  storetPath <- here('data/STORET_historical')
  
  # read the main file
  fpath  <- glue('{storetPath}/STORET_Water_Quality_Results_{programName}.txt')
  df     <- STORETFileToDataFrame(fpath)
  
  if (programName == 'DERM_BBWQ'){
    # also include the 1970–1995 legacy file
    fpath2 <- glue('{storetPath}/STORET_Water_Quality_Results_{programName}_1970_1995.txt')
    df2    <- STORETFileToDataFrame(fpath2)
    
    # merge them by row
    df <- dplyr::bind_rows(df, df2)
  }
  return(df)
}

# Get data from SFER CSV format files
getSFERData <- function(programName) {
  fpath <- here("data/SFER_data.csv")
  df <- read.csv(fpath)
  # modify df to align with WIN standards
  source(here("R/align_sfer_df.R"))
  # Using explicit call to the function with the namespace to avoid lint errors
  df <- align_sfer_df(df)
  return(df)
}

# Get data from STORET historical format files (pipe-delimited)
getSTORETData <- function(programName) {
  storetPath <- here('data/STORET_historical')
  
  # read the main file
  fpath <- glue('{storetPath}/STORET_Water_Quality_Results_{programName}.txt')
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

# Main function to get data for a specific program
getData <- function(programName) {
  # Determine which data source to use based on program name
  if(programName == "SFER") {
    df <- getSFERData(programName)
  } else if (programName %in% c("BROWARD", "DERM_BBWQ", "PALMBEACH")) {
    # For programs with both WIN and historical data
    df <- loadWINData(programName)
    
    # load & append historical STORET data
    hist_data <- getSTORETData(programName)
    
    # Ensure consistent data types before binding rows
    df <- mergeWithHistoricalData(df, hist_data)
  } else {
    # Default case - use WIN data
    df <- loadWINData(programName)
  }
  # Process DMS coordinates and return the dataframe
  df <- processDMSCoordinates(df)
  
  return(df)
}

# Merge WIN data with historical STORET data, ensuring consistent types
mergeWithHistoricalData <- function(df, hist_data) {
  # === coerce column types where necessary         
  # Convert Activity.Start.Date.Time to POSIXct if it exists in both dataframes
  if ("Activity.Start.Date.Time" %in% names(df) && "Activity.Start.Date.Time" %in% names(hist_data)) {
    # Convert to character first if they are different types to ensure consistent conversion
    df$Activity.Start.Date.Time <- as.character(df$Activity.Start.Date.Time)
    hist_data$Activity.Start.Date.Time <- as.character(hist_data$Activity.Start.Date.Time)
    
    # Then convert both to POSIXct
    df$Activity.Start.Date.Time <- as.POSIXct(df$Activity.Start.Date.Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    hist_data$Activity.Start.Date.Time <- as.POSIXct(hist_data$Activity.Start.Date.Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }
  
  # Make sure DEP.Result.Value.Number is consistently numeric in both dataframes
  if ("DEP.Result.Value.Number" %in% names(df) && "DEP.Result.Value.Number" %in% names(hist_data)) {
    # Convert to numeric, handling potential conversion issues with as.numeric
    df$DEP.Result.Value.Number <- as.numeric(as.character(df$DEP.Result.Value.Number))
    hist_data$DEP.Result.Value.Number <- as.numeric(as.character(hist_data$DEP.Result.Value.Number))
  }
  
  # Convert both to character to ensure consistent type
  df$Monitoring.Location.ID <- as.character(df$Monitoring.Location.ID)
  hist_data$Monitoring.Location.ID <- as.character(hist_data$Monitoring.Location.ID)
  
  # Now bind the rows with compatible types
  merged_df <- dplyr::bind_rows(df, hist_data)
  return(merged_df)
}

# Process DMS coordinates to calculate decimal lat/lon when missing
processDMSCoordinates <- function(df) {
  # Function to convert DMS to decimal degrees
  dms_to_decimal <- function(dms_str) {
    # Skip if the string is empty or NA
    if(is.na(dms_str) || dms_str == "") {
      return(NA)
    }
    
    # Parse the DMS string - expected format like: "26 5 7.1880" or "26° 5' 7.1880\""
    # First, clean the string by removing degree, minute, and second symbols
    clean_str <- gsub("[°'\"]", "", dms_str)
    
    # Split by spaces
    parts <- strsplit(clean_str, "\\s+")[[1]]
    
    # Extract degrees, minutes, seconds
    if(length(parts) >= 3) {
      degrees <- as.numeric(parts[1])
      minutes <- as.numeric(parts[2])
      seconds <- as.numeric(parts[3])
      
      # Calculate decimal degrees: degrees + minutes/60 + seconds/3600
      decimal <- degrees + minutes/60 + seconds/3600
      
      # Apply negative sign for western longitudes or southern latitudes if needed
      # (typically handled by the sign in the degrees component)
      return(decimal)
    } else {
      # If the format doesn't match expectations, return NA
      return(NA)
    }
  }
  
  # Apply the conversion to rows with missing decimal coordinates
  missing_lat <- is.na(df$`Org.Decimal.Latitude`) | df$`Org.Decimal.Latitude` == ""
  missing_lon <- is.na(df$`Org.Decimal.Longitude`) | df$`Org.Decimal.Longitude` == ""
  
  # Only process rows that have missing decimal coordinates but have DMS values
  rows_to_process <- which(missing_lat & !is.na(df$`Org.Latitude..DD.MM.SS.SSSS.`) & df$`Org.Latitude..DD.MM.SS.SSSS.` != "")
  if(length(rows_to_process) > 0) {
    df$`Org.Decimal.Latitude`[rows_to_process] <- sapply(df$`Org.Latitude..DD.MM.SS.SSSS.`[rows_to_process], dms_to_decimal)
  }
  
  rows_to_process <- which(missing_lon & !is.na(df$`Org.Longitude..DD.MM.SS.SSSS.`) & df$`Org.Longitude..DD.MM.SS.SSSS.` != "")
  if(length(rows_to_process) > 0) {
    # Apply negative sign for western longitudes (in the western hemisphere)
    lon_decimals <- sapply(df$`Org.Longitude..DD.MM.SS.SSSS.`[rows_to_process], dms_to_decimal)
    # Make western longitudes negative if they aren't already
    lon_decimals <- ifelse(lon_decimals > 0 & grepl("^W|west", df$`Org.Longitude..DD.MM.SS.SSSS.`[rows_to_process], ignore.case = TRUE), 
                          -lon_decimals, lon_decimals)
    df$`Org.Decimal.Longitude`[rows_to_process] <- lon_decimals
  }
  
  # For debugging - print summary of conversion
  # cat("Converted", sum(missing_lat & !is.na(df$`Org.Decimal.Latitude`)), "latitude values from DMS to decimal format.\n")
  # cat("Converted", sum(missing_lon & !is.na(df$`Org.Decimal.Longitude`)), "longitude values from DMS to decimal format.\n")
  
  return(df)
}

loadWINData <- function(programName){
  # use standardized data from WIN:
  fpath <- here(glue("data/WIN/_WIN_WAVES_OTIS_{programName}.txt"))
  
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
  temp_row <- ""
  
  for (line in raw_data_lines) {
    # Start a new temporary row or append to the existing one
    temp_row <- if (temp_row == "") line else paste(temp_row, line, sep = "\n")
    
    # Count the number of pipe delimiters in temp_row
    n_delim <- length(gregexpr("\\|", temp_row)[[1]])
    
    # If the row has the expected number of delimiters (one less than columns), it's complete.
    if (n_delim == (expected_cols - 1)) {
      combined_rows <- c(combined_rows, temp_row)
      temp_row <- ""  # Reset for the next record
    }
  }
  
  # In case any data remains in temp_row, add it as a record
  if (temp_row != "") {
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
                   stringsAsFactors = FALSE)
  
  # Return the dataframe
  return(result_df)
}