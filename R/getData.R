# Required packages
# Adding explicit imports at the top to ensure they're loaded
library(dplyr)
library(here)
library(glue)

# Import the functions from the new module files
source(here::here("R/getSFERData.R"))
source(here::here("R/getSTORETData.R"))
source(here::here("R/getWINData.R"))

# Main function to get data for a specific program
getData <- function(programName) {
  # Determine which data source to use based on program name
  if(programName == "SFER") {
    df <- getSFERData(programName)
  } else if (programName %in% c("BROWARD", "DERM_BBWQ", "PALMBEACH")) {
    # For programs with both WIN and historical data
    df <- getWINData(programName)
    
    # load & append historical STORET data
    hist_data <- getSTORETData(programName)
    
    # Ensure consistent data types before binding rows
    df <- mergeWithHistoricalData(df, hist_data)
  } else if (programName == "test") {
    # Special case for testing with example files
    df <- getWINData(programName)
    
    # Load test STORET data
    hist_data <- getSTORETData(programName)
    
    # Ensure consistent data types before binding rows
    df <- mergeWithHistoricalData(df, hist_data)
  } else {
    # Default case - use WIN data
    df <- getWINData(programName)
  }
  # Process DMS coordinates and return the dataframe
  df <- processDMSCoordinates(df)
  
  return(df)
}

# Merge WIN data with historical STORET data, ensuring consistent types
mergeWithHistoricalData <- function(df, hist_data) {
  # === coerce column types where necessary           
  # Make sure DEP.Result.Value.Number is consistently numeric in both dataframes
  if ("DEP.Result.Value.Number" %in% names(df) && "DEP.Result.Value.Number" %in% names(hist_data)) {
    # Convert to numeric, handling potential conversion issues with as.numeric
    df$DEP.Result.Value.Number <- as.numeric(as.character(df$DEP.Result.Value.Number))
    hist_data$DEP.Result.Value.Number <- as.numeric(as.character(hist_data$DEP.Result.Value.Number))
  }
  
  # Ensure consistent character type for ID columns
  df$Monitoring.Location.ID <- as.character(df$Monitoring.Location.ID)
  hist_data$Monitoring.Location.ID <- as.character(hist_data$Monitoring.Location.ID)
  
  # Handle WBID column type inconsistency (character vs integer)
  if ("WBID" %in% names(df) && "WBID" %in% names(hist_data)) {
    df$WBID <- as.character(df$WBID)
    hist_data$WBID <- as.character(hist_data$WBID)
  }
  
  # Check for other common columns and ensure they have consistent types
  common_cols <- intersect(names(df), names(hist_data))
  for (col in common_cols) {
    if (!identical(class(df[[col]]), class(hist_data[[col]]))) {
      # Convert to character type for string columns
      if (is.character(df[[col]]) || is.character(hist_data[[col]])) {
        df[[col]] <- as.character(df[[col]])
        hist_data[[col]] <- as.character(hist_data[[col]])
      }
      # Convert to numeric for number columns
      else if (is.numeric(df[[col]]) || is.numeric(hist_data[[col]])) {
        df[[col]] <- as.numeric(as.character(df[[col]]))
        hist_data[[col]] <- as.numeric(as.character(hist_data[[col]]))
      }
      # Default to character for any other mismatches
      else {
        df[[col]] <- as.character(df[[col]])
        hist_data[[col]] <- as.character(hist_data[[col]])
      }
    }
  }
  
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