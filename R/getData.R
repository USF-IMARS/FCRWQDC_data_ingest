# Required packages
# Adding explicit imports at the top to ensure they're loaded
library(dplyr)
library(here)
library(glue)

# Import the functions from the new module files
source(here::here("R/getSFERData.R"))
source(here::here("R/getSTORETData.R"))
source(here::here("R/getWINData.R"))
source(here::here("R/getMiamiBeachData.R"))

# Main function to get data for a specific program
getData <- function(programName) {
  # cat("\n===========================================\n")
  # cat(glue("DATA LOADING FOR PROGRAM: {toupper(programName)} \n"))
  # cat("===========================================\n")
  
  # Determine which data source to use based on program name
  if(programName == "SFER") {
    df <- getSFERData(programName)
    
    # cat("\n--- Data Summary ---\n")
    # cat(glue("Total rows: {nrow(df)}\n"))
    # cat(glue("Total columns: {ncol(df)}\n"))
    # cat("------------------\n")
  
  } else if (programName %in% c("BROWARD", "DERM_BBWQ", "PALMBEACH")) {
    # For programs with both WIN and historical data
    # cat("\n--- Loading WIN Data ---\n")
    df <- getWINData(programName) %>%
      mutate(
        Monitoring.Location.ID = as.character(Monitoring.Location.ID)
      )
    
    # load & append historical STORET data
    # cat("\n--- Loading Historical STORET Data ---\n")
    # NOTE: this method is for loading "old" STORET data format
    # hist_data <- getSTORETData(programName)

    # To load "new" STORET data format:
    hist_data <- read.csv(here::here(
      glue("data/STORET_historical/{programName}_STORET_ALL.csv")))
    # align hist_data to WIN format
    hist_data <- hist_data %>% mutate(
      Organization.ID = programName,
      Sampling.Agency.Name = programName,
      Monitoring.Location.ID = as.character(Station),
      Activity.Start.Date.Time = Date,
      # special exception for DERM_BBWQ (missing depth)
      Activity.Depth = if ("Depth" %in% colnames(.)) .data$Depth else NA_real_,
      DEP.Analyte.Name = Parameter,
      DEP.Result.Value.Number = Value,
      DEP.Result.Unit = Unit,
      Value.Qualifier = VQ,
      .keep = "none"  # "unused"
    )

    # Ensure consistent data types before binding rows
    # cat("\n--- Merging Data Sources ---\n")
    df <- mergeWithHistoricalData(df, hist_data)
    
    # cat("\n--- Merged Data Summary ---\n")
    # cat(glue("WIN records: {win_rows} rows, {win_cols} columns\n"))
    # cat(glue("STORET records: {hist_rows} rows, {hist_cols} columns\n"))
    # cat(glue("Total after merging: {merged_rows} rows, {merged_cols} columns\n"))
    # cat("-------------------------\n")
    
  } else if (programName == "MiamiBeach") {
    df <- getMiamiBeachData("MiamiBeach")
  } else if (programName == "FIU") {
    df <- getWINData("FIU")
    df2 <- getFIUData("FIU")
    # combine dataframes
    df <- bind_rows(df, df2)
  } else {
    # Default case - use WIN data
    df <- getWINData(programName)
    
    # cat("\n--- Data Summary ---\n")
    # cat(glue("Total rows: {nrow(df)}\n"))
    # cat(glue("Total columns: {ncol(df)}\n"))
    # cat("------------------\n")
  }
  
  # Process DMS coordinates and return the dataframe
  # cat("\n--- Processing DMS Coordinates ---\n")
  original_rows <- nrow(df)
  df <- processDMSCoordinates(df)
  if (nrow(df) != original_rows) {
    # cat(glue("WARNING: Row count changed during DMS coordinate processing from {original_rows} to {nrow(df)}\n"))
  } else {
    # cat("No change in row count during DMS coordinate processing\n")
  }
  
  # Ensure consistent column types to prevent binding issues
  # cat("\n--- Standardizing Column Types ---\n")
  # Convert DEP.Result.ID to character if it exists
  if ("DEP.Result.ID" %in% names(df)) {
    df$DEP.Result.ID <- as.character(df$DEP.Result.ID)
    # cat("Converted DEP.Result.ID to character type\n")
  }
  
  # Convert other potential problematic columns to standardized types
  type_standardization <- list(
    # Column name = function to apply
    # Character columns
    "Activity.ID" = as.character,
    "WBID" = as.character,
    "Organization.ID" = as.character,
    "Lab.ID" = as.character,
    "Sample.Collection.Type" = as.character,
    
    # Numeric columns - need safe conversion
    "DEP.Result.Value.Number" = function(x) as.numeric(as.character(x))
  )
  
  # Apply type standardization to all columns that exist in the dataframe
  for (col in names(type_standardization)) {
    if (col %in% names(df)) {
      df[[col]] <- type_standardization[[col]](df[[col]])
      # cat(glue("Converted {col} to standardized type\n"))
    }
  }

  # preserve original analyte names
  df$original.analyte.name <- df$DEP.Analyte.Name

  # replace spaces, parentheses, commas in DEP.Analyte.Name
  df$DEP.Analyte.Name <- gsub("[ \\(\\)|,]", "_", df$DEP.Analyte.Name)


  # === combine synonymous analytes
  df <- df %>% 
  mutate(
    DEP.Analyte.Name = case_when(
    original.analyte.name == "Ammonium"                                    ~ "Ammonium",

    original.analyte.name == "Ammonia__N_"                                 ~ "Ammonia",

    original.analyte.name == "Chlorophyll_a"                               ~ "Chlorophyll_a",
    original.analyte.name == "Chlorophyll_a-_uncorrected"                  ~ "Chlorophyll_a",
    original.analyte.name == "Chlorophyll_a__uncorrected_for_pheophytin"   ~ "Chlorophyll_a"
    original.analyte.name == "Chlorophyll_a__free_of_pheophytin"           ~ "Chlorophyll_a",
    original.analyte.name == "Chlorophyll_a-_corrected"                    ~ "Chlorophyll_a",
    original.analyte.name == "Chlorophyll_a__corrected_for_pheophytin"     ~ "Chlorophyll_a",

    original.analyte.name == "Dissolved_oxygen__DO_"                       ~ "Dissolved_Oxygen",
    original.analyte.name == "Oxygen__Dissolved"                           ~ "Dissolved_Oxygen",
    original.analyte.name == "Dissolved_Oxygen__Discrete_"                 ~ "Dissolved_Oxygen",
    original.analyte.name == "Dissolved_Oxygen__CTD_"                      ~ "Dissolved_Oxygen",
    original.analyte.name == "Dissolved_Oxygen"                            ~ "Dissolved_Oxygen",

    original.analyte.name == "Specific_Conductivity"                       ~ "Specific_Conductivity",
    original.analyte.name == "Field_Specific_Conductance"                  ~ "Specific_Conductivity",

    original.analyte.name == "Fecal_Coliforms"                             ~ "Fecal_Coliforms",

    original.analyte.name == "Enterococci"                                 ~ "Enterococci",

    original.analyte.name == "Nitrogen__Ammonia"                           ~ "Nitrogen__Ammonia",
    
    original.analyte.name == "Nitrite"                                     ~ "Nitrite",
    original.analyte.name == "Nitrite__N_"                                 ~ "Nitrite",
    original.analyte.name == "Nitrogen__Nitrite__NO2__as_N"                ~ "Nitrite",
    original.analyte.name == "Nitrogen__Nitrite__NO2__as_NO2"              ~ "Nitrite",

    original.analyte.name == "Nitrate"                                     ~ "Nitrate",
    original.analyte.name == "Nitrate__N_"                                 ~ "Nitrate",
    original.analyte.name == "Nitrogen__Nitrate__NO3__as_N"                ~ "Nitrate",
    original.analyte.name == "Nitrogen__Nitrate__NO3__as_NO3"              ~ "Nitrate",
    
    original.analyte.name == "NO2+3__Filtered"                             ~ "Nitrate+Nitrite",
    original.analyte.name == "Nitrate+Nitrite"                             ~ "Nitrate+Nitrite",
    original.analyte.name == "Nitrogen__Nitrite__NO2__+_Nitrate__NO3__as_N"~ "Nitrate+Nitrite",
    original.analyte.name == "Nitrate-Nitrite__N_"                         ~ "Nitrate+Nitrite",
    original.analyte.name == "Nitrogen__NO2_plus_NO3"                      ~ "Nitrate+Nitrite",
    
    original.analyte.name == "Nitrogen__ammonia__NH3__+_ammonium__NH4_"    ~ "Nitrogen__ammonia__NH3__+_ammonium__NH4_",

    original.analyte.name == "Nitrogen__ammonia_as_N"                      ~ "Nitrogen__ammonia_as_N",

    original.analyte.name == "Nitrogen__ammonia__NH3__as_NH3"              ~ "Nitrogen__ammonia__NH3__as_NH3",

    original.analyte.name == "Nitrogen-_Total"                             ~ "Total_Nitrogen",

    original.analyte.name == "Orthophosphate__P_"                          ~ "Orthophosphate",
    original.analyte.name == "Phosphorus__orthophosphate_as_P"             ~ "Orthophosphate",
    original.analyte.name == "Phosphate"                                   ~ "Orthophosphate",

    original.analyte.name == "Phosphorus__Total__as_P__LL"                 ~ "Phosphorus",
    original.analyte.name == "Total_Phosphorus"                            ~ "Phosphorus",
    original.analyte.name == "Phosphorus-_Total"                           ~ "Phosphorus",
    original.analyte.name == "Phosphorus_as_P"                             ~ "Phosphorus",

    original.analyte.name == "Pheophytin"                                  ~ "Pheophytin",

    original.analyte.name == "Field_pH"                                    ~ "pH",
    original.analyte.name == "pH"                                          ~ "pH",

    original.analyte.name == "Salinity"                                    ~ "Salinity",

    original.analyte.name == "Silica__SiO2_"                               ~ "Silicate",
    original.analyte.name == "Silicate"                                    ~ "Silicate",

    original.analyte.name == "Temperature"                                 ~ "Temperature",
    original.analyte.name == "Temperature__Water"                          ~ "Temperature",
    original.analyte.name == "Temperature__water"                          ~ "Temperature",
    original.analyte.name == "Field_Temperature"                           ~ "Temperature",
    original.analyte.name == "Water_Temperature"                           ~ "Temperature",

    original.analyte.name == "Total_Nitrogen"                              ~ "Total_Nitrogen",

    original.analyte.name == "Total_Kjeldahl_Nitrogen"                     ~ "Total_Kjeldahl_Nitrogen",
    original.analyte.name == "Nitrogen__Kjeldahl"                          ~ "Total_Kjeldahl_Nitrogen",
    original.analyte.name == "Nitrogen-_Total_Kjeldahl"                    ~ "Total_Kjeldahl_Nitrogen",
    original.analyte.name == "Nitrogen__Kjeldahl__Total"                   ~ "Total_Kjeldahl_Nitrogen",

    original.analyte.name == "Turbidity"                                   ~ "Turbidity",
    TRUE ~ original.analyte.name
    )
  )

  # convert all analyte values to mg/L using DEP.Result.Unit
  convertUMolToMgPerL <- function(x, analyte) {
    if (analyte == "Nitrite") {
      return(x * 0.0461)
    } else if (analyte == "Nitrate") {
      return(x * 0.0620)
    } else if (analyte == "Nitrate+Nitrite") {
      return(x * 0.108)
    } else if (analyte == "Ammonium") {
      return(x * 0.018)
    } else if (analyte == "Phosphate") {
      return(x * 0.095)
    } else if (analyte == "Phosphorus") {
      return(x * 0.031)
    } else if (analyte == "Silicate") {
      return(x * 0.0601)
    }
  }

  df <- df %>% 
    mutate(
      DEP.Result.Value.Number = case_when(
        DEP.Result.Unit == "mg/L" ~ DEP.Result.Value.Number,
        DEP.Result.Unit == "ppm" ~ DEP.Result.Value.Number * 1000,
        DEP.Result.Unit == "mg/m3" ~ DEP.Result.Value.Number / 1000,
        DEP.Result.Unit == "umol/L" ~ convertUMolToMgPerL(
          DEP.Result.Value.Number, DEP.Analyte.Name),
        TRUE ~ DEP.Result.Value.Number
      )
    )


  # cat("===========================================\n")
  return(df)
}


# NOTE: this function is for use with "old" STORET data format
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
  
  if ("Activity.Depth" %in% names(df)) {
    # drop rows with depth > 1m
    df <- filter(df, Activity.Depth <= 1)
  }

  return(df)
}