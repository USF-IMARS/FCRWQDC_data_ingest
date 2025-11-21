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
source(here::here("R/getFIURecentData.R"))
source(here::here("R/getFIUHistoricalData.R"))
source(here::here("R/getFIUEstuariesData.R"))
source(here::here("R/getBBWWData.R"))
source(here::here("R/getFBBBData.R"))

source(here::here("R/getListOfAnalytes.R"))

# Main function to get data for a specific program
getData <- function(programName) {
  print(glue("=== LOADING PROVIDER : {programName}...\n"))

  # Determine which data source to use based on program name
  if(programName == "SFER") {
    df <- getSFERData(programName)
  } else if (programName %in% c("BROWARD_STORET", "DERM_BBWQ_STORET", "PALMBEACH_STORET")) {
    df <- getSTORETData(programName)
  } else if (programName == "MiamiBeach") {
    df <- getMiamiBeachData(programName)
  } else if (programName == "FIU_WQMP_RECENT") {
    df <- getFIURecentData()
  } else if (programName == "FIU_WQMP_HISTORICAL") {
    df <- getFIUHistoricalData()
  } else if(programName == "FIU_Estuaries"){
    df <- getFIUEstuariesData()
  } else if (programName == "BBWW"){
    df <- getBBWWData()
  } else if (programName == "AOML_FBBB"){
    df <- getFBBBData()
  } else {
    # Default case - use WIN data
    df <- getWINData(programName)
  }
  
  # set SEACAR `ProgramID`s
  program_ids <- c(
    AOML_FBBB = NA,
    BBAP = 5026,
    BBWW = 4057,
    BROWARD = 10013,
    BROWARD_STORET = NA,
    DEP = 5033,
    DERM_BBWQ = 4018,
    DERM_BBWQ_STORET = NA,
    FIU_Estuaries = 509,
    FIU_WQMP = 297,
    FIU_WQMP_HISTORICAL = NA,
    FIU_WQMP_RECENT = NA,
    MiamiBeach = 4058,
    PALMBEACH = 10012,
    PALMBEACH_STORET = NA,
    SFER = 3
  )
  df$ProgramID <- program_ids[[programName]]
    
  # set any literal "NULL" text strings to na
  df <- df %>%
    mutate(across(where(~ is.character(.) || is.factor(.)), ~ na_if(as.character(.), "NULL")))
  
  
  # Process DMS coordinates and return the dataframe
  # cat("\n--- Processing DMS Coordinates ---\n")
  # original_rows <- nrow(df)
  df <- processDMSCoordinates(df)
  # if (nrow(df) != original_rows) {
  #   cat(glue("WARNING: Row count changed during DMS coordinate processing from {original_rows} to {nrow(df)}\n"))
  # } else {
  #   cat("No change in row count during DMS coordinate processing\n")
  # }
  
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
    "Monitoring.Location.ID" = as.character,
    "Station" = as.character,
    "DEP.Result.Unit" = as.character,
    "RowID" = as.character,
    "ProgramID" = as.character,
    "IndicatorID" = as.character,
    "ParameterID" = as.character,
    "AreaID" = as.character,
    "Include" = as.character,
    "MADup" = as.character,
    
    # Numeric columns - need safe conversion
    "DEP.Result.Value.Number" = function(x) as.numeric(as.character(x)),
    "Activity.Depth" = function(x) as.numeric(as.character(x)),
    "Org.Decimal.Latitude" = function(x) as.numeric(as.character(x)),
    "Org.Decimal.Longitude" = function(x) as.numeric(as.character(x)),
    
    "Activity.Start.Date.Time" = as.Date
  )
  
  # Apply type standardization to all columns that exist in the dataframe
  for (col in names(type_standardization)) {
    if (col %in% names(df)) {
      df[[col]] <- type_standardization[[col]](df[[col]])
      # cat(glue("Converted {col} to standardized type\n"))
    }
  }

  # # preserve original analyte names
  # df$original.analyte.name <- df$DEP.Analyte.Name
  # 
  # # === map analytes to SEACAR standard vocabulary
  # df <- df %>% 
  # mutate(DEP.Analyte.Name = case_when(
  #   #original.analyte.name == "% Xmis" ~ "
  #   #original.analyte.name == "Alkaline_Phosphatase_Activity" ~ "
  #   #original.analyte.name == "Ammonia" ~ "
  #   #original.analyte.name == "Ammonia (N)" ~ "
  #   #original.analyte.name == "CDOM QSU" ~ "
  #   original.analyte.name == "Chlorophyll a, corrected for pheophytin" ~ "Chlorophyll a, Corrected for Pheophytin",
  #   original.analyte.name == "Chlorophyll a, free of pheophytin" ~ "Chlorophyll a, Corrected for Pheophytin",
  #   original.analyte.name == "Chlorophyll a, uncorrected for pheophytin" ~ "Chlorophyll a, Uncorrected for Pheophytin",
  #   original.analyte.name == "Chlorophyll a- corrected" ~ "Chlorophyll a, Corrected for Pheophytin",
  #   original.analyte.name == "Chlorophyll a- uncorrected" ~ "Chlorophyll a, Uncorrected for Pheophytin",
  #   #original.analyte.name == "DIC" ~ "
  #   original.analyte.name == "DIN" ~ "Dissolved_Inorganic_Nitrogen",
  #   #original.analyte.name == "DOP" ~ "
  #   #original.analyte.name == "DO_B" ~ "
  #   #original.analyte.name == "Diffuse_Attenuation_Coefficient" ~ "
  #   original.analyte.name == "Dissolved oxygen (DO)" ~ "Dissolved Oxygen",
  #   #original.analyte.name == "Dissolved_Inorganic_Nitrogen" ~ "
  #   original.analyte.name == "Dissolved_Oxygen_CTD" ~ "Dissolved Oxygen",
  #   #original.analyte.name == "Enterococci" ~ "
  #   #original.analyte.name == "Fecal Coliforms" ~ "
  #   #original.analyte.name == "Field Specific Conductance" ~ "
  #   original.analyte.name == "Field Temperature" ~ "Water Temperature",
  #   original.analyte.name == "Field pH" ~ "pH",
  #   #original.analyte.name == "Kt" ~ "
  #   original.analyte.name == "NO2" ~ "Nitrite (NO2)",
  #   original.analyte.name == "Nitrate (N)" ~ "Nitrate (NO3)",
  #   #original.analyte.name == "Nitrate-Nitrite (N)" ~ "
  #   #original.analyte.name == "Nitrate.Nitrite" ~ "
  #   original.analyte.name == "Nitrite (N)" ~ "Nitrite (NO2)",
  #   #original.analyte.name == "Nitrogen, Ammonia" ~ "
  #   original.analyte.name == "Nitrogen, Kjeldahl" ~ "Total Kjeldahl Nitrogen",
  #   original.analyte.name == "Nitrogen, Kjeldahl, Total" ~ "Total Kjeldahl Nitrogen",
  #   #original.analyte.name == "Nitrogen, NO2 plus NO3" ~ "
  #   #original.analyte.name == "Nitrogen, Nitrate (NO3) as N" ~ "
  #   original.analyte.name == "Nitrogen, Nitrate (NO3) as NO3" ~ "Nitrate (NO3)",
  #   #original.analyte.name == "Nitrogen, Nitrite (NO2) + Nitrate (NO3) as N" ~ ",
  #   #original.analyte.name == "Nitrogen, Nitrite (NO2) as N" ~ "
  #   original.analyte.name == "Nitrogen, Nitrite (NO2) as NO2" ~ "Nitrite (NO2)",
  #   #original.analyte.name == "Nitrogen, ammonia (NH3) + ammonium (NH4)" ~ "
  #   original.analyte.name == "Nitrogen, ammonia (NH3) as NH3" ~ "Ammonia, Un-ionized (NH3)",
  #   #original.analyte.name == "Nitrogen, ammonia as N" ~ "
  #   original.analyte.name == "Nitrogen- Total" ~ "Total Nitrogen",
  #   original.analyte.name == "Nitrogen- Total Kjeldahl" ~ "Total Kjeldahl Nitrogen",
  #   original.analyte.name == "Orthophosphate (P)" ~ "Phosphate, Filtered (PO4)",
  #   original.analyte.name == "Oxygen, Dissolved" ~ "Dissolved Oxygen",
  #   original.analyte.name == "Phosphate" ~ "Phosphate, Filtered (PO4)",
  #   #original.analyte.name == "Phosphorus as P" ~ "
  #   original.analyte.name == "Phosphorus as PO4" ~ "Phosphate, Filtered (PO4)",
  #   #original.analyte.name == "Phosphorus, Total (as P) LL" ~ "
  #   #original.analyte.name == "Phosphorus, orthophosphate as P" ~ "
  #   original.analyte.name == "Phosphorus, orthophosphate as PO4" ~ "Phosphate, Filtered (PO4)",
  #   original.analyte.name == "Phosphorus- Total" ~ "Phosphorus, Total (as P) LL",
  #   original.analyte.name == "Phosphorus- Total" ~ "Total Phosphorus",
  #   #original.analyte.name == "SAL_B" ~ "  # salinity bottom (or bottle?)
  #   #original.analyte.name == "Si" ~ "
  #   #original.analyte.name == "Silica (SiO2)" ~ "
  #   #original.analyte.name == "TEMP_B" ~ "  # temperature bottom (or bottle?)
  #   original.analyte.name == "TN" ~ "Total Nitrogen",
  #   original.analyte.name == "TP" ~ "Total Phosphorus",
  #   original.analyte.name == "Temperature, Water" ~ "Water Temperature",
  #   original.analyte.name == "Temperature, water" ~ "Water Temperature",
  #   #original.analyte.name == "Total_Organic_Carbon" ~ "
  #   #original.analyte.name == "Tripton" ~ "
  #   #original.analyte.name == "Zcol" ~ "
  #   #original.analyte.name == "Zp" ~ "
  #   
  #   
  #   TRUE ~ original.analyte.name
  # ))

  # convert all analyte values to mg/L using DEP.Result.Unit
  # TODO: skip this for pH
  # df <- df %>% 
  #   mutate(
  #     DEP.Result.Value.Number = case_when(
  #       DEP.Result.Unit == "mg/L" ~ DEP.Result.Value.Number,
  #       DEP.Result.Unit == "ppm" ~ DEP.Result.Value.Number * 1000,
  #       DEP.Result.Unit == "mg/m3" ~ DEP.Result.Value.Number / 1000,
  #       DEP.Result.Unit == "umol/L" ~ case_when(
  #         DEP.Analyte.Name == "Nitrite" ~ DEP.Result.Value.Number * 0.0461,
  #         DEP.Analyte.Name == "Nitrate" ~ DEP.Result.Value.Number * 0.0620, 
  #         DEP.Analyte.Name == "Nitrate+Nitrite" ~ DEP.Result.Value.Number * 0.108,
  #         DEP.Analyte.Name == "Ammonium" ~ DEP.Result.Value.Number * 0.018,
  #         DEP.Analyte.Name == "Orthophosphate" ~ DEP.Result.Value.Number * 0.095,
  #         DEP.Analyte.Name == "Phosphorus" ~ DEP.Result.Value.Number * 0.031,
  #         DEP.Analyte.Name == "Silicate" ~ DEP.Result.Value.Number * 0.0601,
  #         # TODO: other values (like "Ammonia") are not handled
  #         # TODO: the DEP.Result.Unit value is not changed. Use orig unit & orig value to retain.
  #         TRUE ~ DEP.Result.Value.Number
  #       ),
  #       TRUE ~ DEP.Result.Value.Number
  #     )
  #   )
    df$program <- programName 

  # prepend `20` to any two-digit years in column `Activity.Start.Date.Time`
  df$Activity.Start.Date.Time <- gsub(
    "^\\s*(\\d{2})-(\\d{2})-(\\d{2})(.*)$",
    "20\\1-\\2-\\3\\4",
    df$Activity.Start.Date.Time,
    perl = TRUE
  )
  
  if(any(is.na(df$Monitoring.Location.ID)) > 0){
    print("WARN - rows found with no location ID")
    # df %>%
    #   filter(is.na(Monitoring.Location.ID)) %>%
    #   print()
  }
  return(df)
}

# Process DMS coordinates to calculate decimal lat/lon when missing
processDMSCoordinates <- function(df) {
  # Function to convert DMS to decimal degrees
  dms_to_decimal <- function(dms_str) {
    # Skip if the string is empty or NA
    if(is.na(dms_str) || dms_str == NA) {
      return(NA)
    }
    
    # Parse the DMS string - expected format like: "26 5 7.1880" or "26° 5' 7.1880"
    # First, clean the string by removing degree, minute, and second symbols
    clean_str <- gsub("[°'\"]", NA, dms_str)
    
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
  missing_lat <- is.na(df$`Org.Decimal.Latitude`) | df$`Org.Decimal.Latitude` == NA
  missing_lon <- is.na(df$`Org.Decimal.Longitude`) | df$`Org.Decimal.Longitude` == ""
  
  # Only process rows that have missing decimal coordinates but have DMS values
  rows_to_process <- which(missing_lat & !is.na(df$`Org.Latitude..DD.MM.SS.SSSS.`) & df$`Org.Latitude..DD.MM.SS.SSSS.` != "")
  if(length(rows_to_process) > 0) {
    df$`Org.Decimal.Latitude`[rows_to_process] <- sapply(df$`Org.Latitude..DD.MM.SS.SSSS.`[rows_to_process], dms_to_decimal)
  }
  
  rows_to_process <- which(missing_lon & !is.na(df$`Org.Longitude..DD.MM.SS.SSSS.`) & df$`Org.Longitude..DD.MM.SS.SSSS.` != NA)
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
