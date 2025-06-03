#' Align column names in a dataframe to standardized SFER names
#'
#' This function takes a dataframe and renames its columns according to a
#' predefined mapping to ensure consistency with SFER naming conventions.
#'
#' @param df A dataframe with column names to be standardized
#' @return A dataframe with renamed columns
#' @export
#'
#' @examples
#' # Assuming df has columns like "latitude", "longitude", etc.
#' standardized_df <- align_sfer_df(df)
align_sfer_df <- function(df) {
  # Define column name mapping (original_name = standardized_name)
  column_mapping <- list(
    # ID and station information
    "keyfield" = "DEP.Result.ID",
    "cruise_id" = "Activity.ID",
    "station" = "Monitoring.Location.ID",
    "station_type" = "Activity.Type",
    
    # Date and time components
    "datetime" = "Activity.Start.Date.Time",
    # "year" = "Activity.Start.Date.Time", # Note: These will need additional processing
    # "month" = "Activity.Start.Date.Time", # to combine into proper datetime format
    # "day" = "Activity.Start.Date.Time",
    # "time" = "Activity.Start.Date.Time",
    
    # Location coordinates
    "lat_dec" = "Org.Decimal.Latitude",
    "lon_dec" = "Org.Decimal.Longitude",
    # Degree/minute format could be used to construct DD MM SS format if needed
    # "lat_deg" = "Org.Latitude..DD.MM.SS.SSSS.", # Will need combination with lat_min
    # "lat_min" = "Org.Latitude..DD.MM.SS.SSSS.", # Will need combination with lat_deg
    # "lon_deg" = "Org.Longitude..DD.MM.SS.SSSS.", # Will need combination with lon_min
    # "lon_min" = "Org.Longitude..DD.MM.SS.SSSS.", # Will need combination with lon_deg
    
    # Depth measurements
    "depth" = "Activity.Depth",
    "depth_class" = "Activity.Depth.Unit", # If this contains unit information
    "depth_order" = "Activity.Depth.Top.Bottom.Unit", # If this is related to depth orientation
    "cast" = "Sample.Collection.Type",
    "nisk_start" = "Activity.Top.Depth",
    "nisk_end" = "Activity.Bottom.Depth",
    
    # Measurement flags map to value qualifiers
    "o2_disc_flag" = "Value.Qualifier",
    "nh4_flag" = "Value.Qualifier",
    "no2_flag" = "Value.Qualifier",
    "no3_flag" = "Value.Qualifier",
    "no3_no2_flag" = "Value.Qualifier",
    "po4_flag" = "Value.Qualifier",
    "si_flag" = "Value.Qualifier",
    "avg_chl_a_flag" = "Value.Qualifier",
    "avg_phaeo_flag" = "Value.Qualifier",
    
    # Notes field
    "notes" = "Result.Comments"
  )
  
  # Define measurement columns and their corresponding units
  measurement_cols <- list(
    "temp" = list(name = "Temperature", unit = "deg C"),
    "sal" = list(name = "Salinity", unit = "psu"),
    "o2_ctd" = list(name = "Dissolved Oxygen (CTD)", unit = "mg/L"),
    "o2_disc" = list(name = "Dissolved Oxygen (Discrete)", unit = "mg/L"),
    "nh4" = list(name = "Ammonium", unit = "mg/L"),
    "no2" = list(name = "Nitrite", unit = "mg/L"),
    "no3" = list(name = "Nitrate", unit = "mg/L"),
    "no3_no2" = list(name = "Nitrate+Nitrite", unit = "mg/L"),
    "po4" = list(name = "Phosphate", unit = "mg/L"),
    "si" = list(name = "Silicate", unit = "mg/L"),
    "avg_chl_a" = list(name = "Chlorophyll a", unit = "ug/L"),
    "avg_phaeo" = list(name = "Pheophytin", unit = "ug/L")
  )
  
  # Step 1: Rename metadata columns using the mapping
  # Function to rename columns if they exist in the dataframe
  rename_if_exists <- function(df, old_name, new_name) {
    # Add debug prints to see what's happening
    if (old_name %in% names(df)) {
      # cat("Renaming column", old_name, "to", new_name, "\n")
      # cat("First few values of", old_name, ":", head(df[[old_name]]), "\n")
      
      # Store the values before renaming
      temp_values <- df[[old_name]]
      
      # Rename the column
      names(df)[names(df) == old_name] <- new_name
      
      # Verify the values were preserved after renaming
      # cat("After renaming, first few values of", new_name, ":", head(df[[new_name]]), "\n")
      
      # Double-check that values match
      if (!identical(temp_values, df[[new_name]])) {
        # cat("WARNING: Values don't match after renaming!\n")
      }
      
      # Special handling for datetime column to ensure it stays in a standard format
      if (old_name == "datetime" && new_name == "Activity.Start.Date.Time") {
        # Preserve the original format which is already ISO
        # cat("SFER datetime format detected - preserving values\n")
        # Print a few values to debug
        if (length(df[[new_name]]) > 0) {
          # cat("Sample datetime values: ", head(df[[new_name]], 3), "\n")
          
          # Convert to WIN standard format if needed
          tryCatch({
            # Parse dates to standard format
            parsed_dates <- as.POSIXct(df[[new_name]], format="%Y-%m-%d %H:%M:%S")
            
            # If parsing succeeded, convert to WIN standard format (MM/DD/YYYY HH:MM:SS)
            if (!all(is.na(parsed_dates))) {
              df[[new_name]] <- format(parsed_dates, "%m/%d/%Y %H:%M:%S")
              # cat("Converted to WIN format: ", head(df[[new_name]], 3), "\n")
            }
          }, error = function(e) {
            # cat("Could not standardize datetime format: ", e$message, "\n")
          })
        }
      }
    } else {
      # cat("Column", old_name, "not found in dataframe\n")
    }
    return(df)
  }
  
  # Apply renaming for each mapping entry
  for (old_name in names(column_mapping)) {
    df <- rename_if_exists(df, old_name, column_mapping[[old_name]])
  }
  
  # Check for case-insensitive matches and rename those too
  df_names_lower <- tolower(names(df))
  for (old_name in names(column_mapping)) {
    matches <- which(df_names_lower == tolower(old_name))
    if (length(matches) > 0) {
      for (match_idx in matches) {
        if (names(df)[match_idx] != column_mapping[[old_name]]) {  # Skip if already renamed
          names(df)[match_idx] <- column_mapping[[old_name]]
        }
      }
    }
  }
  
  # Only construct Activity.Start.Date.Time if it doesn't already exist from the datetime column
  if (all(c("year", "month", "day") %in% names(df)) && 
      ("Activity.Start.Date.Time" %in% names(df) && 
       all(is.na(df$"Activity.Start.Date.Time")))) {
    # Simply concatenate year, month, day as a string without parsing
    if ("time" %in% names(df)) {
      # If time column exists, combine date and time
      df$"Activity.Start.Date.Time" <- paste(df$year, df$month, df$day, df$time)
    } else {
      # Otherwise, just use the date
      df$"Activity.Start.Date.Time" <- paste(df$year, df$month, df$day)
    }
  }
  
  # No date parsing or format conversion should be done - keep Activity.Start.Date.Time as is
  
  # Step 3: Handle measurement columns - transform from wide to long format
  # First, identify which measurement columns exist in the dataframe
  existing_measurements <- names(measurement_cols)[names(measurement_cols) %in% names(df)]
  
  if (length(existing_measurements) > 0) {
    # Create a list to hold all rows for the long format data
    long_rows <- list()
    
    # Get the index columns that will be repeated for each measurement
    # These are all columns except the measurement columns and their flags
    flag_cols <- paste0(existing_measurements, "_flag")
    all_measurement_related_cols <- c(existing_measurements, flag_cols[flag_cols %in% names(df)])
    index_cols <- setdiff(names(df), all_measurement_related_cols)
    
    # For each row in the original dataframe
    for (i in 1:nrow(df)) {
      row_data <- df[i, ]
      
      # For each measurement column
      for (measure_col in existing_measurements) {
        # Skip if the value is NA
        if (!is.na(row_data[[measure_col]])) {
          # Create a new row for this measurement
          new_row <- row_data[index_cols]
          
          # Add the analyte name and value
          new_row$"DEP.Analyte.Name" <- measurement_cols[[measure_col]]$name
          new_row$"DEP.Result.Value.Number" <- row_data[[measure_col]]
          new_row$"DEP.Result.Unit" <- measurement_cols[[measure_col]]$unit
          
          # Add the qualifier/flag if it exists
          flag_col <- paste0(measure_col, "_flag")
          if (flag_col %in% names(df) && !is.na(row_data[[flag_col]])) {
            new_row$"Value.Qualifier" <- row_data[[flag_col]]
          }
          
          # Add this row to our list
          long_rows[[length(long_rows) + 1]] <- new_row
        }
      }
    }
    
    # Combine all rows into a new dataframe
    if (length(long_rows) > 0) {
      result_df <- do.call(rbind, long_rows)
      return(result_df)
    }
  }
  
  # If no transformation happened, return the original dataframe with renamed columns
  return(df)
}
