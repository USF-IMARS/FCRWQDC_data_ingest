#' Align WIN data columns to standardized WIN column names
#'
#' This function takes a dataframe with WIN columns and ensures consistency
#' across data sources. Since WIN data is already in the target format,
#' this function primarily handles type conversions and minor adjustments.
#'
#' @param df A dataframe with WIN column names
#' @return A dataframe with consistent column types
#' @export
#'
#' @examples
#' # Assuming df has WIN columns
#' standardized_df <- align_win_df(df)
align_win_df <- function(df) {
  # WIN data is already in the target format, but we need to ensure
  # that column types are consistent across all data sources
  
  # Ensure date-time is in proper format
  if ("Activity.Start.Date.Time" %in% names(df)) {
    # If it's a character, convert to POSIXct using WIN's date format MM/DD/YYYY HH:MM:SS
    if (is.character(df$Activity.Start.Date.Time)) {
      # Try the most common WIN format first: MM/DD/YYYY HH:MM:SS
      converted_dates <- suppressWarnings(as.POSIXct(
        df$Activity.Start.Date.Time, 
        format = "%m/%d/%Y %H:%M:%S", 
        tz = "UTC"
      ))
      
      # If many NAs were introduced, try other common formats
      if (sum(is.na(converted_dates)) > 0.2 * length(df$Activity.Start.Date.Time)) {
        alternate_formats <- c(
          "%m/%d/%Y %H:%M",       # Without seconds
          "%Y-%m-%d %H:%M:%S",    # ISO format: 2021-11-16 10:25:00
          "%Y-%m-%d %H:%M",       # ISO without seconds
          "%Y-%m-%d",            # Date only
          "%m/%d/%Y"             # Date only MM/DD/YYYY
        )
        
        for (fmt in alternate_formats) {
          # Try with current format for any NA values
          na_indices <- which(is.na(converted_dates))
          if (length(na_indices) == 0) break
          
          temp <- suppressWarnings(as.POSIXct(
            df$Activity.Start.Date.Time[na_indices], 
            format = fmt, 
            tz = "UTC"
          ))
          
          # Replace any successfully converted values
          valid_indices <- which(!is.na(temp))
          if (length(valid_indices) > 0) {
            converted_dates[na_indices[valid_indices]] <- temp[valid_indices]
          }
        }
      }
      
      df$Activity.Start.Date.Time <- converted_dates
    }
  }
  
  # Ensure Monitoring.Location.ID is character type for consistency with other sources
  if ("Monitoring.Location.ID" %in% names(df)) {
    df$Monitoring.Location.ID <- as.character(df$Monitoring.Location.ID)
  }
  
  # Ensure numeric values are properly typed
  if ("DEP.Result.Value.Number" %in% names(df)) {
    df$DEP.Result.Value.Number <- as.numeric(as.character(df$DEP.Result.Value.Number))
  }
  
  # Return the aligned dataframe
  return(df)
}
