# Required packages
library(dplyr)      # For data manipulation
library(lubridate)  # For date handling
library(EnvStats)   # For kendallSeasonalTrendTest

#' Perform Seasonal Mann-Kendall trend test on grouped vectors
#'
#' This function accepts vectors of data and performs a Seasonal Mann-Kendall
#' trend test to detect monotonic trends in water quality parameters over time,
#' accounting for seasonal variation. It's designed to be used within dplyr's
#' summarise() function.
#'
#' @param station_id Vector of station/monitoring location identifiers
#' @param datetime Vector of date/time values (can be character or POSIXct)
#' @param value Vector of numeric measurement values
#' @return Numeric trend slope or NA if analysis fails
#' @importFrom EnvStats kendallSeasonalTrendTest
#' @importFrom lubridate year month
#' @importFrom dplyr %>% mutate group_by summarize filter
#' @export
seasonalMannKendallVectorized <- function(station_id, datetime, value) {
  # Basic input validation
  if (length(station_id) == 0 || length(datetime) == 0 || length(value) == 0) {
    return(NA_real_)
  }
  
  # Convert inputs to appropriate types
  # We expect this to be called within group_by context, so all ids should be the same
  # station_id is already being used directly, so no need to create a separate variable
  
  # Handle dates - convert to POSIXct if they're not already
  if (!inherits(datetime, "POSIXct")) {
    datetime <- as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }
  
  # Extract year and month
  years <- year(datetime)
  months <- month(datetime)
  
  # Convert values to numeric
  values <- as.numeric(value)
  
  # Remove NA values
  valid_idx <- !is.na(values) & !is.na(years) & !is.na(months)
  if (sum(valid_idx) < 8) {
    # Not enough valid data points
    return(NA_real_)
  }
  
  years <- years[valid_idx]
  months <- months[valid_idx]
  values <- values[valid_idx]
  
  # Check if we have enough years
  unique_years <- unique(years)
  if (length(unique_years) < 2) {
    # Need at least 2 years
    return(NA_real_)
  }
  
  # Create a data frame for analysis
  df <- data.frame(
    Month = as.factor(months),
    Year = as.numeric(years),
    Value = as.numeric(values)
  )
  
  # Compute monthly averages if there are multiple readings per month
  analysis_df <- df %>%
    dplyr::group_by(.data$Year, .data$Month) %>%
    dplyr::summarize(
      Value = mean(.data$Value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Check if we have enough data points after aggregation
  if (nrow(analysis_df) < 8) {
    return(NA_real_)
  }
  
  # Check for sufficient data in each month
  month_counts <- table(analysis_df$Month)
  min_month_count <- min(month_counts)
  
  if (min_month_count < 2) {
    # Need at least 2 observations per month
    return(NA_real_)
  }
  
  # Run the seasonal Mann-Kendall test
  tryCatch({
    test_result <- EnvStats::kendallSeasonalTrendTest(
      Value ~ Month + Year, 
      data = analysis_df
    )
    
    # Extract the slope
    if (is.list(test_result) && 
        "estimate" %in% names(test_result) && 
        is.numeric(test_result$estimate["slope"])) {
      return(as.numeric(test_result$estimate["slope"]))
    } else {
      return(NA_real_)
    }
  }, error = function(e) {
    return(NA_real_)
  })
}
