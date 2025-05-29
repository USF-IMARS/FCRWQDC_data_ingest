# Required packages
library(dplyr)      # For data manipulation
library(lubridate)  # For date handling
library(EnvStats)   # For kendallSeasonalTrendTest

#' Perform Seasonal Mann-Kendall trend test on water quality data
#'
#' This function accepts data in WIN format and performs a Seasonal Mann-Kendall
#' trend test to detect monotonic trends in water quality parameters over time,
#' accounting for seasonal variation. It expects data for a single analyte at a
#' specific station.
#'
#' @param df A dataframe containing water quality data in WIN format
#' @param station_id Optional. If provided, only this station will be analyzed.
#' @return A matrix with trend slopes for each monitoring location
#' @importFrom EnvStats kendallSeasonalTrendTest
#' @export
seasonalMannKendall <- function(df, station_id = NULL) {
  # Check for required WIN column names
  required_cols <- c(
    "Monitoring.Location.ID",   # Station/site identifier
    "Activity.Start.Date.Time", # Date of measurement
    "DEP.Result.Value.Number"  # Measured value
  )
  
  missing_cols <- required_cols[!required_cols %in% names(df)]
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Process the incoming data
  cat("Processing data for Seasonal Mann-Kendall analysis...\n")
  
  # Extract year and month from date
  df <- df %>%
    mutate(
      Date = as.POSIXct(.data$Activity.Start.Date.Time, format = "%m/%d/%Y %H:%M:%S", tz = "UTC"),
      Year = lubridate::year(.data$Date),
      Month = lubridate::month(.data$Date)
    )
  
  # Check if we have sufficient data
  n_years <- length(unique(df$Year))
  
  if (n_years < 2) {
    stop("Need at least 2 years of data for Seasonal Mann-Kendall analysis")
  }
  
  # Check for seasonal coverage
  n_months <- length(unique(df$Month))
  if (n_months < 4) {
    warning("Limited seasonal coverage with only ", n_months, " months represented.")
  }
  
  # If station_id is provided, filter for just that station
  if (!is.null(station_id)) {
    df <- df %>% filter(.data$Monitoring.Location.ID == station_id)
    if (nrow(df) == 0) {
      stop("No data found for station ID: ", station_id)
    }
  }
  
  # Create a clean dataframe for analysis
  analysis_df <- df %>%
    # Compute monthly averages if there are multiple readings per month
    group_by(.data$Monitoring.Location.ID, .data$Year, .data$Month) %>%
    summarize(
      Value = mean(.data$DEP.Result.Value.Number, na.rm = TRUE),
      ValueCount = n(),
      .groups = 'drop'
    )
    
  # Check if we have any valid values
  if (nrow(analysis_df) == 0) {
    warning("No valid data points after aggregation. Check for NA values in DEP.Result.Value.Number.")
    return(matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c("Monitoring.Location.ID", "Trend.Slope"))))
  }
  
  # Get unique monitoring locations
  unique_sites <- unique(analysis_df$Monitoring.Location.ID)
  cat(paste0("Processing ", length(unique_sites), " monitoring locations\n"))
  
  # Create result matrix to store slopes
  result_matrix <- matrix(ncol = 2, nrow = length(unique_sites))
  colnames(result_matrix) <- c("Monitoring.Location.ID", "Trend.Slope")
  
  # Process each site
  for (i in seq_along(unique_sites)) {
    site_name <- unique_sites[i]
    cat(paste0("Analyzing site: ", site_name, "...\n"))
    
    # Filter data for this site
    site_df <- analysis_df %>% filter(.data$Monitoring.Location.ID == site_name)
    
    # Check if we have enough data
    if (nrow(site_df) < 8 || length(unique(site_df$Year)) < 2) {
      cat(paste0("  Insufficient data for site ", site_name, 
                "\n  Need at least 8 observations across at least 2 years\n"))
      next
    }
    
    # Prepare data for the test
    # Make sure we have the Value column
    if (!"Value" %in% names(site_df)) {
      cat(paste0("  Missing Value column for site ", site_name, "\n"))
      next
    }
    
    # Convert data types for analysis
    site_df <- site_df %>%
      mutate(
        Month = as.factor(.data$Month),   # Month must be a factor for seasons
        Year = as.numeric(.data$Year),     # Year must be numeric
        Value = as.numeric(.data$Value)    # Values must be numeric
      )
    
    # Run the seasonal Mann-Kendall test
    tryCatch({
      # Debug info
      cat(paste0("  Data dimensions for site ", site_name, ": ", nrow(site_df), " rows with ", 
               length(unique(site_df$Year)), " years and ", 
               length(unique(site_df$Month)), " months\n"))
      
      # First check for sufficient data in each season
      month_counts <- table(site_df$Month)
      min_month_count <- min(month_counts)
      
      if (min_month_count < 2) {
        cat(paste0("  Insufficient data for some months at site ", site_name, 
                  ". Minimum observations per month: ", min_month_count, "\n"))
        next
      }
      
      # Now perform the test
      test_result <- EnvStats::kendallSeasonalTrendTest(Value ~ Month + Year, data = site_df)
      
      # Extract the slope - be explicit about structure
      if (is.list(test_result)) {
        if ("estimate" %in% names(test_result)) {
          estimate_part <- test_result$estimate
          if (is.numeric(estimate_part["slope"])) {
            trend_slope <- as.numeric(estimate_part["slope"])
            
            # Store the result
            result_matrix[i, 1] <- site_name
            result_matrix[i, 2] <- trend_slope
            
            cat(paste0("  Completed analysis for ", site_name, 
                      ": slope = ", sprintf("%.6f", trend_slope), "\n"))
          } else {
            cat(paste0("  Slope is not a numeric value for site ", site_name, "\n"))
          }
        } else {
          cat(paste0("  No estimate component in test result for site ", site_name, "\n"))
        }
      } else {
        cat(paste0("  Test result is not a list for site ", site_name, "\n"))
      }
    }, error = function(e) {
      cat(paste0("ERROR: ", site_name, " - ", conditionMessage(e), "\n"))
    })
  }
  
  # Clean up the result matrix - remove rows with NA values
  result_matrix <- result_matrix[!is.na(result_matrix[,1]), , drop = FALSE]
  
  if (nrow(result_matrix) == 0) {
    warning("No valid results produced. Check that your data meets requirements for Seasonal Mann-Kendall analysis.")
  } else {
    cat(paste0("Completed analysis for ", nrow(result_matrix), " sites\n"))
  }
  
  return(result_matrix)
}