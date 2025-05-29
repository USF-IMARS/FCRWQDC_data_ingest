library(testthat)
library(here)
library(dplyr)
library(lubridate)

# Load necessary source files
source(here::here("R/getMiamiBeachData.R"))
source(here::here("tests/testthat/test-utils.R"))

# Test getMiamiBeachData function
test_that("getMiamiBeachData can open and read Miami Beach data", {
  # Skip if test data folder doesn't exist
  miamibeach_folder <- here::here("data/MiamiBeach")
  skip_if_not(dir.exists(miamibeach_folder), "Miami Beach data folder not found")
  
  # Test that the data can be loaded without error
  expect_no_error(miamibeach_data <- getMiamiBeachData())
  
  # Check basic structure - it's a data frame with rows
  expect_true(is.data.frame(miamibeach_data))
  expect_gt(nrow(miamibeach_data), 0)
  
  # Check for required WIN format columns
  required_cols <- c(
    "Monitoring.Location.ID", 
    "Activity.Start.Date.Time", 
    "DEP.Analyte.Name", 
    "DEP.Result.Value.Number", 
    "DEP.Result.Unit",
    "program"
  )
  
  for (col in required_cols) {
    expect_true(col %in% names(miamibeach_data), 
                paste("Miami Beach data missing required column:", col))
  }
  
  # Check that program identifier is set correctly
  expect_true(all(miamibeach_data$program == "MIAMIBEACH"), 
             "Not all rows have program identifier set to MIAMIBEACH")
  
  # Check for valid dates
  check_datetime_validity(miamibeach_data, "Miami Beach data")
  
  # Additional detailed checks for datetime values
  test_that("Activity.Start.Date.Time contains valid date values", {
    # Check that the column exists
    expect_true("Activity.Start.Date.Time" %in% names(miamibeach_data))
    
    # Check that there are non-NA values
    expect_true(sum(!is.na(miamibeach_data$Activity.Start.Date.Time)) > 0,
               "All date values are NA")
    
    # Try to convert to Date objects
    dates <- as.Date(miamibeach_data$Activity.Start.Date.Time, 
                    format = "%Y-%m-%d", 
                    optional = TRUE)
    
    # Check that at least some dates were successfully converted
    expect_true(sum(!is.na(dates)) > 0,
               "No valid date formats found")
    
    # Check date range - should have reasonable range for environmental monitoring
    valid_dates <- dates[!is.na(dates)]
    expect_true(min(valid_dates, na.rm = TRUE) > as.Date("1990-01-01"),
               "Dates earlier than 1990 detected")
    expect_true(max(valid_dates, na.rm = TRUE) <= Sys.Date(),
               "Future dates detected")
    
    # Print some stats about the dates
    cat(paste0("\nDate range in data: ", 
              min(valid_dates, na.rm = TRUE), " to ", 
              max(valid_dates, na.rm = TRUE), "\n"))
    cat(paste0("Number of valid dates: ", sum(!is.na(dates)), 
              " out of ", length(dates), " total\n"))
  })
  
  # Check that result values are numeric
  expect_true(is.numeric(miamibeach_data$DEP.Result.Value.Number),
             "DEP.Result.Value.Number is not numeric")
  
  # Check for location IDs
  expect_gt(length(unique(miamibeach_data$Monitoring.Location.ID)), 0,
           "No monitoring location IDs found in data")
})
