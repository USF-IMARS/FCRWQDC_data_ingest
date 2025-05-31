library(testthat)
library(dplyr)
library(glue)

# Helper function to check basic date validity
check_datetime_validity <- function(df, source_name) {
  # Check that Activity.Start.Date.Time column exists
  expect_true("Activity.Start.Date.Time" %in% names(df), 
              paste(source_name, "- Missing Activity.Start.Date.Time column"))
  
  # If the column exists, check that there's at least one valid date value
  if("Activity.Start.Date.Time" %in% names(df)) {
    # Check if there's at least one valid date
    valid_dates_exist <- any(!is.na(df$Activity.Start.Date.Time))
    expect_true(valid_dates_exist, 
                paste(source_name, "- No valid dates found in Activity.Start.Date.Time"))
  }
}
    
