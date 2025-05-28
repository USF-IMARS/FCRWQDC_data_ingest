library(testthat)
library(here)
library(glue)
library(dplyr)

# Load necessary source files
source(here::here("R/getWINData.R"))
source(here::here("tests/testthat/test-utils.R"))

# Test getWINData function
test_that("getWINData can open and read WIN data", {
  # Test with test data instead of BROWARD
  win_file <- here::here("data/test/WIN_example.csv")
  skip_if_not(file.exists(win_file), "WIN test data file not found")
  
  # Test that the file can be opened and read without error
  expect_no_error(win_data <- getWINData("test"))
  
  # Check basic structure - it's a data frame with rows
  expect_true(is.data.frame(win_data))
  expect_gt(nrow(win_data), 0)
  
  # Check for Activity.Start.Date.Time column if it should exist
  if("Activity.Start.Date.Time" %in% names(win_data)) {
    check_datetime_validity(win_data, "WIN BROWARD data")
  } else {
    # If the column doesn't exist, make sure other essential columns are there
    expected_cols <- c("Organization.ID", "Monitoring.Location.ID")
    for(col in expected_cols) {
      expect_true(col %in% names(win_data), paste("WIN data missing expected column:", col))
    }
  }
})
