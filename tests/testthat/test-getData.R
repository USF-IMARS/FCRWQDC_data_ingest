library(testthat)
library(here)
library(glue)
library(dplyr)
library(lubridate)

# Load all the source files containing the functions
source(here::here("R/getSFERData.R"))
source(here::here("R/getSTORETData.R"))
source(here::here("R/getWINData.R"))
source(here::here("R/getData.R"))

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

# Test getSFERData function
test_that("getSFERData can open and read SFER data", {
  # Skip if data file doesn't exist
  sfer_file <- here::here("data/SFER_data.csv")
  skip_if_not(file.exists(sfer_file), "SFER data file not found")
  
  # Test that the file can be opened and read without error
  expect_no_error(sfer_data <- getSFERData("SFER"))
  
  # Check basic structure - it's a data frame with rows
  expect_true(is.data.frame(sfer_data))
  expect_gt(nrow(sfer_data), 0)
  
  # Check for valid dates
  check_datetime_validity(sfer_data, "SFER data")
})

# Test getSTORETData function
test_that("getSTORETData can open and read STORET data", {
  # Test with BROWARD (which should have STORET data)
  storet_file <- here::here("data/STORET_historical/STORET_Water_Quality_Results_BROWARD.txt")
  skip_if_not(file.exists(storet_file), "STORET BROWARD data file not found")
  
  # Test that the file can be opened and read without error
  expect_no_error(storet_data <- getSTORETData("BROWARD"))
  
  # Check basic structure - it's a data frame with rows
  expect_true(is.data.frame(storet_data))
  expect_gt(nrow(storet_data), 0)
  
  # Check for valid dates
  check_datetime_validity(storet_data, "STORET BROWARD data")
})

# Test getWINData function
test_that("getWINData can open and read WIN data", {
  # Test with BROWARD (which should have WIN data)
  win_file <- here::here("data/WIN/_WIN_WAVES_OTIS_BROWARD.txt")
  skip_if_not(file.exists(win_file), "WIN BROWARD data file not found")
  
  # Test that the file can be opened and read without error
  expect_no_error(win_data <- getWINData("BROWARD"))
  
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

# Test the main getData function for each program type
test_that("getData can open and read data for all program types", {
  # Test SFER
  sfer_file <- here::here("data/SFER_data.csv")
  skip_if_not(file.exists(sfer_file), "SFER data file not found")
  
  expect_no_error(sfer_data <- getData("SFER"))
  expect_true(is.data.frame(sfer_data))
  expect_gt(nrow(sfer_data), 0)
  check_datetime_validity(sfer_data, "getData SFER")
  
  # Test BROWARD
  win_file <- here::here("data/WIN/_WIN_WAVES_OTIS_BROWARD.txt")
  skip_if_not(file.exists(win_file), "BROWARD data file not found")
  
  expect_no_error(broward_data <- getData("BROWARD"))
  expect_true(is.data.frame(broward_data))
  expect_gt(nrow(broward_data), 0)
  check_datetime_validity(broward_data, "getData BROWARD")
})
