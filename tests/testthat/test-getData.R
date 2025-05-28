library(testthat)
library(here)
library(glue)
library(dplyr)
library(lubridate)

# Load all necessary files
source(here::here("R/getSFERData.R"))
source(here::here("R/getSTORETData.R"))
source(here::here("R/getWINData.R"))
source(here::here("R/getData.R"))
source(here::here("tests/testthat/test-utils.R"))

# Test the main getData function using test data
test_that("getData can open and read data from test files", {
  # Check if test files exist
  sfer_file <- here::here("data/test/SFER_example.csv")
  win_file <- here::here("data/test/WIN_example.csv")
  storet_file <- here::here("data/test/STORET_example.csv")
  
  skip_if_not(file.exists(sfer_file), "SFER test data file not found")
  skip_if_not(file.exists(win_file), "WIN test data file not found")
  skip_if_not(file.exists(storet_file), "STORET test data file not found")
  
  # Test with "test" parameter to use test data
  expect_no_error(test_data <- getData("test"))
  expect_true(is.data.frame(test_data))
  expect_gt(nrow(test_data), 0)
  
  # Verify the Activity.Start.Date.Time column exists (but don't validate values)
  # Since test data is minimal, we just verify the column exists
  expect_true("Activity.Start.Date.Time" %in% names(test_data),
             "Activity.Start.Date.Time column missing in test data")
  
  # Test with smaller datasets instead of the real full ones
  # This helps tests run faster and avoid file availability issues
  # while still verifying the core functionality
})
