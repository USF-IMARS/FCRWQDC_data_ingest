library(testthat)
library(here)
library(dplyr)
library(lubridate)

# Load necessary source files
source(here::here("R/getSFERData.R"))
source(here::here("tests/testthat/test-utils.R"))

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
