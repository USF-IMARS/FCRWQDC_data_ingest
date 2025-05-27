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

  # test DERM_BBWQ
  derm_bbwq_file <- here::here("data/WIN/_WIN_WAVES_OTIS_DERM_BBWQ.txt")
  skip_if_not(file.exists(derm_bbwq_file), "DERM_BBWQ data file not found")
  derm_bbwq_file2 <- here::here("data/STORET_historical/STORET_Water_Quality_Results_DERM_BBWQ.txt")
  skip_if_not(file.exists(derm_bbwq_file2), "DERM_BBWQ historical data file not found")
  
  expect_no_error(derm_bbwq_data <- getData("DERM_BBWQ"))
  expect_true(is.data.frame(derm_bbwq_data))
  expect_gt(nrow(derm_bbwq_data), 0)
  check_datetime_validity(derm_bbwq_data, "getData DERM_BBWQ")
})
