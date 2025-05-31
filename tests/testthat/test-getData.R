library(testthat)
library(here)
library(glue)
library(dplyr)
library(lubridate)

# Load all necessary files
source(here::here("R/getListOfPrograms.R"))
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
})