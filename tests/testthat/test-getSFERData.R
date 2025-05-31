library(testthat)
library(here)
library(dplyr)
library(lubridate)

# Load necessary source files
source(here::here("R/getSFERData.R"))
source(here::here("tests/testthat/check_win_column_alignment.R"))

# Test getSFERData function
test_that("getSFERData can open and read SFER data", {
  
  # Test that the file can be opened and read without error
  expect_no_error(sfer_data <- getSFERData(fpath="data/test/SFER_example.csv"))
  
  # Check basic structure - it's a data frame with rows
  expect_true(is.data.frame(sfer_data))
  expect_gt(nrow(sfer_data), 0)
  
  # Use the column alignment checker to validate WIN format compliance
  cat("\n----- SFER Data Format Validation -----\n")
  alignment_results <- check_win_column_alignment(sfer_data, source_name = "SFER")
  
  # Verify alignment is above acceptable threshold
  # SFER data may have different column naming conventions, so we use a lower threshold
  expect_gte(alignment_results$alignment_percent, 60,
             paste0("SFER data alignment with WIN format is only ", 
                   alignment_results$alignment_percent, "% (below 60% threshold)"))
})
