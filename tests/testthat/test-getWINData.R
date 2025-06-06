library(testthat)
library(here)
library(glue)
library(dplyr)

# Load necessary source files
source(here::here("R/getWINData.R"))
source(here::here("tests/testthat/check_win_column_alignment.R"))
source(here::here("tests/testthat/check_datetime_validity.R"))

# Test getWINData function
test_that("getWINData can open and read WIN data", {  
  # Test that the file can be opened and read without error
  expect_no_error(win_data <- getWINData(fpath=here::here("data/test/WIN_example.csv")))
  
  # Check basic structure - it's a data frame with rows
  expect_true(is.data.frame(win_data))
  expect_gt(nrow(win_data), 0)
  
  # Use the column alignment checker to validate WIN format compliance
  # Since this is the reference WIN format, we expect very high alignment
  cat("\n----- WIN Data Format Validation -----\n")
  alignment_results <- check_win_column_alignment(win_data, source_name = "WIN")
  
  # Verify alignment is above acceptable threshold - should be very high for WIN data
  expect_gte(alignment_results$alignment_percent, 90,
             paste0("WIN data alignment with WIN format is only ", 
                   alignment_results$alignment_percent, "% (below 90% threshold)"))
})
