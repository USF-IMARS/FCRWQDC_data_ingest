# FIU test disabled because of missing lat,lon columns

library(testthat)
library(here)
library(dplyr)

# Load necessary source files
source(here::here("R/getFIUData.R"))
source(here::here("tests/testthat/check_win_column_alignment.R"))
source(here::here("tests/testthat/check_datetime_validity.R"))
source(here::here("R/getFpath.R"))

# Test basic loading functionality and column alignment
test_that("getFIUData can open and read FIU data", {  
  # Test that the data can be loaded without error
  expect_no_error(getFIUData(fpath=here("data/test/FIU.csv")))
  fiu_data <- getFIUData(fpath=here("data/test/FIU.csv"))

  # Check basic structure - it's a data frame with rows
  expect_true(is.data.frame(fiu_data))
  expect_gt(nrow(fiu_data), 0)
  
  # Check for program identifier
  expect_true("program" %in% names(fiu_data),
              "FIU data missing 'program' column")
  
  # Use the column alignment checker to validate WIN format compliance
  cat("\n----- FIU Data Format Validation -----\n")
  alignment_results <- check_win_column_alignment(fiu_data, source_name = "FIU")
  
  # Verify alignment is above acceptable threshold
  expect_gte(alignment_results$alignment_percent, 40,
             paste0("FIU data alignment with WIN format is only ", 
                   alignment_results$alignment_percent, "% (below 40% threshold)"))
})
