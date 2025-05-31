library(testthat)
library(here)
library(dplyr)

# Load necessary source files
source(here::here("R/getMiamiBeachData.R"))
source(here::here("tests/testthat/check_win_column_alignment.R"))
source(here::here("tests/testthat/check_datetime_validity.R"))
source(here::here("R/getFpath.R"))

# Test basic loading functionality and column alignment
test_that("getMiamiBeachData can open and read Miami Beach data", {  
  # Test that the data can be loaded without error
  expect_no_error(getMiamiBeachData(fpath=here("data/test/miamiBeach.txt")))
  miamibeach_data <- getMiamiBeachData(fpath=here("data/test/miamiBeach.txt"))

  # Check basic structure - it's a data frame with rows
  expect_true(is.data.frame(miamibeach_data))
  expect_gt(nrow(miamibeach_data), 0)
  
  # Check for program identifier
  expect_true("program" %in% names(miamibeach_data),
              "Miami Beach data missing 'program' column")
  
  # Use the column alignment checker to validate WIN format compliance
  cat("\n----- Miami Beach Data Format Validation -----\n")
  alignment_results <- check_win_column_alignment(miamibeach_data, source_name = "Miami Beach")
  
  # Verify alignment is above acceptable threshold
  expect_gte(alignment_results$alignment_percent, 40,
             paste0("Miami Beach data alignment with WIN format is only ", 
                   alignment_results$alignment_percent, "% (below 40% threshold)"))
})
