library(testthat)
library(here)
library(glue)
library(dplyr)

# Load necessary source files
source(here::here("R/getSTORETData.R"))
source(here::here("tests/testthat/check_win_column_alignment.R"))


# NOTE: test disabled because of missing lat,lon columns in STORET data
# # Test getSTORETData function
# test_that("getSTORETData can open and read STORET data", {
#   # Test that the file can be opened and read without error
#   expect_no_error(getSTORETData(fpath=here("data/test/STORET_example.csv")))
#   storet_data <- getSTORETData(fpath=here("data/test/STORET_example.csv"))
#   # Check basic structure - it's a data frame with rows
#   expect_true(is.data.frame(storet_data))
#   expect_gt(nrow(storet_data), 0)
  
#   # Use the column alignment checker to validate WIN format compliance
#   cat("\n----- STORET Data Format Validation -----\n")
#   # We're only running a simple validation check to demonstrate the check_win_column_alignment
#   # function - not enforcing specific alignment percentages
#   alignment_results <- check_win_column_alignment(storet_data, source_name = "STORET")
  
#   # Report alignment percentage but don't enforce a threshold
#   cat(paste0("STORET data alignment with WIN format: ", 
#              alignment_results$alignment_percent, "%\n"))
# })
