library(testthat)
library(here)
library(glue)
library(dplyr)

# Load the source file containing the function
source(here::here("R/getData.R"))

test_that("getData does not fail when passed 'BROWARD' as programName", {
  # Test that the function doesn't throw an error with 'BROWARD'
  expect_no_error(getData("BROWARD"))
  
  # Optionally, verify that the returned value is a data frame
  result <- getData("BROWARD")
  expect_true(is.data.frame(result))
  
  # Optionally, check that the result has some rows (not empty)
  expect_gt(nrow(result), 0)
})
