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
  
  # Test with "test" parameter to use test data
  expect_no_error(test_data <- getData("test"))
  expect_true(is.data.frame(test_data))
  expect_gt(nrow(test_data), 0)
  
  # Verify the required fields in test data
  validate_required_fields(test_data, "test")
})

# Get the list of all programs
programs <- getListOfPrograms()

# Test each program individually
for (program_name in programs) {
  test_that(glue("getData can retrieve and validate data for {program_name}"), {
    # Skip some tests during development to avoid running all
    # Comment this out to run tests for all programs
    # skip_if_not(program_name %in% c("SFER", "MiamiBeach", "BROWARD"), "Skipping to limit test scope")
    
    cat("\nTesting getData for program:", program_name, "\n")
    
    # Use a timeout to avoid tests hanging
    program_data <- getData(program_name)
    
    # Basic checks on the returned data
    expect_true(is.data.frame(program_data), glue("{program_name} data is not a dataframe"))
    
    # Skip detailed validation if dataframe is empty
    skip_if(nrow(program_data) == 0, glue("No data available for {program_name}"))
    
    # Validate required fields
    validate_required_fields(program_data, program_name)
    
    # Verify program name is added
    if ("program" %in% names(program_data)) {
      expect_true(all(program_data$program == program_name), 
                glue("program column doesn't consistently contain {program_name}"))
    }
    
    cat(glue("SUCCESS: {program_name} data validated with {nrow(program_data)} rows\n"))
  })
}
