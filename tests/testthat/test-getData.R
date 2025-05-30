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

# Helper function to validate key fields in a dataframe
validate_required_fields <- function(df, program_name) {
  required_fields <- c(
    "Monitoring.Location.ID",
    "DEP.Analyte.Name",
    "DEP.Result.Value.Number",
    "Activity.Start.Date.Time"
  )
  
  location_fields <- c(
    "Org.Decimal.Latitude",
    "Org.Decimal.Longitude"
  )
  
  # Check for existence of required fields
  for (field in required_fields) {
    expect_true(
      field %in% names(df), 
      glue("Required field {field} missing in {program_name} data")
    )
  }
  
  # Check that at least one location field exists (some programs may use different naming)
  has_location_field <- any(location_fields %in% names(df))
  expect_true(
    has_location_field,
    glue("No location fields found in {program_name} data. Expected at least one of: {paste(location_fields, collapse=', ')}")
  )
  
  # Check for non-empty data
  expect_gt(nrow(df), 0, glue("{program_name} data has no rows"))
  
  # Check data types and values
  if ("DEP.Result.Value.Number" %in% names(df)) {
    expect_true(is.numeric(df$DEP.Result.Value.Number), 
               glue("DEP.Result.Value.Number is not numeric in {program_name} data"))
  }
  
  if ("Activity.Start.Date.Time" %in% names(df) && nrow(df) > 0) {
    # Check that dates can be parsed
    date_sample <- head(df$Activity.Start.Date.Time, 10)
    parsed_dates <- as.POSIXct(date_sample, format="%m/%d/%Y %H:%M:%S")
    
    expect_true(
      any(!is.na(parsed_dates)), 
      glue("Activity.Start.Date.Time values cannot be parsed in {program_name} data")
    )
  }
  
  # Check for valid coordinate values if they exist
  if ("Org.Decimal.Latitude" %in% names(df) && nrow(df) > 0) {
    valid_lats <- df$Org.Decimal.Latitude >= -90 & df$Org.Decimal.Latitude <= 90
    expect_true(
      any(valid_lats, na.rm = TRUE),
      glue("No valid latitude values in {program_name} data")
    )
  }
  
  if ("Org.Decimal.Longitude" %in% names(df) && nrow(df) > 0) {
    valid_lons <- df$Org.Decimal.Longitude >= -180 & df$Org.Decimal.Longitude <= 180
    expect_true(
      any(valid_lons, na.rm = TRUE),
      glue("No valid longitude values in {program_name} data")
    )
  }
}

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
    
    # Skip if data directory doesn't exist
    skip_on_ci() # Skip on continuous integration to avoid long-running tests
    program_dir <- here::here("data", program_name)
    win_dir <- here::here("data", "WIN")
    skip_if_not(dir.exists(program_dir) || dir.exists(win_dir), glue("Data directory for {program_name} not found"))
    
    cat("\nTesting getData for program:", program_name, "\n")
    
    # Test retrieving data for this program
    tryCatch({
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
    }, error = function(e) {
      cat(glue("ERROR: Failed to validate {program_name} data: {e$message}\n"))
      fail(glue("Failed to validate {program_name} data: {e$message}"))
    })
  })
}
