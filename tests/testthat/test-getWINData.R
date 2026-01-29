library(testthat)
library(here)
library(glue)
library(dplyr)

# Load necessary source files
source(here::here("R/getWINData.R"))
source(here::here("R/getData.R"))
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

# Test DMS coordinate conversion for PALMBEACH data
test_that("DMS coordinates are converted to decimal degrees when Org.Decimal.Latitude/Longitude are missing", {
  # Load PALMBEACH data which has DMS coordinates but no decimal coordinates
  # The getData function should call processDMSCoordinates to convert them
  palmbeach_data <- getData("PALMBEACH")
  
  # Check that we got data
  expect_true(is.data.frame(palmbeach_data))
  expect_gt(nrow(palmbeach_data), 0)
  
  # Check that Org.Decimal.Latitude and Org.Decimal.Longitude columns exist
  expect_true("Org.Decimal.Latitude" %in% names(palmbeach_data))
  expect_true("Org.Decimal.Longitude" %in% names(palmbeach_data))
  
  # Check that the decimal coordinates are now populated (not all NA)
  # The PALMBEACH file has empty decimal coordinate fields in the original data
  # but should have values after DMS conversion
  non_na_lat <- sum(!is.na(palmbeach_data$Org.Decimal.Latitude))
  non_na_lon <- sum(!is.na(palmbeach_data$Org.Decimal.Longitude))
  
  expect_gt(non_na_lat, 0, 
            "No latitude values were converted from DMS format")
  expect_gt(non_na_lon, 0, 
            "No longitude values were converted from DMS format")
  
  # Verify the conversion is correct for a specific known location
  # Example: "26 41 3.4" should convert to 26 + 41/60 + 3.4/3600 = 26.684278 degrees
  # Example: "-80 21 4.68" should convert to -(80 + 21/60 + 4.68/3600) = -80.351300 degrees
  
  # Find rows with station "38B" which we know has these coordinates
  station_38b <- palmbeach_data %>% 
    filter(Monitoring.Location.ID == "38B") %>%
    filter(!is.na(Org.Decimal.Latitude))
  
  if(nrow(station_38b) > 0) {
    # Check latitude: "26 41 3.4" -> 26.684278 degrees
    expected_lat <- 26 + 41/60 + 3.4/3600
    actual_lat <- station_38b$Org.Decimal.Latitude[1]
    expect_equal(actual_lat, expected_lat, tolerance = 0.00001,
                label = "Latitude conversion from DMS to decimal degrees")
    
    # Check longitude: "-80 21 4.68" -> -80.351300 degrees
    # Note: The DMS string in the file is "-80 21 4.68", so the negative is already in the string
    expected_lon <- -(80 + 21/60 + 4.68/3600)
    actual_lon <- station_38b$Org.Decimal.Longitude[1]
    expect_equal(actual_lon, expected_lon, tolerance = 0.00001,
                label = "Longitude conversion from DMS to decimal degrees")
  }
  
  cat("\n----- DMS Conversion Statistics -----\n")
  cat(glue("Total rows: {nrow(palmbeach_data)}\n"))
  cat(glue("Rows with converted latitude: {non_na_lat}\n"))
  cat(glue("Rows with converted longitude: {non_na_lon}\n"))
  cat(glue("Conversion rate: {round(100 * non_na_lat / nrow(palmbeach_data), 1)}%\n"))
})
