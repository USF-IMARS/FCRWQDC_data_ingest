library(glue)
library(here)
# tests/testthat/test-date-range.R
test_that("minimum Activity.Start.Date.Time is not before 1900", {
  # Load the data
  df <- read.csv(here("data/exports/allDataRaw.csv"), stringsAsFactors = FALSE)
  
  # Ensure the column exists
  expect_true("Activity.Start.Date.Time" %in% names(df),
              info = "Column 'Activity.Start.Date.Time' not found in dataset")

  # parse the date column
  date_values <- as.POSIXct(df$Activity.Start.Date.Time,
                            format = "%Y-%m-%d",
                            tz = "UTC")

  # Compute the minimum date
  min_date <- min(date_values, na.rm = TRUE)
  print(glue("min date: {min_date}"))
  # Test that the minimum date is not before 1900
  expect_true(min_date >= as.POSIXct("1900-01-01", tz = "UTC"),
              info = paste("Minimum date is before 1900:", min_date))
})
