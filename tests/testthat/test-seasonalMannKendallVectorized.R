library(testthat)
library(dplyr)
library(lubridate)

# Source the function
source(here::here("R/seasonalMannKendallVectorized.R"))

test_that("seasonalMannKendallVectorized correctly calculates trends in dplyr pipeline", {
  # Create mock dataset
  set.seed(123) # For reproducible random data
  
  # Helper function to create a date sequence spanning multiple years
  create_date_sequence <- function(start_date, end_date, n_samples) {
    date_range <- seq(as.Date(start_date), as.Date(end_date), by = "day")
    return(sample(date_range, n_samples, replace = TRUE))
  }
  
  # Create test data for multiple stations
  # Station 1 - Increasing trend
  station1_dates <- create_date_sequence("2020-01-01", "2023-12-31", 60)
  # create values with a slope of 1
  station1_values <- seq(1, length(station1_dates))
  
  # Station 2 - Decreasing trend
  station2_dates <- create_date_sequence("2020-01-01", "2023-12-31", 60)
  # create values with a slope of -1
  station2_values <- seq(length(station2_dates), 1)
  
  # Station 3 - No trend
  station3_dates <- create_date_sequence("2020-01-01", "2023-12-31", 60)
  # create values with a slope of 0
  station3_values <- rep(1, length(station3_dates))

  # Station 4 - Insufficient data (only 1 year)
  station4_dates <- create_date_sequence("2022-01-01", "2022-12-31", 10)
  station4_values <- rnorm(length(station4_dates), mean = 5, sd = 0)
  
  # Station 5 - Too few data points
  station5_dates <- create_date_sequence("2020-01-01", "2022-12-31", 5)
  station5_values <- rnorm(length(station5_dates), mean = 5, sd = 0)
  
  # Combine into a single dataframe
  test_df <- data.frame(
    Monitoring.Location.ID = c(
      rep("STATION1", length(station1_dates)),
      rep("STATION2", length(station2_dates)),
      rep("STATION3", length(station3_dates)),
      rep("STATION4", length(station4_dates)),
      rep("STATION5", length(station5_dates))
    ),
    program = c(
      rep("PROGRAM1", length(station1_dates)),
      rep("PROGRAM1", length(station2_dates)),
      rep("PROGRAM2", length(station3_dates)),
      rep("PROGRAM2", length(station4_dates)),
      rep("PROGRAM3", length(station5_dates))
    ),
    Activity.Start.Date.Time = as.POSIXct(c(
      station1_dates, 
      station2_dates, 
      station3_dates,
      station4_dates,
      station5_dates
    )),
    DEP.Result.Value.Number = c(
      station1_values,
      station2_values,
      station3_values,
      station4_values,
      station5_values
    )
  )
  
  # Run the function in a dplyr pipeline
  result_df <- test_df %>%
    group_by(program, Monitoring.Location.ID) %>%
    summarise(
      n_values = n(),
      trend = seasonalMannKendallVectorized(
        Monitoring.Location.ID,
        Activity.Start.Date.Time,
        DEP.Result.Value.Number
      ),
      .groups = "drop"
    )
    
  # Tests
  # Check if we got the expected number of rows
  expect_equal(nrow(result_df), 5)

  # Check if the trends are calculated correctly
  expect_equal(result_df$trend, c(1, -1, 0, NA, NA))
})
