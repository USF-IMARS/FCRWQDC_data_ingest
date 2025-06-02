library(testthat)
library(dplyr)
library(lubridate)

# Source the function
source(here::here("R/seasonalMannKendall.R"))

test_that("seasonalMannKendall correctly calculates trends in dplyr pipeline", {
  # Create mock dataset
  set.seed(123) # For reproducible random data
  
  # Helper function to create a date sequence spanning multiple years
  create_date_sequence <- function(start_date, end_date, n_samples) {
    date_range <- seq(as.Date(start_date), as.Date(end_date), by = "day")
    return(sample(date_range, n_samples, replace = TRUE))
  }
  
  # =========================================================================
  # === Create test data for multiple stations
  # =========================================================================
  n_months <- 12*5
  # === Station 1 - Increasing trend
  # 1) Generate 60 consecutive days
  station1_dates  <- seq(as.Date("2020-01-01"), by = "month", length.out = n_months)
  # 2) Assign values 1 to 60 so that each day i has value i
  station1_values <- seq_len(n_months)
  # === Station 2 - Decreasing trend
  # 1) Generate 60 consecutive dates
  station2_dates  <- seq(
    as.Date("2020-01-01"),
    by = "month",
    length.out = n_months
  )
  # 2) Assign values 60 down to 1 so that each day i has value (61 - i)
  station2_values <- seq(n_months, 1, by = -1)
  
  # === Station 3 - No trend
  # 1) Generate 60 consecutive days
  station3_dates  <- seq(
    as.Date("2020-01-01"),
    by = "month",
    length.out = n_months
  )

  # 2) Assign a constant value (e.g., 42 for all 60 days)
  station3_values <- rep(42, n_months)

  
  # Combine into a single dataframe
  test_df <- data.frame(
    Monitoring.Location.ID = c(
      rep("STATION1", length(station1_dates)),
      rep("STATION2", length(station2_dates)),
      rep("STATION3", length(station3_dates))
    ),
    program = c(
      rep("PROGRAM1", length(station1_dates)),
      rep("PROGRAM1", length(station2_dates)),
      rep("PROGRAM2", length(station3_dates))
    ),
    Activity.Start.Date.Time = as.POSIXct(c(
      station1_dates, 
      station2_dates, 
      station3_dates
    )),
    DEP.Result.Value.Number = c(
      station1_values,
      station2_values,
      station3_values
    )
  )
  
  # cat('\n\n === test_df ===\n')
  # print(test_df)
  # cat('\n\n')

  # Run the function in a dplyr pipeline
  result_df <- test_df %>%
    group_by(program, Monitoring.Location.ID) %>%
    reframe(
      n_values = n(),
      trend = seasonalMannKendallVectorized(
        Activity.Start.Date.Time,
        DEP.Result.Value.Number
      )
    )
    

  cat('\n\n === result ===\n')
  print(result_df)
  cat('\n\n')
  # Tests
  # Check if we got the expected number of rows
  expect_equal(nrow(result_df), 4)

  # Check if the trends are calculated correctly
  expect_equal(result_df$trend, c(12, -12, 0, NA))
})
