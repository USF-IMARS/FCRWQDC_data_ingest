#!/usr/bin/env Rscript

# Test script for seasonalMannKendall function
# This script reads the test WIN data file and passes it to the seasonalMannKendall function

# Load required libraries
library(dplyr)
library(here)
library(lubridate)
library(readr)

# Source the required functions
source(here("R/seasonalMannKendall.R"))
source(here("R/getData.R"))  # To access real production data

# Define test function
test_seasonalMannKendall <- function() {
  cat("Starting seasonalMannKendall test...\n")
  
  # Use the production data from DERM_BBWQ
  cat("Loading data using getData function...\n")
  # Make sure to capture the returned data
  dep_data <- getData("DEP")
  
  if (!is.data.frame(dep_data) || nrow(dep_data) == 0) {
    stop("Failed to load data or data is empty")
  }
  
  cat("Successfully loaded data: ", nrow(dep_data), " rows, ", 
      ncol(dep_data), " columns\n", sep="")
  
  # For this test, we'll focus on finding the best analyte/station combination with sufficient data
  # Start with common analytes
  common_analytes <- c("TURBIDITY", "PH", "DISSOLVED OXYGEN", "TEMPERATURE, WATER", "SALINITY")
  
  # Find an analyte with sufficient data
  test_analyte <- NULL
  test_station <- NULL
  best_count <- 0
  
  # Check each analyte and find the station with the most data for that analyte
  for (analyte in common_analytes) {
    # Get stations and counts for this analyte, excluding empty station IDs
    stations_data <- dep_data %>%
      filter(DEP.Analyte.Name == analyte,
             !is.na(Monitoring.Location.ID),
             Monitoring.Location.ID != "") %>%
      group_by(Monitoring.Location.ID) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    
    if (nrow(stations_data) > 0 && stations_data$count[1] > best_count) {
      test_analyte <- analyte
      test_station <- stations_data$Monitoring.Location.ID[1]
      best_count <- stations_data$count[1]
    }
  }
  
  # If we haven't found a good analyte/station combination, use the most common one
  if (is.null(test_analyte) || is.null(test_station)) {
    # Get the most common analyte/station combination, excluding empty station IDs
    top_combo <- dep_data %>%
      filter(!is.na(Monitoring.Location.ID),
             Monitoring.Location.ID != "") %>%
      group_by(DEP.Analyte.Name, Monitoring.Location.ID) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      head(1)
    
    if (nrow(top_combo) > 0) {
      test_analyte <- top_combo$DEP.Analyte.Name[1]
      test_station <- top_combo$Monitoring.Location.ID[1]
      best_count <- top_combo$count[1]
    } else {
      stop("Could not find any analyte/station combination with sufficient data")
    }
  }
  
  cat("\nTesting for analyte: ", test_analyte, " at station: ", test_station, "\n")
  cat("Found ", best_count, " records for this combination\n")
  
  # Check if we have enough data for this analyte and station
  if (best_count < 10) {
    warning("Limited data for ", test_analyte, " analyte at ", test_station, ". Only ", best_count, " records found.")
  }
  
  df_filtered <- dep_data %>%
    # Make sure we have required columns and filter for the test analyte and station
    filter(!is.na(Activity.Start.Date.Time),
           !is.na(Monitoring.Location.ID),
           !is.na(DEP.Analyte.Name),
           !is.na(DEP.Result.Value.Number),
           DEP.Analyte.Name == test_analyte,
           Monitoring.Location.ID == test_station) %>%
    # Convert result values to numeric (if not already)
    mutate(
      DEP.Result.Value.Number = as.numeric(as.character(DEP.Result.Value.Number))
    )
    
  # Check if we have enough data after filtering
  if (nrow(df_filtered) < 10) {
    cat("WARNING: Very limited data for ", test_analyte, ". Only ", 
        nrow(df_filtered), " records found.\n")
  }
  
  # Display basic info about the filtered data
  cat("Filtered data dimensions:", nrow(df_filtered), "rows,", ncol(df_filtered), "columns\n")
  
  # Get date information from the Activity.Start.Date.Time field
  dates <- as.POSIXct(df_filtered$Activity.Start.Date.Time, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")
  years <- lubridate::year(dates)
  months <- lubridate::month(dates)
  
  # Check if we have enough data for the test
  n_sites <- length(unique(df_filtered$Monitoring.Location.ID))
  n_years <- length(unique(years))
  n_months <- length(unique(months))
  
  cat("Data summary:\n")
  cat("- Number of sites:", n_sites, "\n")
  cat("- Number of years:", n_years, "\n")
  cat("- Number of months:", n_months, "\n")
  cat("- Number of parameters:", length(unique(df_filtered$DEP.Analyte.Name)), "\n")
  
  # Data validation
  if (n_sites < 1 || n_years < 2 || n_months < 2) {
    cat("WARNING: Test data may be insufficient for meaningful trend analysis.\n")
    cat("Seasonal Mann-Kendall typically requires data from multiple years and seasons.\n")
  }
  
  # Run seasonalMannKendall on the filtered data for the specific station
  cat("Running seasonalMannKendall on filtered data for station: ", test_station, "\n")
  
  # Run the function on the test data with specific station_id
  tryCatch({
    result_matrix <- seasonalMannKendall(df_filtered, station_id = test_station)
      
    # Display results
    if (!is.null(result_matrix) && nrow(result_matrix) > 0) {
      cat("\nTest successful! Results from seasonalMannKendall:\n")
      print(result_matrix)
      cat("\nDetected ", nrow(result_matrix), " trend(s) for monitoring location(s): ", 
          paste(result_matrix[, 1], collapse = ", "), "\n")
    } else {
      cat("\nTest completed but returned no results or empty matrix.\n")
    }
  }, error = function(e) {
    cat("\nERROR: seasonalMannKendall function failed with error:\n", e$message, "\n")
  })
  
  cat("\nTest completed.\n")
}

# Run the test
test_seasonalMannKendall()
