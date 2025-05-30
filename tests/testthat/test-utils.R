library(testthat)
library(dplyr)
library(glue)

# Helper function to check basic date validity
check_datetime_validity <- function(df, source_name) {
  # Check that Activity.Start.Date.Time column exists
  expect_true("Activity.Start.Date.Time" %in% names(df), 
              paste(source_name, "- Missing Activity.Start.Date.Time column"))
  
  # If the column exists, check that there's at least one valid date value
  if("Activity.Start.Date.Time" %in% names(df)) {
    # Check if there's at least one valid date
    valid_dates_exist <- any(!is.na(df$Activity.Start.Date.Time))
    expect_true(valid_dates_exist, 
                paste(source_name, "- No valid dates found in Activity.Start.Date.Time"))
  }
}

# Helper function to check dataset alignment with WIN dataframe columns
check_win_column_alignment <- function(df, source_name = "Dataset", enforce_checks = TRUE) {
  # Define core WIN column groups for checking
  win_core_columns <- list(
    # Required identification columns
    id_columns = c(
      "DEP.Result.ID",
      "Activity.ID",
      "Monitoring.Location.ID"
    ),
    
    # Date/time columns
    datetime_columns = c(
      "Activity.Start.Date.Time"
    ),
    
    # Location columns (at least one should be present)
    location_columns = c(
      "Org.Decimal.Latitude",
      "Org.Decimal.Longitude"
    ),
    
    # Result columns
    result_columns = c(
      "DEP.Analyte.Name",
      "DEP.Result.Value.Number", 
      "DEP.Result.Unit"
    ),
    
    # Contextual/metadata columns (important but not all may be required)
    metadata_columns = c(
      "Organization.ID",
      "Activity.Type",
      "Activity.Depth",
      "Activity.Depth.Unit",
      "Sample.Collection.Type",
      "Result.Comments",
      "Value.Qualifier"
    )
  )
    
  # Check required identification columns
  cat("\n=== Checking ID Columns ===\n")
  for (col in win_core_columns$id_columns) {
    has_column <- col %in% names(df)
    cat(glue("Column '{col}': {ifelse(has_column, 'PRESENT', 'MISSING')}\n"))
    
    # Only Monitoring.Location.ID is absolutely required
    if (col == "Monitoring.Location.ID" && enforce_checks) {
      expect_true(has_column, glue("{source_name} missing required column: {col}"))
    }
  }
  
  # Check datetime columns - Activity.Start.Date.Time is required
  cat("\n=== Checking Date/Time Columns ===\n")
  check_datetime_validity(df, source_name)
  
  # Check location columns - at least one should be present
  cat("\n=== Checking Location Columns ===\n")
  location_present <- FALSE
  for (col in win_core_columns$location_columns) {
    has_column <- col %in% names(df)
    if (has_column) location_present <- TRUE
    cat(glue("Column '{col}': {ifelse(has_column, 'PRESENT', 'MISSING')}\n"))
    
    # Check coordinate values if present
    if (has_column && nrow(df) > 0) {
      if (grepl("Latitude", col)) {
        # For latitude columns, values should be between -90 and 90
        valid_values <- sum(!is.na(df[[col]]) & df[[col]] >= -90 & df[[col]] <= 90, na.rm = TRUE)
        cat(glue("  Valid values: {valid_values} out of {sum(!is.na(df[[col]]))}\n"))
      } else if (grepl("Longitude", col)) {
        # For longitude columns, values should be between -180 and 180
        valid_values <- sum(!is.na(df[[col]]) & df[[col]] >= -180 & df[[col]] <= 180, na.rm = TRUE)
        cat(glue("  Valid values: {valid_values} out of {sum(!is.na(df[[col]]))}\n"))
      }
    }
  }
  if (enforce_checks) {
    expect_true(location_present, glue("{source_name} missing all location columns"))
  }
  
  # Check result columns
  cat("\n=== Checking Result Columns ===\n")
  for (col in win_core_columns$result_columns) {
    has_column <- col %in% names(df)
    cat(glue("Column '{col}': {ifelse(has_column, 'PRESENT', 'MISSING')}\n"))
    if (enforce_checks) {
      expect_true(has_column, glue("{source_name} missing required column: {col}"))
    }
    
    # Check DEP.Result.Value.Number specifically for numeric type
    if (col == "DEP.Result.Value.Number" && has_column) {
      is_numeric <- is.numeric(df[[col]])
      cat(glue("  Data type check: {ifelse(is_numeric, 'NUMERIC', 'NON-NUMERIC')}\n"))
      if (enforce_checks) {
        expect_true(is_numeric, glue("{source_name} column {col} is not numeric"))
      }
    }
  }
  
  # Check metadata columns - these are important but not all required
  cat("\n=== Checking Metadata Columns ===\n")
  metadata_count <- 0
  for (col in win_core_columns$metadata_columns) {
    has_column <- col %in% names(df)
    if (has_column) metadata_count <- metadata_count + 1
    cat(glue("Column '{col}': {ifelse(has_column, 'PRESENT', 'MISSING')}\n"))
  }
  cat(glue("Metadata columns present: {metadata_count} out of {length(win_core_columns$metadata_columns)}\n"))
  
  # Output overall summary
  all_core_columns <- unlist(win_core_columns)
  present_core_columns <- sum(all_core_columns %in% names(df))
  cat(glue("\n=== Column Alignment Summary ===\n"))
  cat(glue("Core WIN columns present: {present_core_columns} out of {length(all_core_columns)}\n"))
  cat(glue("Alignment percentage: {round(present_core_columns/length(all_core_columns)*100, 1)}%\n"))
  
  # Return the alignment stats
  return(list(
    present_core_columns = present_core_columns,
    total_core_columns = length(all_core_columns),
    alignment_percent = round(present_core_columns/length(all_core_columns)*100, 1),
    missing_core_columns = setdiff(all_core_columns, names(df))
  ))
}
