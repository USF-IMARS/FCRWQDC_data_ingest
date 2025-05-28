#' Align STORET data columns to standardized WIN column names
#'
#' This function takes a dataframe with STORET columns and maps them to the
#' corresponding WIN column names for consistency across data sources.
#'
#' @param df A dataframe with STORET column names
#' @return A dataframe with columns renamed to match WIN standards
#' @export
#'
#' @examples
#' # Assuming df has STORET columns
#' standardized_df <- align_storet_df(df)
align_storet_df <- function(df) {
  # === maps STORET columns to WIN column names
  # WIN columns:
  #  Organization.ID Monitoring.Location.ID Org.Decimal.Latitude
  #  Org.Latitude..DD.MM.SS.SSSS. Org.Decimal.Longitude
  #  Org.Longitude..DD.MM.SS.SSSS.   WBID Activity.ID Activity.Type
  #  Activity.Start.Date.Time Matrix Sample.Collection.Type
  #  Sampling.Agency.Name Activity.Depth
  #  Activity.Depth.Unit Activity.Top.Depth Activity.Bottom.Depth
  #  Activity.Depth.Top.Bottom.Unit DEP.Result.ID   DEP.Analyte.Name
  #  DEP.Result.Value.Number DEP.Result.Value.Text DEP.Result.Unit DEP.MDL
  #  DEP.PQL Value.Qualifier Sample.Fraction Lab.ID Result.Comments
  #  Audit.Censored.Decisions 
  
  # STORET Columns:
  #  Org.ID Org.Name Station.ID Act.Date Act.Time
  #  Act.Type Act.Category Act.Depth Depth.Units Relative.Depth Characteristic
  #  Result.Value Result.Units   VQ Analysis.Date Analysis.Time Procedure.Name
  #  Comment MDL MDL.Units PQL Medium

  # Using standard evaluation with dplyr to avoid "no visible binding" lints
  df <- df %>%
    # 1) rename known STORET → WIN columns - using proper standard evaluation
    dplyr::mutate(
      Organization.ID            = .data[['Org.ID']],
      Sampling.Agency.Name       = .data[['Org.Name']],
      Monitoring.Location.ID     = .data[['Station.ID']],
      Activity.Type              = .data[['Act.Type']],
      Activity.Depth             = .data[['Act.Depth']],
      Activity.Depth.Unit        = .data[['Depth.Units']],
      DEP.Analyte.Name           = .data[['Characteristic']],
      DEP.Result.Value.Number    = .data[['Result.Value']],
      DEP.Result.Unit            = .data[['Result.Units']],
      DEP.MDL                    = .data[['MDL']],
      DEP.PQL                    = .data[['PQL']],
      Value.Qualifier            = .data[['VQ']],
      Result.Comments            = .data[['Comment']]
    )
  
  # Continue transformation, ensuring we capture the result
  df <- df %>%
    # 2) build the DateTime - using proper standard evaluation with improved format handling
    dplyr::mutate(
      # First try to parse the date-time using standard format
      temp_datetime = suppressWarnings(as.POSIXct(
        paste(.data[['Act.Date']], .data[['Act.Time']]),
        format = "%m/%d/%Y %H:%M:%S",
        tz     = "UTC"
      )),
      
      # If that doesn't work, try with AM/PM format
      temp_datetime2 = suppressWarnings(as.POSIXct(
        paste(.data[['Act.Date']], .data[['Act.Time']]),
        format = "%m/%d/%Y %I:%M:%S %p",
        tz     = "UTC"
      )),
      
      # Combine the results, using the second attempt if the first failed
      Activity.Start.Date.Time = ifelse(
        is.na(temp_datetime) & !is.na(temp_datetime2),
        temp_datetime2,
        temp_datetime
      )
    ) %>%
    # Remove temporary columns
    dplyr::select(-temp_datetime, -temp_datetime2)
  
  # Convert to proper POSIXct type (it might be numeric after the ifelse)
  if ("Activity.Start.Date.Time" %in% names(df)) {
    df$Activity.Start.Date.Time <- as.POSIXct(df$Activity.Start.Date.Time, origin="1970-01-01", tz="UTC")
  }
  return(df)
}
