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
    # 2) Simply combine Act.Date and Act.Time into Activity.Start.Date.Time without parsing
    dplyr::mutate(
      # Create Activity.Start.Date.Time by concatenating Act.Date and Act.Time as strings
      Activity.Start.Date.Time = paste(.data[['Act.Date']], .data[['Act.Time']])
    )
  return(df)
}
