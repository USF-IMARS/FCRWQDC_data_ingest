library(dplyr)

mutateWINToSEACAR <- function(df, .keep = "none"){
  # map WIN column names to post-2025 reporting columns
  # specified by 
  # excel file SEACAR_Metadata_UnifiedWQ_ID10006.xlsx
  # sheet Col_WaterQuality_Discrete
  # provided by Cheryl P. Clark
  
  # Seemingly duplicate column names are listed in comments below.
  # Values in columns that may be added can be checked using :
  #     > v1 <- 'program'
  #     > v2 <- 'ProgramName'
  #     > df[
  #       +     is.na(df[[v1]]) & !is.na(df[[v2]]),
  #       +     c(v1, v2, 'program')
  #       + ]

  
  # To make mutate() not error when a source column is missing, 
  # and instead fill with NA, wrap each source column in a helper 
  # `safe_col` that returns the column if it exists, else NA.
  safe_col <- function(df, col) {
    if (col %in% names(df)) df[[col]] else NA
  }
  
  df %>%
    mutate(
      ProgramName       = safe_col(cur_data(), "program"),
      ParameterName     = safe_col(cur_data(), "DEP.Analyte.Name"),
      ProgramLocationID = safe_col(cur_data(), "Monitoring.Location.ID"),
      ActivityType      = safe_col(cur_data(), "Activity.Type"),
      SampleDate        = safe_col(cur_data(), "Activity.Start.Date.Time"),
      ResultValue       = safe_col(cur_data(), "DEP.Result.Value.Number"),
      ActivityDepth_m   = safe_col(cur_data(), "Activity.Depth"),
      ValueQualifier    = safe_col(cur_data(), "Value.Qualifier"),
      ResultComments    = safe_col(cur_data(), "Result.Comments"),
      OriginalLatitude  = safe_col(cur_data(), "Org.Decimal.Latitude"),
      OriginalLongitude = safe_col(cur_data(), "Org.Decimal.Longitude"),
      
      RowID                = safe_col(cur_data(), "RowID"),
      ProgramID            = safe_col(cur_data(), "ProgramID"),
      SEACAR_EventID       = safe_col(cur_data(), "SEACAR_EventID"),
      Habitat              = safe_col(cur_data(), "Habitat"),
      IndicatorID          = safe_col(cur_data(), "IndicatorID"),
      IndicatorName        = safe_col(cur_data(), "IndicatorName"),
      ParameterID          = safe_col(cur_data(), "ParameterID"),
      ParameterUnits       = safe_col(cur_data(), "DEP.Result.Unit"),
      AreaID               = safe_col(cur_data(), "AreaID"),
      ManagedAreaName      = safe_col(cur_data(), "ManagedAreaName"),
      Region               = safe_col(cur_data(), "Region"),
      Year                 = safe_col(cur_data(), "Year"),
      Month                = safe_col(cur_data(), "Month"),
      RelativeDepth        = safe_col(cur_data(), "RelativeDepth"),
      TotalDepth_m         = safe_col(cur_data(), "TotalDepth_m"),
      MDL                  = safe_col(cur_data(), "MDL"),
      PQL                  = safe_col(cur_data(), "PQL"),
      DetectionUnit        = safe_col(cur_data(), "DetectionUnit"),
      ValueQualifierSource = safe_col(cur_data(), "ValueQualifierSource"),
      SEACAR_QAQCFlagCode  = safe_col(cur_data(), "SEACAR_QAQCFlagCode"),
      SEACAR_QAQC_Description = safe_col(cur_data(), "SEACAR_QAQC_Description"),
      Include              = safe_col(cur_data(), "Include"),
      MADup                = safe_col(cur_data(), "MADup"),
      ExportVersion        = safe_col(cur_data(), "ExportVersion"),
      .keep = .keep
    ) %>%
    mutate(
      RelativeDepth = if_else(
        is.na(RelativeDepth),
        if_else(ActivityDepth_m <= 1, "Surface", "Bottom"),
        RelativeDepth
      )
    )
}