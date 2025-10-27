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
  # It is noted below how many rows may have information to contribute.
  df %>%
    mutate(
      # mapping from WIN column names to SEACAR names
      ProgramName = program,
      ParameterName = DEP.Analyte.Name,
      ProgramLocationID = Monitoring.Location.ID,
      ActivityType = Activity.Type,
      SampleDate = Activity.Start.Date.Time,
      ResultValue = DEP.Result.Value.Number,
      ActivityDepth_m = Activity.Depth,
      ValueQualifier = Value.Qualifier,
      ResultComments = Result.Comments,
      OriginalLatitude = Org.Decimal.Latitude,
      OriginalLongitude = Org.Decimal.Longitude,
      
      # unchanged values. these are likely only filled for data being
      # loaded from SEACAR historical data files.
      # There is a chance of collision here, which would only be a problem
      # if the vocabulary of the two data sources was different for the two
      # columns with the same name.
      RowID = RowID,
      ProgramID = ProgramID,
      SEACAR_EventID = SEACAR_EventID,
      Habitat = Habitat,
      IndicatorID = IndicatorID,
      IndicatorName = IndicatorName,
      ParameterID = ParameterID,
      ParameterUnits = DEP.Result.Unit,
      AreaID = AreaID,
      ManagedAreaName = ManagedAreaName,
      Region = Region,
      Year = Year,
      Month = Month,
      RelativeDepth = RelativeDepth,
      TotalDepth_m = TotalDepth_m,
      MDL = MDL,
      PQL = PQL,
      DetectionUnit = DetectionUnit,
      ValueQualifierSource = ValueQualifierSource,
      SEACAR_QAQCFlagCode = SEACAR_QAQCFlagCode,
      SEACAR_QAQC_Description = SEACAR_QAQC_Description,
      Include = Include,
      MADup = MADup,
      ExportVersion = ExportVersion,
      .keep = .keep) %>%
    # set df$RelativeDepth using df$ActivityDepth
    mutate(
      RelativeDepth = if_else(
        is.na(RelativeDepth),  # do not replace existing RelativeDepth values
        if_else(ActivityDepth_m <= 1, "Surface", "Bottom"),
        RelativeDepth
      )
    )
}