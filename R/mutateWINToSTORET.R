library(dplyr)

mutateWINToSTORET <- function(df, .keep = "none"){
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
      RowID = RowID,
      ProgramID = ProgramID,
      
      ProgramName = program,

      Habitat = Habitat,

      IndicatorID = IndicatorID,
      
      IndicatorName = IndicatorName,
      
      ParameterID = ParameterID,
      
      ParameterName = DEP.Analyte.Name,
      
      ParameterUnits = DEP.Result.Unit,
      
      ProgramLocationID = Monitoring.Location.ID,
      
      AreaID = AreaID,
      
      ManagedAreaName = ManagedAreaName,
      
      Region = Region,
      
      ActivityType = Activity.Type,
      
      
      SampleDate = Activity.Start.Date.Time,
      
      ResultValue = DEP.Result.Value.Number,
      
      Year = Year,
      
      Month = Month,
      
      ActivityDepth_m = Activity.Depth,
      
      RelativeDepth = RelativeDepth,
      TotalDepth_m = TotalDepth_m,
      
      MDL = MDL,
      
      PQL = PQL,
      
      DetectionUnit = DetectionUnit,
      
      ValueQualifier = Value.Qualifier,
      
      ValueQualifierSource = ValueQualifierSource,
      
      ResultComments = Result.Comments,
      
      OriginalLatitude = Org.Decimal.Latitude,
      OriginalLongitude = Org.Decimal.Longitude,
      
      SEACAR_QAQCFlagCode = SEACAR_QAQCFlagCode,
      
      SEACAR_QAQC_Description = SEACAR_QAQC_Description,
      
      Include = Include,
      
      SEACAR_EventID = Activity.ID,
      
      MADup = MADup,
      
      ExportVersion = ExportVersion,
      
      .keep = .keep)
}