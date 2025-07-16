library(here)
library(dplyr)
library(tidyr)


getBBWWData <- function() {
  fpath <- here("data", "BBWW_DiscreteWQ4057.txt")
  
  df <- read.csv(
    fpath,
    sep="|"
  )
  
  # === align columns
  # input columns:
  # RowID|ProgramID|ProgramName|Habitat|IndicatorID|IndicatorName|ParameterID|ParameterName|ParameterUnits|ProgramLocationID|AreaID|ManagedAreaName|Region|ActivityType|SampleDate|ResultValue|Year|Month|ActivityDepth_m|RelativeDepth|TotalDepth_m|MDL|PQL|DetectionUnit|ValueQualifier|ValueQualifierSource|SampleFraction|ResultComments|OriginalLatitude|OriginalLongitude|SEACAR_QAQCFlagCode|SEACAR_QAQC_Description|Include|SEACAR_EventID|MADup|ExportVersion

  # standard columns to align to:
  # DEP.Result.ID Activity.ID year month day  time   Activity.Start.Date.Time lat_deg lat_min Org.Decimal.Latitude lon_deg lon_min   Org.Decimal.Longitude Monitoring.Location.ID Activity.Type Activity.Depth   Activity.Depth.Unit Activity.Depth.Top.Bottom.Unit Sample.Collection.Type   Activity.Top.Depth Activity.Bottom.Depth Value.Qualifier Result.Comments   DEP.Analyte.Name DEP.Result.Value.Number DEP.Result.Unit
  
  df <- df %>%
    rename(
      `DEP.Result.ID`                   = RowID,
      `Activity.ID`                     = SEACAR_EventID,
      year                              = Year,
      month                             = Month,
      `Activity.Start.Date.Time`       = SampleDate,
      `Org.Decimal.Latitude`           = OriginalLatitude,
      `Org.Decimal.Longitude`          = OriginalLongitude,
      `Monitoring.Location.ID`         = ProgramLocationID,
      `Activity.Type`                  = ActivityType,
      `Activity.Depth`                 = ActivityDepth_m,
      `DEP.Analyte.Name`               = ParameterName,
      `DEP.Result.Value.Number`        = ResultValue,
      `DEP.Result.Unit`                = ParameterUnits,
      `Value.Qualifier`                = ValueQualifier,
      `Result.Comments`                = ResultComments
    ) %>%    
    dplyr::mutate(
      # Convert date/time if needed
      Activity.Start.Date.Time = as.Date(.data$Activity.Start.Date.Time)
    )
  
  return(df)
}
