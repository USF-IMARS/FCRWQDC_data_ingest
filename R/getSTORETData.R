# Required packages
library(dplyr)
library(here)
library(glue)

STORETFileToDataFrame <- function(fpath){
  # read dataframe from pipe-delimited file
  # print(glue('reading file {fpath}...'))
  df <- read.delim(
    file           = fpath,
    sep            = "|",
    header         = TRUE,
    stringsAsFactors = FALSE,
    na.strings     = c("", "NA")
  )
  df <- mapSTORETToWIN(df)

  return(df)
}

mapSTORETToWIN <- function(df){
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
    # 2) build the DateTime - using proper standard evaluation
    dplyr::mutate(
      Activity.Start.Date.Time = as.POSIXct(
        paste(.data[['Act.Date']], .data[['Act.Time']]),
        format = "%m/%d/%Y %H:%M:%S",
        tz     = "UTC"
      )
    )
  return(df)
}

# Get data from STORET historical format files (pipe-delimited)
getSTORETData <- function(programName) {
  storetPath <- here('data/STORET_historical')
  
  # read the main file
  fpath <- glue('{storetPath}/STORET_Water_Quality_Results_{programName}.txt')
  df <- STORETFileToDataFrame(fpath)
  
  if (programName == 'DERM_BBWQ') {
    # also include the 1970–1995 legacy file
    fpath2 <- glue('{storetPath}/STORET_Water_Quality_Results_{programName}_1970_1995.txt')
    df2 <- STORETFileToDataFrame(fpath2)
    
    # merge them by row
    df <- dplyr::bind_rows(df, df2)
  }
  return(df)
}
