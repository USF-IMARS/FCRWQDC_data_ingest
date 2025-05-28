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
  
  # Load and apply the STORET column alignment function
  source(here("R/align_storet_df.R"))
  df <- align_storet_df(df)

  return(df)
}

# Get data from STORET historical format files (pipe-delimited)
getSTORETData <- function(programName) {
  # Check if we're using test data
  if (programName == "test") {
    # Use test data file
    fpath <- here('data/test/STORET_example.csv')
  } else {
    storetPath <- here('data/STORET_historical')
    # read the main file
    fpath <- glue('{storetPath}/STORET_Water_Quality_Results_{programName}.txt')
  }
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
