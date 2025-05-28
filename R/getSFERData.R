# Required packages
library(dplyr)
library(here)

# Get data from SFER CSV format files
getSFERData <- function(programName) {
  # Check if we're using test data
  if (programName == "test") {
    fpath <- here("data/test/SFER_example.csv")
  } else {
    fpath <- here("data/SFER_data.csv")
  }
  df <- read.csv(fpath)
  # modify df to align with WIN standards
  source(here("R/align_sfer_df.R"))
  # Using explicit call to the function with the namespace to avoid lint errors
  df <- align_sfer_df(df)
  return(df)
}
