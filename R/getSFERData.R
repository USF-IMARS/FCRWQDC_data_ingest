# Required packages
library(dplyr)
library(here)

# Get data from SFER CSV format files
getSFERData <- function(programName) {
  fpath <- here("data/SFER_data.csv")
  df <- read.csv(fpath)
  # modify df to align with WIN standards
  source(here("R/align_sfer_df.R"))
  # Using explicit call to the function with the namespace to avoid lint errors
  df <- align_sfer_df(df)
  return(df)
}
