library(here)
library(dplyr)
library(tidyr)


getFIUData <- function(programName=NULL, fpath=NULL) {
  fpath <- here("data", "FIU_recent_all.csv")

  df <- read.csv(fpath)

  # align columns
  # FIU columns:
  # Station,Date,Nitrate-Nitrite,NO2,Ammonia,TN,TP,Phosphate,Si,Chlorophyll_a
  df <- df %>%
    mutate(across(c("Nitrate.Nitrite", "NO2", "Ammonia", "TN", "TP", "Phosphate", "Si", "Chlorophyll_a"), as.numeric))

  # pivot long
  df <- df %>%
    pivot_longer(
      cols = c("Nitrate.Nitrite", "NO2", "Ammonia", "TN", "TP", "Phosphate", "Si", "Chlorophyll_a"),
      names_to = "DEP.Analyte.Name",
      values_to = "DEP.Result.Value.Number"
    )

  df <- df %>%
    mutate(
      Monitoring.Location.ID = as.character(Station),
      # format date from `%m/%d/%Y %H:%M` to `%Y-%m-%d %H:%M:%S`
      Activity.Start.Date.Time = format(
        as.POSIXct(Date, format = "%m/%d/%Y %H:%M"),
        "%Y/%m/%d %H:%M:%S"),
      datetime = Activity.Start.Date.Time,
      Organization.ID = "FIU_WQMP",
      program = "FIU_WQMP"
    )
  return(df)
}
