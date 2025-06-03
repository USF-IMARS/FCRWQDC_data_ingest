library(here)
library(dplyr)
library(tidyr)

source(here("R/getFpath.R"))



getFIUData <- function(programName=NULL, fpath=NULL) {
  fpath <- getFpath(
    programName, 
    fpath, 
    here("data/FIU_recent_all.csv")
  )

  df <- read.csv(fpath)

  # align columns
  # FIU columns:
  # Station,Date,Nitrate-Nitrite,NO2,Ammonia,TN,TP,Phosphate,Si,Chlorophyll_a

  # cast to numeric
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
      Monitoring.Location.ID = Station,
      Activity.Start.Date.Time = Date,
      program = "FIU"
    )

  return(df)
}
