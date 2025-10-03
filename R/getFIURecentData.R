library(here)
library(dplyr)
library(tidyr)


getFIURecentData <- function(programName=NULL, fpath=NULL) {
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
      Activity.Start.Date.Time = as.Date(
        Date, 
        format = "%m/%d/%Y %H:%M"
      ),
      Organization.ID = "FIU_WQMP",
      # map analytes
      DEP.Analyte.Name = recode(
        DEP.Analyte.Name,
        "Ammonia"             = "Ammonia, Un-ionized (NH3)",
        "NO2" = "Nitrite (NO2)",
        "Nitrate.Nitrite" = "NO2+3, Filtered",
        "Phosphate" = "Phosphate, Filtered (PO4)",
        "TN" = "Total Nitrogen",
        "TP" = "Total Phosphorus"
      )
    )
  return(df)
}
