getListOfAnalytes <- function(){
  # NOTE: instead of reading all data here, we hardcode the list of analytes
  analytes <- c(
    # === subset of desired analytes:
    # higher priority:
    "Chlorophyll a",
    # "Chlorophyll a, free of pheophytin",
    # "Chlorophyll a- corrected",
    # "Chlorophyll a- uncorrected",

    "Total Nitrogen",
    # "Total Kjeldahl Nitrogen",
    # "Nitrogen, Kjeldahl, Total",
    # "Nitrogen- Total Kjeldahl",
    # "Nitrogen- Total",

    "Nitrite",
    # "Nitrite (N)",
    
    "Nitrate",
    # "Nitrate (N)",

    "Nitrate+Nitrite",
    # "NO2+3, Filtered",
    # "Nitrate-Nitrite (N)",
    # "Nitrogen, NO2 plus NO3",

    "Ammonia (N)",
    
    "Total Phosphorus",
    # "Phosphorus, Total (as P) LL",
    # "Phosphorus- Total",
   
    "Orthophosphate (P)",
    
    "Silica (SiO2)",
    
    "Silicate",
    
    "Turbidity",

    # lower priority:
    "Salinity",

    "pH",
    # "Field pH",

    "Dissolved Oxygen",
    # "Dissolved Oxygen (Discrete)",
    # "Dissolved Oxygen (CTD)",
    # "Oxygen, Dissolved",

    "Water Temperature",
    # "Temperature, Water",
    # "Temperature",
    # "Field Temperature",
    # "Temperature, Water"


    # other analytes (as reported by index.md 2025-05)
    "Ammonium",
    "Phosphate",
    "Pheophytin",
    "Specific Conductivity",
    "Fecal Coliforms",
    "Enterococci",
    "Field Specific Conductance",
    "Nitrogen, Ammonia"
  )
  analytes <- gsub("[ \\(\\)|,]", "_", analytes)

  return(analytes)
}