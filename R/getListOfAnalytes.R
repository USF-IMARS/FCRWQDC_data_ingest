getListOfAnalytes <- function(){
  # NOTE: instead of reading all data here, we hardcode the list of analytes. Other analytes will be ignored.
  analytes <- c(
    # === subset of desired analytes
    # NOTE: this should align with getData
    "Ammonium, Filtered (NH4)",
    # "Ammonia+Ammonium",
    "Ammonia, Un-ionized (NH3)",
    "Chlorophyll_a",
    "Chlorophyll a, Corrected for Pheophytin",
    "Chlorophyll a, Uncorrected for Pheophytin",
    "Dissolved Oxygen",
    "Specific Conductivity",
    # "Fecal_Coliforms",      
    # "Enterococci",    
    "Nitrite (NO2)",
    "Nitrate (NO3)",
    # "Nitrate+Nitrite",
    "Phosphate, Filtered (PO4)",
    "Total Phosphorus",
    "Pheophytin",
    "pH",
    "Salinity",
    "Silicate",
    "Water Temperature",
    "Total Nitrogen",
    "Total Kjeldahl Nitrogen",
    "Turbidity"
  )

  return(analytes)
}