getListOfAnalytes <- function(){
  # NOTE: instead of reading all data here, we hardcode the list of analytes. Other analytes will be ignored.
  analytes <- c(
    # === subset of desired analytes
    # NOTE: this should align with getData
    "Ammonium",
    "Ammonia+Ammonium",
    "Ammonia",
    "Chlorophyll_a",
    "Dissolved_Oxygen",
    # "Specific_Conductivity",
    # "Fecal_Coliforms",      
    # "Enterococci",    
    "Nitrite",
    "Nitrate",
    "Nitrate+Nitrite",
    "Orthophosphate",
    "Phosphorus",
    "Pheophytin",
    "pH",
    "Salinity",
    "Silicate",
    "Water_Temperature",  # should be water temperature (check?)
    "Total_Nitrogen",
    "Total_Kjeldahl_Nitrogen",
    "Turbidity"
  )

  return(analytes)
}