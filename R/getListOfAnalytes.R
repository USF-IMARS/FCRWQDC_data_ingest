getListOfAnalytes <- function(){
  # NOTE: instead of reading all data here, we hardcode the list of analytes
  analytes <- c(
    # === subset of desired analytes
    # NOTE: this should align with getData
    "Ammonium",
    "Ammonia_plus_Ammonium",
    "Ammonia",
    "Chlorophyll_a",
    "Dissolved_Oxygen",
    "Specific_Conductivity",
    "Fecal_Coliforms",
    "Enterococci",
    "Nitrite",
    "Nitrate",
    "Nitrate+Nitrite",
    "Orthophosphate",
    "Phosphorus",
    "Pheophytin",
    "pH",
    "Salinity",
    "Silicate",
    "Temperature",
    "Total_Nitrogen",
    "Total_Kjeldahl_Nitrogen",
    "Turbidity"
  )

  return(analytes)
}