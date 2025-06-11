getListOfAnalytes <- function(){
  # NOTE: instead of reading all data here, we hardcode the list of analytes
  analytes <- c(
    # === subset of desired analytes
    # NOTE: this should align with getData
    "Ammonium",
    "Ammonia__N",
    "Chlorophyll_a",
    "Dissolved_Oxygen",
    "Specific_Conductivity",
    "Fecal_Coliforms",
    "Enterococci",
    "Nitrogen__Ammonia",
    "Nitrate",
    "Nitrite",
    "Nitrate+Nitrite",
    "Nitrogen__ammonia__NH3__+_ammonium__NH4_",
    "Nitrogen__ammonia_as_N",
    "Nitrogen__ammonia__NH3__as_NH3",
    "Total_Nitrogen",
    "Orthophosphate",
    "Phosphorus",
    "Pheophytin",
    "Temperature",
    "Total_Nitrogen",
    "Total_Kjeldahl_Nitrogen",
    "Silicate",
    "Turbidity",
    "pH",
    "Salinity"
  )

  return(analytes)
}