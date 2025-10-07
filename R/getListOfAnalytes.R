getListOfAnalytes <- function(){
  # NOTE: instead of reading all data here, we hardcode the list of analytes. Other analytes will be ignored.
  analytes <- c(
    # === subset of desired analytes (SEACAR vocab)
    "Ammonia, Un-ionized (NH3)",
    "Ammonium, Filtered (NH4)",
    "Nitrate (NO3)",
    "Nitrite (NO2)",
    "Nitrogen, organic",
    "NO2+3, Filtered",
    "Phosphate, Filtered (PO4)",
    "Total Kjeldahl Nitrogen",
    "Total Nitrogen",
    "Total Phosphorus",
    "Chlorophyll a, Corrected for Pheophytin",
    "Chlorophyll a, Uncorrected for Pheophytin",
    # "Colored Dissolved Organic Matter",
    # "Fluorescent Dissolved Organic Matter",
    # "Light Extinction Coefficient",
    # "Secchi Depth",
    "Total Suspended Solids",
    "Turbidity",
    "Dissolved Oxygen", 
    "Dissolved Oxygen Saturation",
    "pH",
    "Salinity",
    "Specific Conductivity",
    "Water Temperature"
  )

  return(analytes)
}