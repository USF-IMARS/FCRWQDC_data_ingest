getListOfAnalytes <- function(){
    # NOTE: instead of reading all data here, we hardcode the list of analytes
    analytes <- c(
        "Ammonia (N)", 
        "Nitrite (NO2)", 
        "Nitrate (NO3)", 
        "Nitrate + Nitrite (NO3 + NO2)", 
        "Orthophosphate (PO4)", 
        "Silicate (Si)", 
        "Chlorophyll a (chl-a)", 
        "Phytoplankton (phaeo)",
        "Turbidity",
        "Nitrogen, Ammonia",
        "Nitrogen, Kjeldahl, Total",
        "Nitrogen, NO2 plus NO3",
        "Phosphorus, Total (as P) LL",
        "Salinity",
        "Oxygen, Dissolved",
        "Field pH",
        "Field Temperature"
    )
    return(analytes)
}