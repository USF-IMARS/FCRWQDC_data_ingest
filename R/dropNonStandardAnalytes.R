source(here::here("R/getListOfAnalytes.R"))

dropNonStandardAnalytes <- function(df){
  # === drop any non-standard analyte names  
  valid_analytes <- getListOfAnalytes()
  
  df <- df %>%
    filter(DEP.Analyte.Name %in% valid_analytes)
  
  return(df)
}