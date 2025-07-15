library(readxl)
library(here)
library(tidyr)
library(dplyr)

getFIUEstuariesData <- function(){
  fname <- here::here('data', 'SouthFloridaEstuariesWQ_ppm.xlsx')
  
  
  # Read full sheet
  df <- read_excel(fname, sheet = "Data ppm")
  
  # analytes from columns into rows (wide to long)
  df <- df %>%
    pivot_longer(
      cols = c(NOX, NO3, NO2, NH4, TN, DIN, TON, TP, SRP, APA,
               CHLA, TOC, SiO2, SAL_S, SAL_B, TEMP_S, TEMP_B,
               DO_S, DO_B, TURB, pH, Kd),
      names_to = "DEP.Analyte.Name",
      values_to = "DEP.Result.Value.Number"
    )
  
  # === map columns
  # from input data:
  # SITE	BASIN	CLUSTER	ZSI	ZONE	LATDEC	LONDEC	DEPTH	YEAR	NOX	NO3	NO2	NH4	TN	DIN	TON	TP	SRP	APA	CHLA	TOC	SiO2	SAL_S	SAL_B	TEMP_S	TEMP_B	DO_S	DO_B	TURB	pH	Kd	DETECTION LIMITS	NOX DL	NO3 DL	NO2 DL	NH4 DL	TN DL	DIN DL	TON DL	TP DL	SRP DL	APA DL	CHLA DL	TOC DL	SiO2 DL
  
  # WIN standard columns
  # DEP.Result.ID Activity.ID year month day  time   Activity.Start.Date.Time lat_deg lat_min Org.Decimal.Latitude lon_deg lon_min   Org.Decimal.Longitude Monitoring.Location.ID Activity.Type Activity.Depth   Activity.Depth.Unit Activity.Depth.Top.Bottom.Unit Sample.Collection.Type   Activity.Top.Depth Activity.Bottom.Depth Value.Qualifier Result.Comments   DEP.Analyte.Name DEP.Result.Value.Number DEP.Result.Unit
  
  df <- df %>%
    rename(
      Org.Decimal.Latitude = LATDEC,
      Org.Decimal.Longitude = LONDEC,
      Activity.Depth = DEPTH,
      Activity.Start.Date.Time = YEAR,
      Monitoring.Location.ID = SITE,
    ) %>%
    mutate(Activity.Start.Date.Time = paste0(Activity.Start.Date.Time, "-01-01"))
  
  
  # === map analyte names to standard from unique(df$DEP.Analyte.Name))
  df <- df %>%
    mutate(DEP.Analyte.Name = recode(DEP.Analyte.Name,
           "NOX"      = "Nitrate+Nitrite",
           "NO3"      = "Nitrate",
           "NO2"      = "Nitrite",
           "NH4"      = "Ammonium",
           "TN"       = "Total_Nitrogen",
           "DIN"      = "Dissolved_Inorganic_Nitrogen",
           "TON"      = "Total_Kjeldahl_Nitrogen",
           "TP"       = "Phosphorus",
           "SRP"      = "Orthophosphate",
           "APA"      = "Alkaline_Phosphatase_Activity",
           "CHLA"     = "Chlorophyll_a",
           "TOC"      = "Total_Organic_Carbon",
           "SiO2"     = "Silicate",
           "SAL_S"    = "Salinity",
           "TEMP_S"   = "Temperature",
           "DO_S"     = "Dissolved_Oxygen",
           "TURB"     = "Turbidity",
           "pH"       = "pH",
           "Kd"       = "Diffuse_Attenuation_Coefficient"
    ))
  
  df$`DEP.Result.Unit` = "ppm"
  return(df)
}