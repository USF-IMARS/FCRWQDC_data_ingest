library(readxl)
library(here)
library(tidyr)
library(dplyr)

getFIUHistoricalData <- function(){
  fname <- here::here('data', 'WQFloridaKeys_Shelf__ppm__UPDATED_2-9-2023.xlsx')
  

  # Read full sheet
  df <- read_excel(fname, sheet = "Data in ppm")
  
  # analytes from columns into rows (wide to long)
  df <- df %>%
    pivot_longer(
      cols = -c(SURV, BASIN, SEGMENT, ZONE, DATE, TIME,
                STATION, SITE, LATDEC, LONDEC, DEPTH),
      names_to = "DEP.Analyte.Name",
      values_to = "DEP.Result.Value.Number"
    )
  
  # === map columns
  # from input data:
  # SURV	BASIN	SEGMENT	ZONE	DATE	TIME	STATION	SITE	LATDEC	LONDEC	DEPTH	NOX-S	NOX-B	NO3_S	NO3_B	NO2-S	NO2-B	NH4-S	NH4-B	TN-S	TN-B	DIN-S	DIN-B	TON-S	TON-B	TP-S	TP-B	SRP-S	SRP-B	APA-S	APA-B	CHLA-S	CHLA-B	TOC-S	TOC-B	SiO2-S	SiO2-B	TURB-S	TURB-B	SAL-S	SAL-B	TEMP-S	TEMP-B	DO-S	DO-B	Kd	pH	TN:TP	N:P	DIN:TP	Si:DIN	%SAT-S	%SAT_B	%Io	DSIGT
  
  # WIN standard columns
  # DEP.Result.ID Activity.ID year month day  time   Activity.Start.Date.Time lat_deg lat_min Org.Decimal.Latitude lon_deg lon_min   Org.Decimal.Longitude Monitoring.Location.ID Activity.Type Activity.Depth   Activity.Depth.Unit Activity.Depth.Top.Bottom.Unit Sample.Collection.Type   Activity.Top.Depth Activity.Bottom.Depth Value.Qualifier Result.Comments   DEP.Analyte.Name DEP.Result.Value.Number DEP.Result.Unit
  
  df <- df %>%
    rename(
      Org.Decimal.Latitude = LATDEC,
      Org.Decimal.Longitude = LONDEC,
      Activity.Depth = DEPTH,
      time = TIME,
      Activity.Start.Date.Time = DATE,
      Monitoring.Location.ID = STATION,  # or SITE, depending on usage
    )
  
  # === map analyte names to standard from unique(df$DEP.Analyte.Name))
  df <- df %>%
    mutate(DEP.Analyte.Name = recode(DEP.Analyte.Name,
     "NOX-S"   = "Nitrate+Nitrite",
     "NO3_S"   = "Nitrate",
     "NO2-S"   = "Nitrite",
     "NH4-S"   = "Ammonium",
     "TN-S"    = "Total_Nitrogen",
     "DIN-S"   = "Dissolved_Inorganic_Nitrogen",
     "TON-S"   = "Total_Kjeldahl_Nitrogen",
     "TP-S"    = "Phosphorus",
     "SRP-S"   = "Orthophosphate",
     "APA-S"   = "Alkaline_Phosphatase_Activity",
     "CHLA-S"  = "Chlorophyll_a",
     "TOC-S"   = "Total_Organic_Carbon",
     "SiO2-S"  = "Silicate",
     "TURB-S"  = "Turbidity",
     "SAL-S"   = "Salinity",
     "TEMP-S"  = "Temperature",
     "DO-S"    = "Dissolved_Oxygen",
     "Kd"      = "Diffuse_Attenuation_Coefficient",
     "pH"      = "pH",
     "TN:TP"   = "TN_to_TP_Ratio",
     "N:P"     = "N_to_P_Ratio",
     "DIN:TP"  = "DIN_to_TP_Ratio",
     "Si:DIN"  = "Si_to_DIN_Ratio",
     "%SAT-S"  = "Oxygen_Saturation",
     "%Io"     = "Surface_Light_Penetration",
     "DSIGT"   = "Sigma_T_Density_Difference"
    ),
    Activity.Start.Date.Time = as.Date(Activity.Start.Date.Time)
  )
  
  return(df)
}