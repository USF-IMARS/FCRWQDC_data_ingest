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
    mutate(
      Activity.Depth = as.character(DEPTH),
      time = as.character(TIME),
      Activity.Start.Date.Time = as.character(DATE),
      Monitoring.Location.ID = as.character(STATION), # for short code
      # Monitoring.Location.ID = as.character(SITE),  # for human readable name
      Org.Decimal.Latitude = as.character(LATDEC),
      Org.Decimal.Longitude = as.character(LONDEC)
    )
  
  # set df$RelativeDepth using _B and _S in analyte names
  df <- df %>%
    mutate(
      RelativeDepth = case_when(
        DEP.Analyte.Name %in% c(
          "%SAT-S",
          "NOX-S",
          "NO3_S",
          "NO2-S",
          "NH4-S",
          "TN-S",
          "DIN-S",
          "TON-S",
          "TP-S",
          "SRP-S",
          "APA-S",
          "CHLA-S",
          "TOC-S",
          "SiO2-S",
          "TURB-S",
          "SAL-S",
          "TEMP-S",
          "DO-S"
        ) ~ "Surface",
        DEP.Analyte.Name %in% c(
          "%SAT-B",
          "NOX-B",
          "NO3_B",
          "NO2-B",
          "NH4-B",
          "TN-B",
          "DIN-B",
          "TON-B",
          "TP-B",
          "SRP-B",
          "APA-B",
          "CHLA-B",
          "TOC-B",
          "SiO2-B",
          "TURB-B",
          "SAL-B",
          "TEMP-B",
          "DO-B"
        ) ~ "Bottom",
        TRUE ~ ""
      )
    )
  
  # === map analyte names to standard from unique(df$DEP.Analyte.Name))
  df <- df %>%
    mutate(DEP.Analyte.Name = recode(DEP.Analyte.Name,
     "Kd"      = "Diffuse_Attenuation_Coefficient",
     "pH"      = "pH",
     "TN:TP"   = "TN_to_TP_Ratio",
     "N:P"     = "N_to_P_Ratio",
     "DIN:TP"  = "DIN_to_TP_Ratio",
     "Si:DIN"  = "Si_to_DIN_Ratio",
     "%Io"     = "Surface_Light_Penetration",
     "DSIGT"   = "Sigma_T_Density_Difference",
     
     # surface
     "%SAT-S"  = "Dissolved Oxygen Saturation",
     "NOX-S"   = "NO2+3, Filtered",
     "NO3_S"   = "Nitrate (NO3)",
     "NO2-S"   = "Nitrite (NO2)",
     "NH4-S"   = "Ammonium, Filtered (NH4)",
     "TN-S"    = "Total Nitrogen",
     "DIN-S"   = "Inorganic Nitrogen",
     "TON-S"   = "Nitrogen, organic",
     "TP-S"    = "Total Phosphorus",
     "SRP-S"   = "Phosphate, Filtered (PO4)",
     "APA-S"   = "Alkaline_Phosphatase_Activity",
     "CHLA-S"  = "Chlorophyll a, Uncorrected for Pheophytin",
     "TOC-S"   = "Total_Organic_Carbon",
     "SiO2-S"  = "Silicate",
     "TURB-S"  = "Turbidity",
     "SAL-S"   = "Salinity",
     "TEMP-S"  = "Water Temperature",
     "DO-S"    = "Dissolved Oxygen",
     
     # bottom
     "%SAT-B"  = "Dissolved Oxygen Saturation",
     "NOX-B"   = "NO2+3, Filtered",
     "NO3_B"   = "Nitrate (NO3)",
     "NO2-"   = "Nitrite (NO2)",
     "NH4-B"   = "Ammonium, Filtered (NH4)",
     "TN-B"    = "Total Nitrogen",
     "DIN-B"   = "Inorganic Nitrogen",
     "TON-B"   = "Nitrogen, organic",
     "TP-B"    = "Total Phosphorus",
     "SRP-B"   = "Phosphate, Filtered (PO4)",
     "APA-B"   = "Alkaline_Phosphatase_Activity",
     "CHLA-B"  = "Chlorophyll a, Uncorrected for Pheophytin",
     "TOC-B"   = "Total_Organic_Carbon",
     "SiO2-B"  = "Silicate",
     "TURB-B"  = "Turbidity",
     "SAL-B"   = "Salinity",
     "TEMP-B"  = "Water Temperature",
     "DO-B"    = "Dissolved Oxygen"
     
    ),
    Activity.Start.Date.Time = as.Date(Activity.Start.Date.Time)
  )
  
  # xlsx says all units are ppm
  df$DEP.Result.Unit <- "ppm"
  
  return(df)
}