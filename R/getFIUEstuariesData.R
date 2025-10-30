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
      Activity.Start.Date.Time = DATE,
      Monitoring.Location.ID = SITE,
    ) %>%
    mutate(Activity.Start.Date.Time = as.Date(Activity.Start.Date.Time))
  
  # set df$RelativeDepth using _B and _S in analyte names
  df <- df %>%
    mutate(
      RelativeDepth = case_when(
        DEP.Analyte.Name %in% c("SAL_S", "TEMP_S", "DO_S") ~ "Surface",
        DEP.Analyte.Name %in% c("SAL_B", "TEMP_B", "DO_B") ~ "Bottom",
        TRUE ~ ""
      )
    )
  
  # === map analyte names to standard from unique(df$DEP.Analyte.Name))
  df <- df %>%
    mutate(DEP.Analyte.Name = recode(DEP.Analyte.Name,
           "NOX"      = "NO2+3, Filtered",
           "NO3"      = "Nitrate (NO3)",
           "NO2"      = "Nitrite (NO2)",
           "NH4"      = "Ammonium, Filtered (NH4)",
           "TN"       = "Total Nitrogen",
           "DIN"      = "Inorganic Nitrogen",
           "TON"      = "Total Kjeldahl Nitrogen",
           "TP"       = "Total Phosphorus",
           "SRP"      = "Phosphate, Filtered (PO4)",
           "APA"      = "Alkaline_Phosphatase_Activity",
           "CHLA"     = "Chlorophyll a, uncorrected for pheophytin",
           "TOC"      = "Total_Organic_Carbon",
           "SiO2"     = "Silicate",
           "TURB"     = "Turbidity",
           "pH"       = "pH",
           "Kd"       = "Diffuse_Attenuation_Coefficient",
           # surface
           "SAL_S"    = "Salinity",
           "TEMP_S"   = "Water Temperature",
           "DO_S"     = "Dissolved Oxygen",
           # bottom
           "SAL_B"    = "Salinity",
           "TEMP_B"   = "Water Temperature",
           "DO_B"     = "Dissolved Oxygen"
    ))
  
  df$`DEP.Result.Unit` = "ppm"
  
  df <- df %>%
    dplyr::mutate(
      # Convert date/time if needed
      Activity.Start.Date.Time = as.Date(
        .data$Activity.Start.Date.Time,
        format="%m/%d/%y"
      ),
      RowID = NA,
      ProgramID = NA,
      Habitat = NA,
      IndicatorID = NA,
      IndicatorName = NA,
      ParameterID = NA,
      AreaID = NA,
      ManagedAreaName = NA,
      Activity.Type = NA,
      Year = YEAR,
      Month = month(Activity.Start.Date.Time),
      Activity.Depth = NA,
      RelativeDepth = NA,
      TotalDepth_m = NA,
      MDL = NA,
      PQL = NA,
      DetectionUnit = NA,
      Value.Qualifier = NA,
      ValueQualifierSource = NA,
      Result.Comments = NA,
      SEACAR_QAQCFlagCode = NA,
      SEACAR_QAQC_Description = NA,
      Include = NA,
      MADup = NA,
      ExportVersion = NA,
      Region = NA,
      Activity.ID = NA
      
    )
  
  return(df)
}
