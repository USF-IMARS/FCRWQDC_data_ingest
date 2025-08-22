library(readxl)
library(here)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)

getFBBBData <- function(){
  fname <- here::here(
    'data', 
    'AOML_Florida_Bay_and_Biscayne_Bay_Data_1998-2017.xlsx'
  )
  
  
  # Read full sheet
  df <- read_excel(
    fname, 
    sheet = "1996-2016", 
    col_types = "text"
  )
  
  # analytes from columns into rows (wide to long)
  df <- df %>%
    pivot_longer(
      cols = c(Temp, Salinity, Zp, Zcol, `% Xmis`, `CDOM QSU`, `Tripton (mg/L)`, NH4, PO4, `N+N`, NO2, NO3, Si, TDP, DOP, pH, TDN, DON, DIN, DIC, `Chl a (µg/L)`, `Phaeo (µg/L)`, Kt, `TSS (mg/L)`),
      names_to = "DEP.Analyte.Name",
      values_to = "DEP.Result.Value.Number"
    )
  
  # set DEP.Result.Unit from DEP.Analyte.Name where appropriate
  # & drop unit from DEP.Analyte.Name
  df <- df %>%
    mutate(
      # Assign units based on matching patterns
      DEP.Result.Unit = case_when(
        str_detect(DEP.Analyte.Name, fixed("Tripton (mg/L)")) ~ "mg/L",
        str_detect(DEP.Analyte.Name, fixed("Chl a (µg/L)"))   ~ "µg/L",
        str_detect(DEP.Analyte.Name, fixed("Phaeo (µg/L)"))   ~ "µg/L",
        str_detect(DEP.Analyte.Name, fixed("TSS (mg/L)"))     ~ "mg/L",
        TRUE ~ NA_character_
      ),
      # Clean up analyte names by removing units
      DEP.Analyte.Name = str_remove(DEP.Analyte.Name, fixed(" (mg/L)")) %>%
        str_remove(fixed(" (µg/L)"))
    )
  
  
  # === map columns
  # from input data:
  # Record #	Cruise ID	Date	Station	Region	Long Deg	Long Min	Longitude	Lat Deg	Lat Min	Latitude	Temp	Salinity	Zp	Zcol	% Xmis	CDOM QSU	Tripton (mg/L)	NH4	PO4	N+N   	NO2	NO3	Si	TDP	DOP	pH	TDN	DON	DIN	DIC	Chl a (µg/L)	Phaeo (µg/L)	Kt	TSS (mg/L) 
  
  # WIN standard columns
  # DEP.Result.ID Activity.ID year month day  time   Activity.Start.Date.Time lat_deg lat_min Org.Decimal.Latitude lon_deg lon_min   Org.Decimal.Longitude Monitoring.Location.ID Activity.Type Activity.Depth   Activity.Depth.Unit Activity.Depth.Top.Bottom.Unit Sample.Collection.Type   Activity.Top.Depth Activity.Bottom.Depth Value.Qualifier Result.Comments   DEP.Analyte.Name DEP.Result.Value.Number DEP.Result.Unit
  
  df <- df %>%
    rename(
      `DEP.Result.ID` = `Record #`,
      `Activity.ID` = `Cruise ID`,
      `Activity.Start.Date.Time` = Date,
      `Monitoring.Location.ID` = Station,
      `Org.Decimal.Longitude` = Longitude,
      `Org.Decimal.Latitude` = Latitude,
    )
  
  # === map analyte names to standard from unique(df$DEP.Analyte.Name))
  df <- df %>%
    mutate(
      Activity.Start.Date.Time = as.Date(
        Activity.Start.Date.Time, 
        format="%m/%d/%y"
      ),
      DEP.Analyte.Name = recode(
        DEP.Analyte.Name,
        "NH4"       = "Ammonium",
        "N+N"       = "Nitrate+Nitrite",
        "NO2"       = "Nitrite",
        "NO3"       = "Nitrate",
        "PO4"       = "Orthophosphate",
        "TDP"       = "Phosphorus",
        "Phaeo"     = "Pheophytin",
        "Chl a"     = "Chlorophyll_a",
        "Salinity"  = "Salinity",
        "Si"        = "Silicate",
        "Temp"      = "Temperature",
        "TDN"       = "Total_Nitrogen",
        "DON"       = "Total_Kjeldahl_Nitrogen",
        "TSS"       = "Turbidity",
        "pH"        = "pH"
      ),
      # columsn for STORET mapping
      RowID = NA,
      ProgramID = NA,
      Habitat = NA,
      IndicatorID = NA,
      IndicatorName = NA,
      ParameterID = NA,
      AreaID = NA,
      ManagedAreaName = NA,
      Activity.Type = NA,
      Year = year(Activity.Start.Date.Time),
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
      ExportVersion = NA
    )
  
  return(df)
}
