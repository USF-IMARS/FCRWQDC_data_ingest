# Required packages
library(dplyr)
library(here)
library(glue)
source(here("R/getFpath.R"))

# Get data from SFER CSV format files
getSFERData <- function(programName=NULL, fpath=NULL) {
  fpath <- getFpath(
    programName, 
    fpath, 
    here::here("data/SFER_data.csv")
  )
  if(is.null(programName)){
    programName = "SFER"
  }

  df <- read.csv(fpath)
  
  # === modify df to align with WIN standards
  # input data columns:
  # keyfield cruise_id year month day  time
  # datetime lat_deg lat_min lat_dec lon_deg lon_min  lon_dec station
  # station_type depth depth_class depth_order cast nisk_start nisk_end   temp    sal  o2_ctd o2_disc o2_disc_flag  nh4 nh4_flag  no2 no2_flag  no3 no3_flag no3_no2
  # no3_no2_flag  po4 po4_flag  si si_flag avg_chl_a avg_chl_a_flag avg_phaeo
  # avg_phaeo_flag notes
  
  # WIN standard columns
  # DEP.Result.ID Activity.ID year month day  time   Activity.Start.Date.Time lat_deg lat_min Org.Decimal.Latitude lon_deg lon_min   Org.Decimal.Longitude Monitoring.Location.ID Activity.Type Activity.Depth   Activity.Depth.Unit Activity.Depth.Top.Bottom.Unit Sample.Collection.Type   Activity.Top.Depth Activity.Bottom.Depth Value.Qualifier Result.Comments   DEP.Analyte.Name DEP.Result.Value.Number DEP.Result.Unit
  df <- df %>%
    rename(
      `Activity.ID` = `cruise_id`,
      `Activity.Start.Date.Time` = datetime,
      `Monitoring.Location.ID` = station,
      `Org.Decimal.Longitude` = lon_dec,
      `Org.Decimal.Latitude` = lat_dec,
      Activity.Depth = depth,
    )
  
  df <- df %>%
    # drop flags
    select(-ends_with("_flag")) %>%
    # ensure columns are numeric
    mutate(across(
      c(
        temp, sal, o2_ctd, o2_disc,
        nh4, no2, no3, no3_no2,
        po4, si,
        avg_chl_a, avg_phaeo
      ),
      as.numeric
    )) %>%
    # pivot analyte columns
    pivot_longer(
      cols = c(
        temp, sal, o2_ctd, o2_disc,
        nh4, no2, no3, no3_no2,
        po4, si,
        avg_chl_a, avg_phaeo
      ),
      names_to = "DEP.Analyte.Name",
      values_to = "DEP.Result.Value.Number"
    )
  
  df <- df %>%
    mutate(
      Activity.Start.Date.Time = as.Date(
        Activity.Start.Date.Time, 
      ),
      DEP.Analyte.Name = recode(
        DEP.Analyte.Name,
        "temp"             = "Water Temperature",
        "sal"              = "Salinity",
        "o2_ctd"           = "Dissolved Oxygen",
        "o2_disc"          = "Dissolved Oxygen",
        "nh4"              = "Ammonium, Filtered (NH4)",
        "no2"              = "Nitrite (NO2)",
        "no3"              = "Nitrate (NO3)",
        "no3_no2"          = "NO2+3, Filtered",
        "po4"              = "Phosphate, Filtered (PO4)",
        "si"               = "Silicate",
        "avg_chl_a"        = "Chlorophyll a, corrected for pheophytin",
        "avg_phaeo"        = "Pheophytin",
        "notes"            = "Notes",
        
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
      Year = year,
      Month = month,
      RelativeDepth = depth_class,
      TotalDepth_m = NA,
      MDL = NA,
      PQL = NA,
      DetectionUnit = NA,
      Value.Qualifier = NA,
      ValueQualifierSource = NA,
      Result.Comments = notes,
      SEACAR_QAQCFlagCode = NA,
      SEACAR_QAQC_Description = NA,
      Include = NA,
      MADup = NA,
      ExportVersion = NA,
      Region = NA,
      Activity.ID = NA
    )
  
  df$DEP.Result.Unit = NA
  return(df)
}
