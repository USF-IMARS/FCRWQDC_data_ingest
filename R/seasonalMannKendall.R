#Packages
library(tidyverse)
library(EnvStats)
library(wesanderson)
library(metR)
library(reshape2)

seasonalMannKendall <- function(df){
  CHLA_merged <- df
  ## select only columns we need
  CHLA_merged_x <- CHLA_merged[,c("Site", "Year", "Month", "Parameter", "Value", "Units", "Source")]
  ## remove NAs
  CHLA_merged_x_na <- na.omit(CHLA_merged_x)
  ## make sure Value column is numeric
  CHLA_merged_x_na$Value<-as.numeric(CHLA_merged_x_na$Value)
  ## pivot wider
  CHLA_wide <- pivot_wider(CHLA_merged_x_na, id_cols = c("Month", "Year"), values_from = "Value", names_from = "Site", values_fn = mean)
  ## write .csv (I do this because it's easier for me to look at data in excel)
  write.csv(CHLA_wide, "CHLA_wide.csv", row.names = F) #Don't necessarily need this step but now you'll have both datasets saved


  ## Create empty matrix to place values in during loop
  slopedf_CHLA_wide <- matrix(ncol=2, nrow=length(1:ncol(CHLA_wide)))
  colnames(slopedf_CHLA_wide) <- c("Site", "Slope")

  ## SMK Loop by site + put slopes in new df (tryCatch({}) is what allows loop to continue after error occuring)
  for (i in 3:ncol(CHLA_wide)) {
    tryCatch({
      tempdf <- data.frame(Month = CHLA_wide$Month, Year = CHLA_wide$Year, Site = CHLA_wide[,i])
      colnames(tempdf)[3] <- "Site"
      tempdf[,3] <- as.numeric(tempdf[,3])
      test <- kendallSeasonalTrendTest(Site ~ Month + Year, data = tempdf)
      Slope <- test[["estimate"]][["slope"]]
      slopedf_CHLA_wide[i,1] <- colnames(CHLA_wide[i])
      slopedf_CHLA_wide[i,2] <- Slope
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }

  #Slope as data frame and slope numeric (OMITS ALL ROWS WITH NA DUE TO ERROR/FAILING KENDALLTEST)
  site_slope<-as.data.frame(na.omit(slopedf_CHLA_wide))
  site_slope$Slope<-as.numeric(site_slope$Slope)

  #Add Programs back
  CHLA_progsite<-unique(select(CHLA_merged, Source, Site))

  CHLA_slopes<-full_join(CHLA_progsite, site_slope, by="Site")
  CHLA_slopes<-na.omit(CHLA_slopes)

  return(CHLA_slopes)
}