### CHLA ###

rm(list=ls())

#Packages
library(tidyverse)
library(EnvStats)
library(wesanderson)
library(metR)
library(reshape2)

#Data Setup from Milly
#Set working directory to wear you have chla_merged.csv located
setwd("//vmware-host/Shared Folders/Desktop/Work/csv data for R/DEP_Yr2")

## read file
CHLA_merged <- read.csv("CHLA_merged.csv")
#Added to fix source typing
colnames(CHLA_merged)[1]<-"Source"  #Fixes weird colname after reading in

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

write.csv(CHLA_slopes, "CHLA Slopes.csv", row.names = F)



### NOX ###


rm(list=ls())

#Packages
library(tidyverse)
library(EnvStats)
library(wesanderson)
library(metR)
library(reshape2)

#Data Setup from Milly
#Set working directory to wear you have nox_merged.csv located
setwd("//vmware-host/Shared Folders/Desktop/Work/csv data for R/DEP_Yr2")

## read file
NOX_merged <- read.csv("NOX_merged.csv")
#Added to fix source typing
colnames(NOX_merged)[1]<-"Source"  #Fixes weird colname after reading in

## select only columns we need
NOX_merged_x <- NOX_merged[,c("Site", "Year", "Month", "Parameter", "Value", "Units", "Source")]
## remove NAs
NOX_merged_x_na <- na.omit(NOX_merged_x)
## make sure Value column is numeric
NOX_merged_x_na$Value<-as.numeric(NOX_merged_x_na$Value)
## pivot wider
NOX_wide <- pivot_wider(NOX_merged_x_na, id_cols = c("Month", "Year"), values_from = "Value", names_from = "Site", values_fn = mean)
## write .csv (I do this because it's easier for me to look at data in excel)
write.csv(NOX_wide, "NOX_wide.csv", row.names = F) #Don't necessarily need this step but now you'll have both datasets saved


## Create empty matrix to place values in during loop
slopedf_NOX_wide <- matrix(ncol=2, nrow=length(1:ncol(NOX_wide)))
colnames(slopedf_NOX_wide) <- c("Site", "Slope")

## SMK Loop by site + put slopes in new df (tryCatch({}) is what allows loop to continue after error occuring)
for (i in 3:ncol(NOX_wide)) {
  tryCatch({
    tempdf <- data.frame(Month = NOX_wide$Month, Year = NOX_wide$Year, Site = NOX_wide[,i])
    colnames(tempdf)[3] <- "Site"
    tempdf[,3] <- as.numeric(tempdf[,3])
    test <- kendallSeasonalTrendTest(Site ~ Month + Year, data = tempdf)
    Slope <- test[["estimate"]][["slope"]]
    slopedf_NOX_wide[i,1] <- colnames(NOX_wide[i])
    slopedf_NOX_wide[i,2] <- Slope
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#Slope as data frame and slope numeric (OMITS ALL ROWS WITH NA DUE TO ERROR/FAILING KENDALLTEST)
site_slope<-as.data.frame(na.omit(slopedf_NOX_wide))
site_slope$Slope<-as.numeric(site_slope$Slope)

#Add Programs back
NOX_progsite<-unique(select(NOX_merged, Source, Site))

NOX_slopes<-full_join(NOX_progsite, site_slope, by="Site")
NOX_slopes<-na.omit(NOX_slopes)

write.csv(NOX_slopes, "NOX Slopes.csv", row.names = F)



### PO4 ###

rm(list=ls())

#Packages
library(tidyverse)
library(EnvStats)
library(wesanderson)
library(metR)
library(reshape2)

#Data Setup from Milly
#Set working directory to wear you have chla_merged.csv located
setwd("//vmware-host/Shared Folders/Desktop/Work/csv data for R/DEP_Yr2")

## read file
po4_merged <- read.csv("po4_merged.csv")
#Added to fix source typing
colnames(po4_merged)[1]<-"Source"  #Fixes weird colname after reading in

## select only columns we need
po4_merged_x <- po4_merged[,c("Site", "Year", "Month", "Parameter", "Value", "Units", "Source")]
## remove NAs
po4_merged_x_na <- na.omit(po4_merged_x)
## make sure Value column is numeric
po4_merged_x_na$Value<-as.numeric(po4_merged_x_na$Value)
## pivot wider
po4_wide <- pivot_wider(po4_merged_x_na, id_cols = c("Month", "Year"), values_from = "Value", names_from = "Site", values_fn = mean)
## write .csv (I do this because it's easier for me to look at data in excel)
write.csv(po4_wide, "po4_wide.csv", row.names = F) #Don't necessarily need this step but now you'll have both datasets saved


## Create empty matrix to place values in during loop
slopedf_po4_wide <- matrix(ncol=2, nrow=length(1:ncol(po4_wide)))
colnames(slopedf_po4_wide) <- c("Site", "Slope")

## SMK Loop by site + put slopes in new df (tryCatch({}) is what allows loop to continue after error occuring)
for (i in 3:ncol(po4_wide)) {
  tryCatch({
    tempdf <- data.frame(Month = po4_wide$Month, Year = po4_wide$Year, Site = po4_wide[,i])
    colnames(tempdf)[3] <- "Site"
    tempdf[,3] <- as.numeric(tempdf[,3])
    test <- kendallSeasonalTrendTest(Site ~ Month + Year, data = tempdf)
    Slope <- test[["estimate"]][["slope"]]
    slopedf_po4_wide[i,1] <- colnames(po4_wide[i])
    slopedf_po4_wide[i,2] <- Slope
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#Slope as data frame and slope numeric (OMITS ALL ROWS WITH NA DUE TO ERROR/FAILING KENDALLTEST)
site_slope<-as.data.frame(na.omit(slopedf_po4_wide))
site_slope$Slope<-as.numeric(site_slope$Slope)

#Add Programs back
po4_progsite<-unique(select(po4_merged, Source, Site))

po4_slopes<-full_join(po4_progsite, site_slope, by="Site")
po4_slopes<-na.omit(po4_slopes)

write.csv(po4_slopes, "po4 Slopes.csv", row.names = F)






### Si ###


rm(list=ls())

#Packages
library(tidyverse)
library(EnvStats)
library(wesanderson)
library(metR)
library(reshape2)

#Data Setup from Milly
#Set working directory to wear you have chla_merged.csv located
setwd("//vmware-host/Shared Folders/Desktop/Work/csv data for R/DEP_Yr2")

## read file
si_merged <- read.csv("si_merged.csv")
#Added to fix source typing
colnames(si_merged)[1]<-"Source"  #Fixes weird colname after reading in

## select only columns we need
si_merged_x <- si_merged[,c("Site", "Year", "Month", "Parameter", "Value", "Units", "Source")]
## remove NAs
si_merged_x_na <- na.omit(si_merged_x)
## make sure Value column is numeric
si_merged_x_na$Value<-as.numeric(si_merged_x_na$Value)
## pivot wider
si_wide <- pivot_wider(si_merged_x_na, id_cols = c("Month", "Year"), values_from = "Value", names_from = "Site", values_fn = mean)
## write .csv (I do this because it's easier for me to look at data in excel)
write.csv(si_wide, "si_wide.csv", row.names = F) #Don't necessarily need this step but now you'll have both datasets saved


## Create empty matrix to place values in during loop
slopedf_si_wide <- matrix(ncol=2, nrow=length(1:ncol(si_wide)))
colnames(slopedf_si_wide) <- c("Site", "Slope")

## SMK Loop by site + put slopes in new df (tryCatch({}) is what allows loop to continue after error occuring)
for (i in 3:ncol(si_wide)) {
  tryCatch({
    tempdf <- data.frame(Month = si_wide$Month, Year = si_wide$Year, Site = si_wide[,i])
    colnames(tempdf)[3] <- "Site"
    tempdf[,3] <- as.numeric(tempdf[,3])
    test <- kendallSeasonalTrendTest(Site ~ Month + Year, data = tempdf)
    Slope <- test[["estimate"]][["slope"]]
    slopedf_si_wide[i,1] <- colnames(si_wide[i])
    slopedf_si_wide[i,2] <- Slope
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#Slope as data frame and slope numeric (OMITS ALL ROWS WITH NA DUE TO ERROR/FAILING KENDALLTEST)
site_slope<-as.data.frame(na.omit(slopedf_si_wide))
site_slope$Slope<-as.numeric(site_slope$Slope)

#Add Programs back
si_progsite<-unique(select(si_merged, Source, Site))

si_slopes<-full_join(si_progsite, site_slope, by="Site")
si_slopes<-na.omit(si_slopes)

write.csv(si_slopes, "si Slopes.csv", row.names = F)




### NH4 ###

rm(list=ls())

#Packages
library(tidyverse)
library(EnvStats)
library(wesanderson)
library(metR)
library(reshape2)

#Data Setup from Milly
#Set working directory to wear you have chla_merged.csv located
setwd("C:/Users/brittanytroast/Dropbox/Work/For Others")

## read file
nh4_merged <- read.csv("nh4_merged.csv")
#Added to fix source typing
colnames(nh4_merged)[1]<-"Source"  #Fixes weird colname after reading in

## select only columns we need
nh4_merged_x <- nh4_merged[,c("Site", "Year", "Month", "Parameter", "Value", "Units", "Source")]
## remove NAs
nh4_merged_x_na <- na.omit(nh4_merged_x)
## make sure Value column is numeric
nh4_merged_x_na$Value<-as.numeric(nh4_merged_x_na$Value)
## pivot wider
nh4_wide <- pivot_wider(nh4_merged_x_na, id_cols = c("Month", "Year"), values_from = "Value", names_from = "Site", values_fn = mean)
## write .csv (I do this because it's easier for me to look at data in excel)
write.csv(nh4_wide, "nh4_wide.csv", row.names = F) #Don't necessarily need this step but now you'll have both datasets saved


## Create empty matrix to place values in during loop
slopedf_nh4_wide <- matrix(ncol=2, nrow=length(1:ncol(nh4_wide)))
colnames(slopedf_nh4_wide) <- c("Site", "Slope")

## SMK Loop by site + put slopes in new df (tryCatch({}) is what allows loop to continue after error occuring)
for (i in 3:ncol(nh4_wide)) {
  tryCatch({
    tempdf <- data.frame(Month = nh4_wide$Month, Year = nh4_wide$Year, Site = nh4_wide[,i])
    colnames(tempdf)[3] <- "Site"
    tempdf[,3] <- as.numeric(tempdf[,3])
    test <- kendallSeasonalTrendTest(Site ~ Month + Year, data = tempdf)
    Slope <- test[["estimate"]][["slope"]]
    slopedf_nh4_wide[i,1] <- colnames(nh4_wide[i])
    slopedf_nh4_wide[i,2] <- Slope
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#Slope as data frame and slope numeric (OMITS ALL ROWS WITH NA DUE TO ERROR/FAILING KENDALLTEST)
site_slope<-as.data.frame(na.omit(slopedf_nh4_wide))
site_slope$Slope<-as.numeric(site_slope$Slope)

#Add Programs back
nh4_progsite<-unique(select(nh4_merged, Source, Site))

nh4_slopes<-full_join(nh4_progsite, site_slope, by="Site")
nh4_slopes<-na.omit(nh4_slopes)

write.csv(nh4_slopes, "nh4 Slopes.csv", row.names = F)




### turb ###
rm(list=ls())

#Packages
library(tidyverse)
library(EnvStats)
library(wesanderson)
library(metR)
library(reshape2)

#Data Setup from Milly
#Set working directory to wear you have chla_merged.csv located
setwd("C:/Users/brittanytroast/Dropbox/Work/For Others")

## read file
turb_merged <- read.csv("turb_merged.csv")
#Added to fix source typing
colnames(turb_merged)[1]<-"Source"  #Fixes weird colname after reading in

## select only columns we need
turb_merged_x <- turb_merged[,c("Site", "Year", "Month", "Parameter", "Value", "Units", "Source")]
## remove NAs
turb_merged_x_na <- na.omit(turb_merged_x)
## make sure Value column is numeric
turb_merged_x_na$Value<-as.numeric(turb_merged_x_na$Value)
## pivot wider
turb_wide <- pivot_wider(turb_merged_x_na, id_cols = c("Month", "Year"), values_from = "Value", names_from = "Site", values_fn = mean)
## write .csv (I do this because it's easier for me to look at data in excel)
write.csv(turb_wide, "turb_wide.csv", row.names = F) #Don't necessarily need this step but now you'll have both datasets saved


## Create empty matrix to place values in during loop
slopedf_turb_wide <- matrix(ncol=2, nrow=length(1:ncol(turb_wide)))
colnames(slopedf_turb_wide) <- c("Site", "Slope")

## SMK Loop by site + put slopes in new df (tryCatch({}) is what allows loop to continue after error occuring)
for (i in 3:ncol(turb_wide)) {
  tryCatch({
    tempdf <- data.frame(Month = turb_wide$Month, Year = turb_wide$Year, Site = turb_wide[,i])
    colnames(tempdf)[3] <- "Site"
    tempdf[,3] <- as.numeric(tempdf[,3])
    test <- kendallSeasonalTrendTest(Site ~ Month + Year, data = tempdf)
    Slope <- test[["estimate"]][["slope"]]
    slopedf_turb_wide[i,1] <- colnames(turb_wide[i])
    slopedf_turb_wide[i,2] <- Slope
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#Slope as data frame and slope numeric (OMITS ALL ROWS WITH NA DUE TO ERROR/FAILING KENDALLTEST)
site_slope<-as.data.frame(na.omit(slopedf_turb_wide))
site_slope$Slope<-as.numeric(site_slope$Slope)

#Add Programs back
turb_progsite<-unique(select(turb_merged, Source, Site))

turb_slopes<-full_join(turb_progsite, site_slope, by="Site")
turb_slopes<-na.omit(turb_slopes)

write.csv(turb_slopes, "turb Slopes.csv", row.names = F)



### TN ###

rm(list=ls())

#Packages
library(tidyverse)
library(EnvStats)
library(wesanderson)
library(metR)
library(reshape2)

#Data Setup from Milly
#Set working directory to wear you have chla_merged.csv located
setwd("//vmware-host/Shared Folders/Desktop/Work/csv data for R/DEP_Yr2")

## read file
TN_merged <- read.csv("TN_merged.csv")
#Added to fix source typing
colnames(TN_merged)[1]<-"Source"  #Fixes weird colname after reading in

## select only columns we need
TN_merged_x <- TN_merged[,c("Site", "Year", "Month", "Parameter", "Value", "Units", "Source")]
## remove NAs
TN_merged_x_na <- na.omit(TN_merged_x)
## make sure Value column is numeric
TN_merged_x_na$Value<-as.numeric(TN_merged_x_na$Value)
## pivot wider
TN_wide <- pivot_wider(TN_merged_x_na, id_cols = c("Month", "Year"), values_from = "Value", names_from = "Site", values_fn = mean)
## write .csv (I do this because it's easier for me to look at data in excel)
write.csv(TN_wide, "TN_wide.csv", row.names = F) #Don't necessarily need this step but now you'll have both datasets saved


## Create empty matrix to place values in during loop
slopedf_TN_wide <- matrix(ncol=2, nrow=length(1:ncol(TN_wide)))
colnames(slopedf_TN_wide) <- c("Site", "Slope")

## SMK Loop by site + put slopes in new df (tryCatch({}) is what allows loop to continue after error occuring)
for (i in 3:ncol(TN_wide)) {
  tryCatch({
    tempdf <- data.frame(Month = TN_wide$Month, Year = TN_wide$Year, Site = TN_wide[,i])
    colnames(tempdf)[3] <- "Site"
    tempdf[,3] <- as.numeric(tempdf[,3])
    test <- kendallSeasonalTrendTest(Site ~ Month + Year, data = tempdf)
    Slope <- test[["estimate"]][["slope"]]
    slopedf_TN_wide[i,1] <- colnames(TN_wide[i])
    slopedf_TN_wide[i,2] <- Slope
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#Slope as data frame and slope numeric (OMITS ALL ROWS WITH NA DUE TO ERROR/FAILING KENDALLTEST)
site_slope<-as.data.frame(na.omit(slopedf_TN_wide))
site_slope$Slope<-as.numeric(site_slope$Slope)

#Add Programs back
TN_progsite<-unique(select(TN_merged, Source, Site))

TN_slopes<-full_join(TN_progsite, site_slope, by="Site")
TN_slopes<-na.omit(TN_slopes)

write.csv(TN_slopes, "TN Slopes.csv", row.names = F)




### TP ###

rm(list=ls())

#Packages
library(tidyverse)
library(EnvStats)
library(wesanderson)
library(metR)
library(reshape2)

#Data Setup from Milly
#Set working directory to wear you have chla_merged.csv located
setwd("//vmware-host/Shared Folders/Desktop/Work/csv data for R/DEP_Yr2")

## read file
TP_merged <- read.csv("TP_merged.csv")
#Added to fix source typing
colnames(TP_merged)[1]<-"Source"  #Fixes weird colname after reading in

## select only columns we need
TP_merged_x <- TP_merged[,c("Site", "Year", "Month", "Parameter", "Value", "Units", "Source")]
## remove NAs
TP_merged_x_na <- na.omit(TP_merged_x)
## make sure Value column is numeric
TP_merged_x_na$Value<-as.numeric(TP_merged_x_na$Value)
## pivot wider
TP_wide <- pivot_wider(TP_merged_x_na, id_cols = c("Month", "Year"), values_from = "Value", names_from = "Site", values_fn = mean)
## write .csv (I do this because it's easier for me to look at data in excel)
write.csv(TP_wide, "TP_wide.csv", row.names = F) #Don't necessarily need this step but now you'll have both datasets saved


## Create empty matrix to place values in during loop
slopedf_TP_wide <- matrix(ncol=2, nrow=length(1:ncol(TP_wide)))
colnames(slopedf_TP_wide) <- c("Site", "Slope")

## SMK Loop by site + put slopes in new df (tryCatch({}) is what allows loop to continue after error occuring)
for (i in 3:ncol(TP_wide)) {
  tryCatch({
    tempdf <- data.frame(Month = TP_wide$Month, Year = TP_wide$Year, Site = TP_wide[,i])
    colnames(tempdf)[3] <- "Site"
    tempdf[,3] <- as.numeric(tempdf[,3])
    test <- kendallSeasonalTrendTest(Site ~ Month + Year, data = tempdf)
    Slope <- test[["estimate"]][["slope"]]
    slopedf_TP_wide[i,1] <- colnames(TP_wide[i])
    slopedf_TP_wide[i,2] <- Slope
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#Slope as data frame and slope numeric (OMITS ALL ROWS WITH NA DUE TO ERROR/FAILING KENDALLTEST)
site_slope<-as.data.frame(na.omit(slopedf_TP_wide))
site_slope$Slope<-as.numeric(site_slope$Slope)

#Add Programs back
TP_progsite<-unique(select(TP_merged, Source, Site))

TP_slopes<-full_join(TP_progsite, site_slope, by="Site")
TP_slopes<-na.omit(TP_slopes)

write.csv(TP_slopes, "TP Slopes.csv", row.names = F)

