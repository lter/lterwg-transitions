library(tidyverse)
library(lubridate)
source("utilities/data_import.R")

## Read in the jrn_prism data
jrn_prism <- read_csv_gdrive("0B7o8j0RLpcxiWEdiT09uYVEwdTg") %>%
  tbl_df() 

# Read spp abundance data to select the same years for climate data as we have plant data
source("Data_cleaning/jrn_cleaning.R")

# Reformat jrn_crossite years to remove decimal from seasonal sampling 
# Create a vector of years for which we have plant data
years<- unique(as.numeric(substr(jrn_crosssite$year,start = 1,stop = 4)))

# Calculate mean daily temperature to match hay data (we only have mean temperature) 
# We keep tmin and tmax for potential future use
jrn_prism$tmean<-rowMeans(jrn_prism[,4:5])
# Create year and month columns to calculate growing season values for temp and ppt
jrn_prism$year<-as.numeric(format(as.Date(jrn_prism$Date),"%Y"))
jrn_prism$month<-as.numeric(format(as.Date(jrn_prism$Date),"%m"))
# Create a vector of growing season month for the site
growing_season<-c(7:10)

# Aggregate data annually for mean yearly values and growing season 
jrn_prism<- data.frame(aggregate(ppt_daily~year,data=jrn_prism,sum),
                       temp=aggregate(tmean~year,data=jrn_prism,mean)[2],
                       tmax=aggregate(tmax_daily~year,data=jrn_prism,mean)[2],
                       tmin=aggregate(tmin_daily~year,data=jrn_prism,mean)[2],
                       growing_season_ppt=aggregate(ppt_daily~year,data=jrn_prism[which(jrn_prism$month%in%growing_season),],sum)[2],
                       growing_season_tmean=aggregate(tmean~year,data=jrn_prism[which(jrn_prism$month%in%growing_season),],mean)[2],
                       growing_season_tmax=aggregate(tmax_daily~year,data=jrn_prism[which(jrn_prism$month%in%growing_season),],mean)[2],
                       growing_season_tmin=aggregate(tmin_daily~year,data=jrn_prism[which(jrn_prism$month%in%growing_season),],mean)[2])
# Rename colunms
colnames(jrn_prism)<-c("year","ppt","temp","tmax","tmin","growing_season_ppt","growing_season_tmean","growing_season_tmax","growing_season_tmin")
# Select climate data for years for which we have spp abundance data
#jrn_prism<-jrn_prism[jrn_prism$year %in% years,]

jrn_prism_test <- subset(jrn_prism, jrn_prism$year > 1988)
jrn_prism <- subset(jrn_prism_test, jrn_prism_test$year < 2013)

rm(jrn_prism_test)
# Save data as .csv
# write.csv(jrn_prism,"C:/Users/lgherar1/jrn_prsm.csv")
