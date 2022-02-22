### SEV Nfert Data Cleaning ###

## Data manipulation packages
library(tidyverse)

source("utilities/data_import.R")

## Data import
sev_nfert <- read_csv_gdrive("1nMrJK_K3AJwqVbCDMEjRf1bo_W2MqiQ_")%>%
  tbl_df()

## Working data frame
sevnfert <- sev_nfert

unique(sevnfert$site)
# only 1 site

unique(sevnfert$treatment)
# 2 unique trts

unique(sevnfert$year)
#Twice a year sampling
unique(sevnfert$season)
sevnfert$seas_num<-as.numeric(as.character(recode_factor(sevnfert$season,fall="0.5",spring="0")))
sevnfert$year_seas<-sevnfert$year+sevnfert$seas_num

### Add ExpYear
df <- data.frame(year_seas= seq(from=min(sevnfert$year_seas),to=max(sevnfert$year_seas), by=0.5),
                 ExpYear= seq(from=1,to=(max(sevnfert$year_seas)-min(sevnfert$year_seas)+1), by=0.5))
sevnfert <- merge.data.frame(sevnfert, df, by="year_seas" )

## Clean up data

sev_nfert_clean <- sevnfert %>%
  tbl_df() %>%
  dplyr::select(-date) %>%  # Remove unwanted columns
  tbl_df() %>%
  ## format column names to match the rest of the datasets
  dplyr::mutate(site = "sev",
                project = "nfert",
                field = site,
                year = year_seas, #Sto deal with fall/spring
                expyear = ExpYear,
                plot = plot, 
                subplot = quad,   # check if this does make sense
                abundance = cover/100, #cover was entered as % of 1m^2 
                #trt = treatment,
                carbon = "0",
                nadd = treatment,
                ncess = "0",
                fence = "0",
                burn = "0",
                rainfall = "0", 
                warm = "0",
                unitAbund = "cover",
                scaleAbund = "m2",   
                species = paste(genus, sp.epithet, sep = " "),
                uniqueID = paste(site, field, project, plot, subplot, sep = "_")) %>%
  dplyr::select(year, site, field, project, plot, subplot, uniqueID, 
                carbon, nadd, ncess, fence, burn, rainfall, warm,
                species, abundance, unitAbund, scaleAbund) 



rm(df, sev_nfert, sevnfert)

