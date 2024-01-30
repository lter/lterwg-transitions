###             NWT ITEX Data Cleaning     ###
### Increased Rainfall, N and snowpack exp ###

## Data manipulation packages
library(tidyverse)
source("utilities/data_import.R")

## Data import
nwt_itex <- read_csv_gdrive("1Qw-CBv5wSqO6kEv9q5xoN3qlDrqLxpLr")%>%
  tbl_df()

## Working data frame
nwtitex <- nwt_itex

### Add ExpYear
df <- data.frame(year= 2006:2020,
                 ExpYear= 1:15)
nwtitex <- merge.data.frame(nwtitex, df, by="year" )

rm(df)

### Trt names
# X = control
# W = warming
# N  = N added
# P  = P added

## Trt column indicates amount of N added
## Plottype comun is NCess

### Cessation of N addition
# NCess = 1  means N was previously added, but stopped
# NCess = 0  means N is being added if Nadd > 0 
# or NCess can be 0 if N was never added

## In this data set, NCess is T or R 
## R = recovery = ncess


## Clean up data NWT Recovery

nwt_itex_clean <- nwtitex %>%
  tbl_df() %>%
  filter(growth_habit != "Litter") %>%
  filter(growth_habit != "Bare Ground") %>%
  dplyr::select(-LTER_site, -date, -spp, -USDA_code, -growth_habit) %>%  # Remove unwanted columns
  tbl_df() %>%
  ## format column names to match the rest of the datasets
  dplyr::mutate(site = "nwt",
                project = "itex",
                field = block,
                year = year,
                expyear = ExpYear,
                plot = plot, 
                subplot = "1",
                abundance = hits, 
                carbon = "0",
                nadd = N,
                ncess = "0",
                fence = "0",
                burn = "0",
                rainfall = snow, 
                warm = temp,
                unitAbund = "hits",
                scaleAbund = " ",
                species = USDA_name,
                uniqueID = paste(site, field, project, plot, subplot, sep = "_")) %>%
  dplyr::select(year, site, field, project, plot, subplot, uniqueID, 
                carbon, nadd, ncess, fence, burn, rainfall, warm,
                species, abundance, unitAbund, scaleAbund) 


rm(nwt_itex, nwtitex)
