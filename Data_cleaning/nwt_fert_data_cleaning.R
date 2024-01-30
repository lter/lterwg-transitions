### NWT Fertilization Data Cleaning ###

## Data manipulation packages
library(tidyverse)
source("utilities/data_import.R")

## Data import
nwt_fert <- read_csv_gdrive("1klWmshV2Iad-NVpiqzLIpFFyK38nom5B")%>%
  tbl_df()

## Working data frame
nwtfert <- nwt_fert

### Add ExpYear
df <- data.frame(year= 2011:2019,
                 ExpYear= 1:9)
nwtfert <- merge.data.frame(nwtfert, df, by="year" )

rm(df)

### Trt names
# fert = yes/no to fertilization
# plots paired by cover of Aco and Des
# 288 kg/ ha yr initially, then

## Trt column indicates amount of N added
## Plottype comun is NCess

### Cessation of N addition
# NCess = 1  means N was previously added, but stopped
# NCess = 0  means N is being added if Nadd > 0 
# or NCess can be 0 if N was never added

## In this data set, NCess is T or R 
## R = recovery = ncess

## Clean up data NWT Recovery

nwt_fert_clean <- nwtfert %>%
  tbl_df() %>%
  filter(growth_habit != "Litter") %>%
  filter(growth_habit != "Bare Ground") %>%
  dplyr::select(-LTER_site, -date, -spp, -USDA_code, -growth_habit) %>%  # Remove unwanted columns
  tbl_df() %>%
  ## format column names to match the rest of the datasets
  dplyr::mutate(site = "nwt",
                project = "fert",
                field = "Nsaddle",
                year = year,
                expyear = ExpYear,
                plot = plot, 
                subplot = "1",
                abundance = hits, 
                carbon = "0",
                nadd = fert,
                ncess = "0",
                fence = "0",
                burn = "0",
                rainfall = "0", 
                warm = "0",
                unitAbund = "hits",
                scaleAbund = " ",
                species = USDA_name,
                uniqueID = paste(site, field, project, plot, subplot, sep = "_")) %>%
  dplyr::select(year, site, field, project, plot, subplot, uniqueID, 
                carbon, nadd, ncess, fence, burn, rainfall, warm,
                species, abundance, unitAbund, scaleAbund) 


rm(nwt_fert, nwtfert)
