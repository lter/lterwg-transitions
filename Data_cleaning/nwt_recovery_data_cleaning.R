### NWT Recovery Data Cleaning ###

## Data manipulation packages
library(tidyverse)
source("utilities/data_import.R")

## Data import
nwt_recovery <- read_csv_gdrive("1xQjWs4H8W3GrS2wDyST5_Wml4qzqnXdv")%>%
  tbl_df()

## Working data frame
nwtrec <- nwt_recovery

### Add ExpYear
df <- data.frame(YEAR= 2011:2017,
                 ExpYear= 1:7)
nwtrec <- merge.data.frame(nwtrec, df, by="YEAR" )

### This data set is cessation from N addition
### All of it?

## Trt column indicates amount of N added
## Plottype comun is NCess

### Cessation of N addition
# NCess = 1  means N was previously added, but stopped
# NCess = 0  means N is being added if Nadd > 0 
# or NCess can be 0 if N was never added

## In this data set, NCess is T or R 
## R = recovery = ncess

rm(df)

## Clean up data NWT Recovery

nwt_rec_clean <- nwtrec %>%
  tbl_df() %>%
  dplyr::select(-LTER_SITE) %>%  # Remove unwanted columns
  tbl_df() %>%
  ## format column names to match the rest of the datasets
  dplyr::mutate(site = "nwt",
                project = "recovery",
                field = "1",
                year = YEAR,
                expyear = ExpYear,
                plot = BLOCK, 
                subplot = "1",
                abundance = HITS, 
                carbon = "0",
                nadd = TRT,
                ncess = PLOTTYPE,
                fence = "0",
                burn = "0",
                rainfall = "0", 
                warm = "0",
                unitAbund = "hits",
                scaleAbund = " ",
                species = TAXONID,
                uniqueID = paste(site, field, project, plot, subplot, sep = "_")) %>%
  dplyr::select(year, site, field, project, plot, subplot, uniqueID, 
                carbon, nadd, ncess, fence, burn, rainfall, warm,
                species, abundance, unitAbund, scaleAbund) 


rm(nwt_recovery, nwtrec)
