### SEV MRME Data Cleaning ###

## Data manipulation packages
library(tidyverse)
source("utilities/data_import.R")

## Data import
sev_mrme <- read_csv_gdrive("1MtFgTlkTk7KZkW28C1JtuM3fU7qKn1Ul")%>%
  tbl_df()

## Working data frame
sevmrme <- sev_mrme

unique(sevmrme$site)
# only 1 site

unique(sevmrme$treatment)
# 6 unique trts


### Add ExpYear
df <- data.frame(year= 2006:2017,
                 ExpYear= 1:12)
sevmrme <- merge.data.frame(sevmrme, df, by="year" )

## Clean up data SEV WENNDEX!

sev_mrme_clean <- sevmrme %>%
  tbl_df() %>%
  dplyr::select(-comment, -date, -height, -count) %>%  # Remove unwanted columns
  tbl_df() %>%
  ## format column names to match the rest of the datasets
  dplyr::mutate(site = "sev",
                project = "wen",
                field = site,
                year = year,
                expyear = ExpYear,
                plot = plot, 
                subplot = quad,   # check if this does make sense
                abundance = cover, 
                #trt = treatment,
                carbon = "0",
                nadd = treatment,
                ncess = "0",
                fence = "0",
                burn = "0",
                rainfall = "0", 
                warm = "0",
                unitAbund = "cover",
                scaleAbund = "m2",   # check metadata
                species = species,
                uniqueID = paste(site, field, project, plot, subplot, sep = "_")) %>%
  dplyr::select(year, site, field, project, plot, subplot, uniqueID, 
                carbon, nadd, ncess, fence, burn, rainfall, warm,
                species, abundance, unitAbund, scaleAbund) 



rm(df, sev_mrme, sevmrme)
