### SEV WENNDEX Data Cleaning ###

## Data manipulation packages
library(tidyverse)
source("utilities/data_import.R")

## Data import
sev_wen <- read_csv_gdrive("1s-d1gJ4gZ6BeobZUPFpH5wzDl-ewsQeD")%>%
  tbl_df()

## Working data frame
sevwen <- sev_wen

unique(sevwen$site)
# only 1 site

unique(sevwen$treatment)
# 8 unique trts


### Add ExpYear
df <- data.frame(year= 2006:2017,
                 ExpYear= 1:12)
sevwen <- merge.data.frame(sevwen, df, by="year" )

## Clean up data SEV WENNDEX!

sev_wen_clean <- sevwen %>%
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
                nadd = "treatment",
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

rm(df, sev_wen, sevwen)
