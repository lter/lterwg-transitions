### SEV EDGE Data Cleaning ###

## Data manipulation packages
library(tidyverse)
#source("utilities/data_import.R")

## Data import
sev_edge <- read.csv("shape-shifting-subordinates/sev298_NPP_edge_biomass.csv")

## Working data frame
sevedge <- sev_edge %>%
  filter(season == "spring")

unique(sevedge$site)
# only 2 sites

unique(sevedge$treatment)
# 3 unique trts


### Add ExpYear
df <- data.frame(year= 2012:2021,
                 ExpYear= 1:10)
sevedge <- merge.data.frame(sevedge, df, by="year" )

## Clean up data SEV edgeNDEX!

sev_edge_clean <- sevedge %>%
  tbl_df() %>%
  #dplyr::select(-date, -height, -count) %>%  # Remove unwanted columns
  tbl_df() %>%
  ## format column names to match the rest of the datasets
  dplyr::mutate(site = "SEV",
                project = "edge",
                field = sevedge$site,
                year = year,
                expyear = ExpYear,
                plot = plot, 
                subplot = quad,   # check if this does make sense
                block = block,
                abundance = biomass.BM, 
                #trt = treatment,
                carbon = "0",
                nadd = "0",
                ncess = "0",
                fence = "0",
                burn = "0",
                rainfall = treatment, 
                warm = "0",
                unitAbund = "biomass",
                scaleAbund = "g m2",   # check metadata
                species = kartez,
                uniqueID = paste(site, field, project, block, plot, subplot, sep = "_")) %>%
  dplyr::select(year, site, field, project, plot, subplot, uniqueID,
                carbon, nadd, ncess, fence, burn, rainfall, warm,
                species, abundance, unitAbund, scaleAbund) 

rm(df, sev_edge, sevedge)





