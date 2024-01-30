### SEV EDGE Data Cleaning ###

## Data manipulation packages
library(tidyverse)
#source("utilities/data_import.R")

# Read in Data ####
sev_edge <- read.csv("shape-shifting-subordinates/sev298_NPP_edge_biomass.csv")

## Working data frame
sevedge <- sev_edge %>%
  filter(season == "spring")
## NOTE: ####
  ## we've filtered to only the spring season. Is there a way to incorporate the fall season also?


# Explore Data ####

## info about the data
## years 2013 - 2021
## two fields - EDGE_black, EDGE_blue
## three rainfall treatments: E, C, D
## C = control; E = event reduction by 66%; D = delayed monsoon season
## looks like 96 total species; currently includes EMPTY; UNKFORB1
## abundance data is biomass in g/m2 -> need to confirm this

## drought was imposed for 4 years; other years in this dataset are recovery



unique(sevedge$site)
# only 2 sites

unique(sevedge$treatment)
# 3 unique trts


### Add ExpYear
df <- data.frame(year= 2013:2022,
                 ExpYear= 1:10) ## seems like 2012 is not in this dataset?
sevedge <- merge.data.frame(sevedge, df, by="year" )





# Make Mods ####

## Clean up data SEV edgeNDEX!
sev_edge_clean <- sevedge %>%
  ## format column names to match the rest of the datasets
  mutate(site = "SEV",
         project = "edge",
         field = sevedge$site,
         year = year,
         expyear = ExpYear,
         plot = plot,
         subplot = quad,   # check if this does make sense
         block = block,
         abundance = biomass.BM,
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
  filter(rainfall != "D") %>% ## remove monsoon timing treatment
  mutate(treatment_year = ifelse(rainfall == "E" & expyear %in% 1:4, "drought",
                                 ifelse(rainfall == "E" &  expyear > 4, "recovery", "control"))) %>% 
  select(year, expyear, site, field, project, plot, subplot, uniqueID,
                carbon, nadd, ncess, fence, burn, rainfall, treatment_year, warm,
                species, abundance, unitAbund, scaleAbund, cover, LifeHistory, PhotoPath, FunctionalGroup, SPEI.comp, season.precip) 

## have changed the selected columns to include life history, precip info, etc.

rm(df, sev_edge, sevedge)





