### JRN Grassland & Shrubland Plant Data ###

## Data manipulation packages
library(tidyverse)

## only read this in if using USDA for spp cleaning
# Source USDA species cleaning code
# source("Data_cleaning/sppcodes_cleaning.R")
#source("utilities/data_import.R")

## Read in the data
## Using data requested by Shaopeng in 2014; appears to be most up-to-date
## Removed metadata from the top of file for easier import
#jrn <- read_csv_gdrive("0BwguSiR80XFZWlBFdnFyWV9Jbzg") %>%
 # tbl_df()

## download data from google drive
#drive_download(file = "https://drive.google.com/file/d/1oF0qiBUJEDzTRi5g7plpHehJFudP7IuI/view?usp=share_link")

#jrn <- read.csv("JornadaStudy_011_npp_quad_data.csv")
jrn2 <- read.csv("shape-shifting-subordinates/grassland_ambient_data/nppqdbio.csv", skip = 42)
    ##42 was the magic number that got the colnames to align correctly.

## Read in the coordiantes for spatially explicit analyses
## LMH derived these from the raw dataset
## NOTE: 
# site RABB is missing 1 
# site COLL is in a long line 
# site SMAL is missing a point 
# site WEST is missing two rows 
# and another might be missing a point? 

## download the JornadaStudy_011_npp_quad_data...
#drive_download(file = "https://drive.google.com/file/d/1y-lBRkgO6i_xy1c12C_KC6qDBUxvw0O4/view?usp=share_link")

## download the jornada nppqdbio.csv file
#drive_download(file = "https://drive.google.com/file/d/14oiqlU7x2dyY97c1udVlwpPhGI423imN/view?usp=share_link")


jrn_coordinates <- read.csv("shape-shifting-subordinates/grassland_ambient_data/JRN_plotcoordinates.csv") %>%
  tbl_df() %>%
  mutate(habitat = zone,
         project = site,
         plot = site,
         site = "jrn",
         subplot = quad,
         longitude = POINT_X,
         latitude = POINT_Y) %>%
  dplyr::select(site, habitat, project, plot, subplot, latitude, longitude)

# check that each season was measured in each plot
# winter is late feb
# spring is may
# fall is october
# jrn %>%
#  dplyr::select(season, zone) %>%
#   unique()

# Clean the dataset
jrn_clean <- jrn2 %>%
  # make year a decimal by season 
  # mutate(year = ifelse(season == "W", year + .2, year),
  #        year = ifelse(season == "S", year + .4, year),
  #        year = ifelse(season == "F", year + .8, year)) %>%
  filter(spp != ".") %>%
   # make biomass numeric
  mutate(biomass2 = as.numeric(as.character(biomass)))  %>%
  # rename what they called site to what we call project
  mutate(project = site,
         plot = site) %>%
  dplyr::select(-site)%>%
  # format column names
  mutate(site = "jrn",
         habitat = zone,
         project = plot,
         species = spp,
         subplot = quad,
         abundance = biomass2,
         unitAbund = "biomass_g", 
         scaleAbund = "1m2",
         # func = paste(form, path, sep = "_"),
         growth = form,
         uniqueID = paste(site, project, plot, subplot, sep = "_")) %>%
  
  group_by(year, site, habitat, project, plot, subplot, uniqueID, species, unitAbund, scaleAbund) %>%
  summarize(abundance=max(abundance)) %>%
  
  dplyr::select(year, site, habitat, project, plot, subplot, uniqueID, species, abundance, unitAbund, scaleAbund) %>%
  tbl_df()

## JRN spp - will need a lot of work, for now let's go with what is on the record (which does not give duration or status)
# jrn_spp1 <- as.data.frame(unique(jrn_clean$species)) %>%
#   tbl_df()
# names(jrn_spp1) = "species"
# 
# usda_spp_jrn <- usda_spp %>%
#   mutate(species = symbol)
# 
# jrn_spp <- left_join(jrn_spp1, usda_spp_jrn)

## SPECIES LEVEL ##
## Remove the unknowns
jrn_unknowns <- c("UKCA1", "UKFO?", "UKFO1", "UKFO2", "UKFO3", 
                    "UKFO4", "UKFO5", "UKFO6", "UKFO7", "UKFO8",
                    "UKFOa", "UKGR1", "UKGR2", "UKGR3", "UKGR4", "UKGS1",
                    "UKGS2", "UKSH1" )

## The full JRN record for habitat scaling
# might need some more species checking (like "SPVEG"?)
jrn_hierarchies <- jrn_clean[-which(jrn_clean$species%in%jrn_unknowns),] 

## Select the 3 grassland sites for the hierarchy group
jrn_hierarchies <- jrn_hierarchies[which(jrn_hierarchies$project%in%c("BASN","IBPE","SUMM")),]
  
## The BASN project for cross-site
 jrn_crosssite <- jrn_hierarchies %>%
  filter(project == "BASN") 

## FUNCTIONAL GROUPS ##
## Subset a version that is just by functional group
## might want to broaden the categories (e.g., Grass instead of Grass_C3 or Grass_C4)
## will need to clean up the names of categories to match other sites
##  ignore the seasonal sampling by taking the maximum abundance per plot per year among the three sampling dates
## Here, we are going to use the maximmum abundance for each functional group each year
jrn_unkn_rm<-jrn[-which(jrn$spp%in%jrn_unknowns),]
jrn_unkn_rm<-jrn_unkn_rm[-which(jrn_unkn_rm$form=="."),]
jrn_func <- jrn_unkn_rm %>%
    # make biomass numeric
  mutate(biomass2 = as.numeric(as.character(biomass))) %>%
  # remove the times with no species (this will get filled in when we add zeros)
  # all reps have a species at least one time in the time series, so not losing reps by doing this
  filter(spp != ".") %>%
  # rename what they called site to what we call project
  mutate(project = site,
         plot = site) %>%
  dplyr::select(-site) %>%
  # format column names
  mutate(site = "jrn",
         habitat = zone,
         species = spp, 
         subplot = quad,
         abundance = biomass2,
         unitAbund = "biomass_g", scaleAbund = "1m2",
         # func = paste(form, path, sep = "_"),
         growth = form,
         uniqueID = paste(site, project, plot, subplot, sep = "_")) %>%
  # we are only using the BASN site within jornada
  filter(project == "BASN") %>%
  group_by(year, site, habitat, project, plot, subplot, uniqueID, species, unitAbund, scaleAbund, growth)%>%
  summarize(abundance=max(abundance))%>%
  group_by(year, site, habitat, project, plot, subplot, uniqueID, unitAbund, scaleAbund, growth)%>%
  summarize(abundance=sum(abundance))
  # arrange(subplot,year,growth)

# Remove unneeded files
rm(jrn2, jrn_clean, jrn_unknowns, jrn_unkn_rm)
  