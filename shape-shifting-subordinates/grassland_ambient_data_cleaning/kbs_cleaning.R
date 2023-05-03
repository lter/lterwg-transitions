### KBS Grassland Plant Data ###

## Data manipulation packages
library(tidyverse)
library(lubridate)
source("utilities/data_import.R")
source("Data_cleaning/sppcodes_cleaning.R")

# Package ID: knb-lter-kbs.55.16 Cataloging System:https://pasta.lternet.edu.
# Data set title: Plant Community and Ecosystem Responses to Long-term Fertilization & Disturbance at the Kellogg Biological Station, Hickory Corners, MI  (1989 to 2014).
# Data set creator:  Katherine Gross - Michigan State University 
# Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
# Contact:    - Data Manager Kellogg Biological Station  - lter.data.manager@kbs.msu.edu
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-kbs.55.16
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

#infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-kbs/55/16/3eb91382bd90ec317c67740dd800c9d6" 
#infile1 <- sub("^https","http",infile1) 
# kbs <-read.csv(infile1,header=F 
#          ,skip=26
#            ,sep=","  
#                ,quot='"' 
#        , col.names=c(
#                    "sample_date",     
#                    "species",     
#                    "treatment",     
#                    "replicate",     
#                    "disturbed",     
#                    "fertilized",     
#                    "biomass_g",     
#                    "area_sampled"    ), check.names=TRUE)
               


## Read in the data cached on Google Drive 
## NOTE: KBS puts meta data at the top of their files
## LMH deleted this to make it easier to import (154-early+successional+microplot+biomass+by+species.csv)
# kbs <- read_csv_gdrive("0BwguSiR80XFZdnh2M216RS1sazA") %>%
#   tbl_df()

library(googledrive)
Kbs_Folder<-"https://drive.google.com/drive/folders/1jx6T3nVap8oa9FUlQkFEE7WP-B5uHMy0"
Kbs_File<-googledrive::drive_ls(path = as_id(Kbs_Folder), type = "csv", pattern = "154-early")
Kbs_Id<-Kbs_File$id
googledrive::drive_download(file = as_id(Kbs_Id), 
                            overwrite = T)

kbs<-read.csv("154-early+successional+microplot+biomass+by+species.csv")
unique(kbs$treatment)
unique(kbs$sample_date)

## Clean the data
kbs_clean <- kbs %>%
  ## Extract month and year from the date
  mutate(date = mdy(sample_date),
         year = year(date),
         month = month(date),
  ## Standardize column names       
         habitat = paste(disturbed, fertilized, sep = "_"), 
         site = "kbs",
         project = treatment, 
         plot = replicate,
  ## NOTE: In the previous LTER grasslands synthesis there were subplots, not sure why not from the raw data
         subplot = NA,
         abundance = biomass_g_m2,
         unitAbund = "biomass_gm2",
         scaleAbund = area_sampled_m2,
         uniqueID = paste(site, habitat, plot, sep = "_")) %>%
  ## KBS has a lot of different unknowns, some more refined than others
  ## removing any that are unknown at the family level or higher
  ## We might want to do this later so that we can include it for the kbs_func dataframe
  filter(species != "UnSorted",
         species != "Surface Litter",
      #   species != "Unknown dicot (*)",
       #  species != "unknown Asteraceae",
      #   species != "Unknown grass",
      #   species != "Unkown Fabaceae",
      #   species != "Unknown Rosacae",
      #   species != "Unknown Solanaceae",
         species != "Standing Dead",
       #  species != "unknown Sedge",
      #   species != "unknown Brassicaceae",
      #   species != "Woody",
      #   species != "Unknown monocot (*)",
      #   species != "Aster sp. (*)",
         species != "Unknown") %>%
  ## Until 1997 KBS did multiple harvests per year to capture species at their peak
  ## select the sampling period closest to August (which was the sampling time thereafter) 
  mutate(tokeep = 1,
         tokeep = ifelse(year == 1989 & month == 7, 0, tokeep),
         tokeep = ifelse(year == 1991 & month == 7, 0, tokeep),
         tokeep = ifelse(year == 1996 & month < 8, 0, tokeep)) %>%
  filter(tokeep == 1) %>%
  ## In 2007 there were several repeat entries
  ## average them to create a single entry
  group_by(year, site, habitat, project, plot, subplot, uniqueID, species, unitAbund, scaleAbund) %>%
  summarize(abundance = mean(abundance)) %>%
  tbl_df() %>%
  ## select columns to retain
  dplyr::select(year, site, habitat, project, plot, subplot, uniqueID, species, abundance, unitAbund, scaleAbund) 

## Species cleaning ##

# unique species names from kbs
kbs_spp1 <- kbs_clean %>%
  # just do the species we retain for now...
  filter(habitat == "undisturbed_unfertilized" & project == "T7") %>%
  dplyr::select(species) %>%
  unique() %>%
  mutate(species2 = species) %>%
  separate(species2, c("genusName", "speciesName"))

# modify the usda_spp to link to kbs 
# No Usda_spp file available ot link to 
usda_spp_kbs <- usda_spp %>%
  mutate(species = Scientific.Name) %>%
  dplyr::select(-altSymbol, -acceptedSymbol, -symbol) %>%
  mutate(duration = as.character(duration),
         durationDetail = as.character(durationDetail),
         growth = as.character(growth),
         growthDetail = as.character(growth),
         family = as.character(family))


# about half of the kbs species are on file from usda
# merge those in
kbs_spp2 <- left_join(kbs_spp1, usda_spp_kbs)

# retain the ones that are okay
kbs_spp2.5 <- kbs_spp2 %>%
  filter(!is.na(Scientific.Name)) %>%
  dplyr::select(-Scientific.Name)

# need to look through the ones that didn't line up
kbs_spp3 <- kbs_spp2 %>%
  filter(is.na(Scientific.Name)) %>%
  mutate(speciesFromSite = species) %>%
  dplyr::select(speciesFromSite, species) %>%
  separate(species, c("genusName", "speciesName")) %>%
  dplyr::select(speciesFromSite, genusName, speciesName) %>%
  # update genus or species name to align
  mutate(speciesName = as.character(speciesName),
         genusName = as.character(genusName),
         speciesName = ifelse(speciesFromSite == "Bromus mollis L.", "hordeaceus", speciesName),
         speciesName = ifelse(speciesFromSite == "Silene alba (Mill.) E.H.L.Krause", "latifolia", speciesName),
         speciesName = ifelse(speciesFromSite == "Capsella bursa-pastoris (L.) Medicus", "bursa-pastoris", speciesName),
         speciesName = ifelse(speciesFromSite == "Aster pilosus Willd.", "pilosum", speciesName),
         genusName = ifelse(speciesFromSite == "Aster pilosus Willd.", "Symphyotrichum", genusName),
         speciesName = ifelse(speciesFromSite == "Cerastium vulgatum L.", "fontanum", speciesName),
         speciesName = ifelse(speciesFromSite == "Aster cordifolius", "cordifolium", speciesName),
         genusName = ifelse(speciesFromSite == "Aster cordifolius", "Symphyotrichum", genusName),
         genusName = ifelse(speciesFromSite == "Aster ericoides L.", "Symphyotrichum", genusName),
         genusName = ifelse(speciesFromSite == "Aster novae-angliae L.", "Symphyotrichum", genusName),
         speciesName = ifelse(speciesFromSite == "Aster novae-angliae L.", "novae-angliae", speciesName),
         genusName = ifelse(speciesFromSite == "Aster pilosus Willd.", "Symphyotrichum", genusName),
         genusName = ifelse(speciesFromSite == "Aster sagittifolius", "Symphyotrichum", genusName),
         speciesName = ifelse(speciesFromSite == "Aster sagittifolius", "urophyllum", speciesName),
         speciesName = ifelse(speciesFromSite == "Bromus japonicus Thunb. ex Murr.", "arvensis", speciesName),
         speciesName = ifelse(speciesFromSite == "Echinochloa crus-galli (L.) Beauv.", "crus-galli", speciesName),
         speciesName = ifelse(speciesFromSite == "Epilobium glandulosum Lehm. (*)", "ciliatum", speciesName)) %>%
  mutate(speciesName = ifelse(speciesFromSite == "Persicaria maculosa  Gray", "persicaria", speciesName),
         genusName = ifelse(speciesFromSite == "Persicaria maculosa  Gray", "Polygonum", genusName))

# join the updated species names with the usda
kbs_spp4 <- left_join(kbs_spp3, usda_spp_kbs) %>% 
  dplyr::select(-Scientific.Name, -species) %>%
  # clean up duplicates
  mutate(status = ifelse(speciesFromSite == "Taraxacum officinale Weber in", "NI", status),
         duration = ifelse(speciesFromSite == "Eragrostis pectinacea (Michx.) Nees", "Annual", duration),
         durationDetail = ifelse(speciesFromSite == "Eragrostis pectinacea (Michx.) Nees", "Annual, Perennial", durationDetail)) %>%
  mutate(todelete = ifelse(speciesFromSite == "Eragrostis cilianensis (All.) E.Mosher" & is.na(status), 1, 0),
         todelete = ifelse(speciesFromSite == "Prunus serotina Ehrh. (*)" & is.na(status), 1, todelete),
         todelete = ifelse(speciesFromSite == "Bromus mollis L." & is.na(status), 1, todelete),
         todelete = ifelse(speciesFromSite == "Cerastium vulgatum L." & duration =="Biennial", 1, todelete),
         speciesName = ifelse(speciesName == "spp", "sp", speciesName)) %>%
  filter(todelete == 0) %>%
  unique() %>%
  group_by(speciesFromSite) %>%
  mutate(count = n()) %>%
  dplyr::select(-count, -todelete)

# retain the ones that are okay
kbs_spp4.5 <- kbs_spp4 %>%
  tbl_df() %>%
  filter(!is.na(family)) %>%
  mutate(species = speciesFromSite) %>%
  dplyr::select(-speciesFromSite)

# for the ones that are just by genus, create a linking usda file
usda_spp_kbs_genus <- usda_spp_kbs %>%
  dplyr::select(-Scientific.Name, -speciesName, -species, -growthDetail, -speciesName,
         -durationDetail, -status, -duration) %>%
  unique()
  
# pull out just the ones that have a genus but not species
kbs_spp5 <- kbs_spp4 %>%
  filter(speciesName == "sp") %>%
  dplyr::select(speciesFromSite, genusName, speciesName)

kbs_spp5.5 <- kbs_spp4 %>%
  tbl_df() %>%
  mutate(toretain = ifelse(genusName == "Unknown" | genusName == "unknown" | genusName == "Woody", 1,0)) %>%
  filter(toretain == 1) %>%
  dplyr::select(-toretain) %>%
  mutate(growth = ifelse(speciesName == "grass", "Graminoid", "Forb"),
         growth = ifelse(speciesName == "Woody", "Woody", growth)) %>%
  mutate(family = ifelse(speciesName == "Asteraceae", "Asteraceae", family),
         family = ifelse(speciesName == "Brassicaceae", "Brassicaceae", family),
         family = ifelse(speciesName == "Solanaceae", "Solanaceae", family)) %>%
  mutate(genusName = NA,
         speciesName = NA,
         species = speciesFromSite) %>%
  dplyr::select(-speciesFromSite)

# merge with usda and retain as much info as possible
kbs_spp6 <- left_join(kbs_spp5, usda_spp_kbs_genus) %>%
  tbl_df() %>%
  filter(growth != "") %>%
  # go with most likely scenario when multiple growth forms
  mutate(growth = ifelse(family == "Asteraceae", "Forb", growth)) %>%
  mutate(growth = ifelse(genusName == "Potentilla", "Forb", growth)) %>%
  mutate(growth = ifelse(genusName == "Rubus", "Subshrub", growth)) %>%
  mutate(growth = ifelse(genusName == "Crataegus", "Shrub", growth)) %>%
  mutate(growth = ifelse(genusName == "Prunus", "Tree", growth)) %>%
  mutate(growth = ifelse(genusName == "Geum", "Forb", growth)) %>%
  mutate(growth = ifelse(genusName == "Rosa", "Subshrub", growth)) %>%
  mutate(growth = ifelse(genusName == "Plantago", "Subshrub", growth)) %>%
  mutate(growth = ifelse(genusName == "Rosa", "Subshrub", growth)) %>%
  mutate(growth = ifelse(genusName == "Veronica", "Forb", growth)) %>%
  mutate(growth = ifelse(genusName == "Malus", "Shrub", growth)) %>%
  mutate(growth = ifelse(genusName == "Carex", "Graminoid", growth)) %>%
  unique() %>%
  mutate(growthDetail = NA,
         duration = NA,
         durationDetail = NA,
         status = NA) %>%
  mutate(species = speciesFromSite) %>%
  dplyr::select(-speciesFromSite)
  
# put it all together
kbs_spp <- rbind(kbs_spp2.5, kbs_spp4.5,  kbs_spp6, kbs_spp5.5)

##### CROSS SITE DATA ####

## Select treatment and habitat for cross-site synthesis
kbs_crosssite <- kbs_clean %>%
  filter(species != "UnSorted",
         species != "Surface Litter",
           species != "Unknown dicot (*)",
          species != "unknown Asteraceae",
           species != "Unknown grass",
           species != "Unkown Fabaceae",
           species != "Unknown Rosacae",
           species != "Unknown Solanaceae",
         species != "Standing Dead",
          species != "unknown Sedge",
           species != "unknown Brassicaceae",
           species != "Woody",
           species != "Unknown monocot (*)",
           species != "Aster sp. (*)",
         species != "Unknown") %>%
  filter(habitat == "undisturbed_unfertilized" & project == "T7") 

kbs_func <- left_join(kbs_clean, kbs_spp) %>%
  tbl_df() %>%
  group_by(year, site, habitat, project, plot, subplot, uniqueID, growth, unitAbund, scaleAbund) %>%
  summarize(abundance = sum(abundance)) %>%
  filter(!is.na(growth))

# remove mess
rm(usda_spp, kbs, kbs_clean, kbs_spp1, kbs_spp2, kbs_spp2.5, kbs_spp3, kbs_spp4, kbs_spp4.5, kbs_spp5, kbs_spp5.5, kbs_spp6, usda_spp_kbs, usda_spp_kbs_genus)

## Check that the removal of some sampling months didn't create empty reps
# kbscheck <- kbs_crosssite %>%
#   dplyr::select(year, uniqueID) %>%
#   unique() %>%
#   group_by(year) %>%
#   summarize(count = n())
