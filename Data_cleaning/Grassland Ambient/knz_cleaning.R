### KNZ Grassland Plant Data ###

## Data manipulation packages
library(tidyverse)
source("utilities/data_import.R")

# Source USDA species cleaning code
source("Data_cleaning/sppcodes_cleaning.R")


# Package ID: knb-lter-knz.69.7 Cataloging System:https://pasta.lternet.edu.
# Data set title: PVC02 Plant Species Composition on Selected Watersheds at Konza Prairie.
# Data set creator:  David Hartnett -  
# Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
# Contact:    - KNZ-LTER Data Manager   - knzlter@ksu.edu
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-knz.69.7
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 


## Read in the data directly from portal.lter.net
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/69/7/71b9a42ccc28af53ca56a2625adc688b" 
infile1 <- sub("^https","http",infile1) 
knz <-read.csv(infile1, header=T)
knz <- tbl_df(knz)

## Clean up data
knz_clean <- knz %>%
  ## convert cover classes to mean values within that class
  mutate(cover = .1, 
         cover = ifelse(Cover == 2, 3.5, cover),
         cover = ifelse(Cover == 3, 15, cover),
         cover = ifelse(Cover == 4, 37.5, cover),
         cover = ifelse(Cover == 5, 62.5, cover),
         cover = ifelse(Cover == 6, 85, cover), 
         cover = ifelse(Cover == 7, 97.5, cover)) %>%
  ## transect e became d and the original d was abandoned in watershed n20b f soils in 1987
  ## swap it in accordingly
  mutate(toremove = ifelse(SoilType == "f" & Transect == "d" & WaterShed == "n20b" & RecYear < 1987, 1, 0)) %>%
  filter(toremove == 0) %>%
  mutate(Transect = as.character(Transect)) %>%
  mutate(Transect = ifelse(Transect == "e", "d", Transect)) %>%
  ## make a unique species column
  mutate(species = paste(Abgenus, Abspecies, sep = "_")) %>%
  dplyr::select(-SpeCode, -Pid, RecDay, -Abgenus, -Abspecies, -Cover) %>%
  tbl_df() %>%
  ## sometimes they were sampled multiple times within a year; average the value for the species within that year
  ## might discuss and perhaps decide to always do the earliest or latest sampling date
  group_by(WaterShed, species, RecYear, SoilType, Transect, Plot) %>%
  summarize(cover = mean(cover)) %>%
  tbl_df() %>%
  ## format column names to match the rest of the datasets
  mutate(site = "knz",
         project = WaterShed,
         year = RecYear,
         plot = Transect, 
         subplot = Plot,
         abundance = cover, 
         habitat = SoilType,
         unitAbund = "percentcover",
         scaleAbund = "10m2",
         uniqueID = paste(site, habitat, project, plot, subplot, sep = "_")) %>%
  dplyr::select(year, site, habitat, project, plot, subplot, uniqueID, species, abundance, unitAbund, scaleAbund) %>%
  filter(abundance > 0)

## SPECIES DATA ##
## Create a list of all species found with their new scientific names
knz_spp1 <- knz_clean %>%
  dplyr::select(species) %>%
  unique() %>%
  mutate(oldSpecies = species,
         # the asclepius have the same first letters, so doing this now then fixing later
         species = ifelse(species == "asclep_virdf", "asclep_virid", species),
         # most of these are out of date species names or have a knz-specific abbrev
         species = ifelse(species == "asclep_virds", "asclep_virid", species),
         species = ifelse(species == "hedyot_nigri", "stenar_nigri", species),
         species = ifelse(species == "buchlo_dacty", "boutel_dacty", species),
         species = ifelse(species == "coroni_varia", "securi_varia", species),
         species = ifelse(species == "bromus_japon", "bromus_arven", species),
         species = ifelse(species == "gaura_molli", "oenoth_curti", species),
         species = ifelse(species == "krigia_cespi", "krigia_caesp", species),
         species = ifelse(species == "onosmo_molle", "onosmo_bejar", species),
         species = ifelse(species == "echino_crusg", "echino_crus-", species),
         species = ifelse(species == "hespst_spart", "hesper_spart", species),
         species = ifelse(species == "penste_tubif", "penste_tubae", species),
          # species name updates from Scott
         species = ifelse(species == "chamcr_fasci", "chamae_fasci", species),
         species = ifelse(species == "isanth_brach", "tricho_brach", species))

## Format the usda data to match the knz codes
usda_spp_knz <- usda_spp %>%
  mutate(species = tolower(paste(substr(genusName, 1, 6), substr(speciesName, 1, 5), sep = "_"))) %>%
  mutate(speciesName = as.character(speciesName),
         genusName = as.character(genusName),
         growthDetail = as.character(growthDetail),
         durationDetail = as.character(durationDetail),
         family = as.character(family))

## Merge konza and usda and clean
knz_spp2 <- left_join(knz_spp1, usda_spp_knz) %>%
  dplyr::select(-symbol, -altSymbol, -acceptedSymbol) %>%
  unique() %>%
  mutate(speciesName = as.character(speciesName),
         genusName = as.character(genusName)) %>%
  # remove times when there was an extra species that matched the code
  filter(speciesName != "brevicaulis",
         speciesName != "breviculmis",
         speciesName != 'longiberbe', 
         speciesName != "austroalpina",
         speciesName != "austrocaroliniana",
         speciesName != "austrodeflexa",
         speciesName != "ciliaris",
         speciesName != "pedata",
         speciesName != "capillarioides",
         speciesName != "crus-pavonis",
         speciesName != "macrocentra") %>%
  filter(genusName != "Chamaecrista") %>%
  # clean up redundant records
  mutate(status = as.character(status),
         status = ifelse(species == "achill_mille", "N", status),
         status = ifelse(species == "ambros_artem", "N", status),
         durationDetail = as.character(durationDetail),
         growthDetail = as.character(growthDetail),
         durationDetail = ifelse(species == "eriger_strig", "Annual, Biennial, Perennial", durationDetail),
         durationDetail = ifelse(species == "descur_pinna", "Annual, Biennial, Perennial", durationDetail),
         duration = ifelse(species == "eriger_strig", "Annual", duration),
         status = ifelse(species == "euphor_denta", "NI", status),
         status = ifelse(species == "bromus_inerm", "NI", status),
         status = ifelse(species == "chenop_album", "NI", status),
         status = ifelse(species == "poa_prate", "NI", status),
         status = ifelse(species == "taraxa_offic", "NI", status),
         status = ifelse(species == "polygo_scand", "NI", status),
         growthDetail = ifelse(species == "galium_circa", "Forb/herb, Subshrub", growthDetail),
         growthDetail = ifelse(species == "monard_fistu", "Forb/herb, Subshrub", growthDetail),
         growth = ifelse(species == "monard_fistu", "Forb/herb", growth),
         growth = ifelse(species == "galium_circa", "Forb/herb", growth),
         speciesName = ifelse(oldSpecies == "asclep_virdf", "viridiflora", speciesName),
         speciesName = ifelse(oldSpecies == "asclep_virds", "viridis", speciesName)) %>%
  mutate(todelete = ifelse(species == "hordeu_pusil" & is.na(status), 1, 0),
         todelete = ifelse(genusName == "Opuntia" & growth == "Tree", 1, todelete),
         todelete = ifelse(species == "vicia_ameri" & is.na(status), 1, todelete),
         todelete = ifelse(species == "chenop_album" & growthDetail == "", 1, todelete),
         todelete = ifelse(species == "chenop_berla" & growthDetail == "", 1, todelete),
         todelete = ifelse(species == "pseudo_obtus" & genusName == "Pseudocrossidium", 1, todelete),
         todelete = ifelse(species == "medica_lupul" & is.na(status), 1, todelete),
         todelete = ifelse(species == "eragro_cilia" & is.na(status), 1, todelete)) %>%
  filter(todelete == 0) %>%
  unique() %>%
  group_by(species) %>%
  mutate(ncount = n()) %>%
  dplyr::select(-ncount, -todelete) %>%
  tbl_df()

# identify the mised species
missedspecies <- c(setdiff(knz_spp1$species, knz_spp2$species))

# format the missed species
knz_spp3 <- left_join(knz_spp1[which(knz_spp1$species%in%missedspecies),], usda_spp_knz) %>%
  dplyr::select(-symbol, -altSymbol, -acceptedSymbol) %>%
  mutate(growth = ifelse(species == "carex_spp.", "Graminoid", growth),
         family = ifelse(species == "carex_spp.", "Cyperaceae", family),
         family = ifelse(species == "euphor_spp.", "Euphorbiaceae", family),
         family = ifelse(species == "cyperu_spp.", "Cyperaceae", family),
         growth = ifelse(species == "annual_forb", "Forb", growth),
         duration = ifelse(species == "annual_forb", "Annual", duration),
         growth = ifelse(species == "euphor_spp.", "Forb", growth ),
         growth = ifelse(species =="cyperu_spp.", "Graminoid", growth),
         growth = ifelse(species == "symphy_spp.", "Forb", growth),
         growth = ifelse(species == "chamae_fasci", "Forb", growth),
         duration = ifelse(species == "chamae_fasci", "Annual", duration)) %>%
  tbl_df()

# Put it all together
knz_spp <- rbind(knz_spp2, knz_spp3) %>%
  mutate(updatedSpecies = species,
         species = oldSpecies) %>%
  dplyr::select(-oldSpecies)
length(intersect(knz_spp$species, knz_spp2$species))


#### SPECIES LEVEL DATA ####

## Need to decide which treatments and locations to keep if we were to do a hierarchical scaling
knz_lengthcheck <- knz_clean %>%
  tbl_df() %>%
  dplyr::select(project, year, habitat) %>%
  unique() %>%
  tbl_df() %>%
  group_by(project, habitat) %>%
  summarize(nyear = n())

# For the hierarchy project we are going to use watershed 001d because it has the most complete time series, is annually burned
# for 001d we are using tully (lowland) and florence (upland) soil types. Within those we are using the 4 transects and 5 subplots in each
knz_hierarchy <- knz_clean %>%
  tbl_df() %>%
  filter(project == "001d")%>%
  filter(habitat=="t"|habitat=="f")


## For cross-site focus on 001d because it has a long continuous time series and burning is at same interval as sampling
## Need to decide how to treat soil type (should they be different sites?)
knz_crosssite <- knz_clean %>%
  tbl_df() %>%
  filter(project == "001d") 


### FUNCTIONAL GROUP DATA ###
knz_func <- left_join(knz_clean, knz_spp) %>%
  group_by(year, site, habitat, project, plot, subplot, uniqueID, growth, unitAbund, scaleAbund) %>%
  summarize(abundance = sum(abundance))


knz_func_001d <- left_join(knz_clean, knz_spp) %>%
  filter(project == "001d") %>%
  group_by(year, site, habitat, project, plot, subplot, uniqueID, growth, unitAbund, scaleAbund) %>%
  summarize(abundance = sum(abundance))

### COORDINATES ###
knz_coords <- read_csv_gdrive("11qtwfPub-srGw9onrdoaKVjCsOh49qtG")
colnames(knz_coords)<-c("watershed","plotnum","soil","transect","dataid","datacode","longitude","latitude")

# write.csv(knz_crosssite, "knz_001d.csv")
# write.csv(knz_spp, "knz_spp.csv")
# write.csv(knz_func_001d, "knz_func_001d.csv")
#  write.csv(knz_func, "knz_func.csv")


## For potential within-site scaling analyses look at length and discuss how to approach treatments
## notes: no data were collected for r20b in transect A and B in fall 2011 due to wildfire
## need to check that each area has a continuous record
## definitely will want to remove the short ones


    