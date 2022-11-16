### JRG Serpentine Grassland Plant Data ###

## Data are collected by R. J. Hobbs and L. M. Hallett
## Data provided by LMH 

## Data manipulation packages
library(tidyverse)
source("utilities/data_import.R")

# Source USDA species cleaning code
source("Data_cleaning/sppcodes_cleaning.R")

## Read in data
jrg <- read_csv_gdrive("0BwguSiR80XFZMjljT0hhakVEaTg") %>%
  tbl_df() 

## Format for  synthesis
jrg_clean <- jrg %>%
  mutate(SpeciesCode = as.character(SpeciesCode),
         year = SamplingYear,
         species = SpeciesCode,
         project = treatment,
         site = "jrg",
         habitat = NA,
         abundance = cover,
         subplot = plot, 
         plot = trtrep,
         unitAbund = "percentcover",
         scaleAbund = "1m2") %>%
  dplyr::select(year, site, habitat, project, plot, subplot, uniqueID, species, abundance, unitAbund, scaleAbund) %>%
  filter(abundance > 0) %>%
  mutate(species = ifelse(species == "BR__", "BRSP", species),
         species = ifelse(species == "BRMO", "BRHO", species)) %>%
  # not sure what this is, only was in one plot once
  filter(species != "MISP")

jrg_crosssite <- jrg_clean

## format species data
## Read in data from old LTER master
jrg_spp <- read_csv_gdrive("0BwguSiR80XFZSnlSMWJWY1hVUTQ") %>%
  tbl_df() %>%
  filter(location == "JRG") %>%
  dplyr::select(species, oldSpecies, family, duration, growth, durationDetail, growthDetail) %>%
  unique() %>%
  separate(species, c("genusName", "speciesName")) %>%
  # match up the species names 
  mutate(species = toupper(paste(substr(genusName, 1,2), substr(speciesName, 1, 2), sep = ""))) %>%
  mutate(species = ifelse(species == "BOCA", "MICA", species),
         species = ifelse(species == "CADE", "ORDE", species),
         species = ifelse(species == "ELMU", "SIJU", species),
         species = ifelse(species == "EPBR", "EPPA", species),
         species = ifelse(species == "HECO", "HELU", species),
         species = ifelse(species == "HESP", "EVSP", species),
         species = ifelse(species == "NAPU", "STPU", species),
         species = ifelse(species == "CRCO", "TIER", species),
         species = ifelse(species == "LOWR", "LOSU", species),
         species = ifelse(species == "POSE", "POSC", species),
         species = ifelse(species == "TRWI", "TRTR", species),
         species = ifelse(species == "BRBE", "BRTR", species),
         species = ifelse(genusName == "minuartia", "ARDO", species),
         species = ifelse(genusName == "leptosiphon", "LIAN", species)) %>%
  # add nativity
  mutate(status = "N",
         status = ifelse(species == "BRHO", "I", status),
         status = ifelse(species == "LOMU", "I", status),
         status = ifelse(species == "LASP", "I", status),
         status = ifelse(species == "BRTR", "I", status)) %>%
  mutate(updatedSpecies = paste(genusName, speciesName, sep ="_")) %>%
  dplyr::select(-oldSpecies) %>%
  # format growth to match others
  separate(growth, c("growth"))

## Summarize by functional group
jrg_func <- left_join(jrg_clean, jrg_spp) %>%
  group_by(year, site, habitat, project, plot, subplot, uniqueID, growth, unitAbund, scaleAbund) %>%
  summarize(abundance = sum(abundance))

jrg.coords<-read_csv_gdrive("0B3XITcJWZbxSekxMZTZaUnc3XzQ")

jrg.coords <- jrg.coords %>%
  arrange(uniqueID)
coords<-as.matrix(jrg.coords[,2:3])

jrg.soildepth<-read_csv_gdrive("0B3XITcJWZbxSVWdSTGlEand3YTg")

jrg.soildepth <-jrg.soildepth %>%
  arrange(uniqueID)

jrg_crosssite2 <- jrg_crosssite %>%
  spread(key = species, value = abundance, fill = 0) %>%
  gather(key = species, value = abundance, AGHE:VUMI)

jrg_data_array<-array(NA, dim=c(length(unique(jrg_crosssite2$species)),length(unique(jrg_crosssite2$uniqueID)),
                                length(unique(jrg_crosssite2$year))))

for(spp in unique(jrg_crosssite2$species)){
  mydat<- jrg_crosssite2 %>%
    filter(species == spp) %>%
    spread(year, abundance, fill=0) %>%
    dplyr::select(-(site:scaleAbund))
  
  mydat<-mydat[,-1]
  
  jrg_data_array[unique(jrg_crosssite2$species)==spp,,]<-as.matrix(mydat)
}

jrg<-list(dataArray=jrg_data_array, coords=coords, time=min(jrg_crosssite2$year):max(jrg_crosssite2$year),
          habitat=NA)

# clean up
rm(jrg_clean,jrg_crosssite2,jrg_data_array,mydat,coords)
