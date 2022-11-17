### CDR Grassland Plant Data ###
## Cristy Portales - May 9 2017 ##

## Data manipulation packages
library(tidyverse)
#source("utilities/data_import.R")

#READ IN DATA FROM EDI

## Read in e001:
# Package ID: knb-lter-cdr.14.6 Cataloging System:https://pasta.edirepository.org.
# Data set title: Plant aboveground biomass data: Long-Term Nitrogen Deposition: Population, Community, and Ecosystem Consequences.
# Data set creator:  David Tilman -  
# Metadata Provider:    - Cedar Creek LTER 
# Contact:  Dan Bahauddin - Information Manager Cedar Creek Ecosystem Science Reserve  - webmaster@cedarcreek.umn.edu
# Stylesheet v2.7 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

#inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/14/6/057e39850bd748d364df8a5ef60bb08d" 
#infile1 <- tempfile()
#download.file(inUrl1,infile1,method="curl")

                   
#cdr1 <-read.csv(infile1,header=F 
 #         ,skip=1
  #          ,sep="\t"  
   #     , col.names=c(
    #                "Exp",     
     #               "Year",     
      #              "Field",     
       #             "Plot",     
        #            "NTrt",     
         #           "NAdd",     
          #          "NitrAdd",     
           #         "NAtm.plus.NAdd",     
            #        "Species",     
             #       "Biomass"    ), check.names=TRUE, stringsAsFactors = F)
#rm(list = c("inUrl1", "infile1"))
#names(cdr1) <- c("Exp", "Year", "Field", "Plot", "NTrt","NAdd", "NitrAdd", "NAtm.NAdd", "Species", "Biomass")                                  

#Read in e002:
# Package ID: knb-lter-cdr.18.6 Cataloging System:https://pasta.edirepository.org.
# Data set title: Plant aboveground biomass data: Long-Term Nitrogen Deposition During Grassland Succession.
# Data set creator:  David Tilman -  
# Metadata Provider:    - Cedar Creek LTER 
# Contact:  Dan Bahauddin - Information Manager Cedar Creek Ecosystem Science Reserve  - webmaster@cedarcreek.umn.edu
# Stylesheet v2.7 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

#inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/18/6/b164ab6b1dc782e0c33714fc2f6522db" 
#infile1 <- tempfile()
#download.file(inUrl1,infile1,method="curl")

                   
#cdr2 <-read.csv(infile1,header=F 
 #         ,skip=1
  #          ,sep="\t"  
   #     , col.names=c(
    #                "Sampling.date.paren.mm.per.dd.per.yyyy.paren.",     
     #               "Field",     
      #              "Experiment.number",     
       #             "plot.number",     
        #            "Ntrt",     
         #           "NAdd.paren.g.per.m2.per.yr.paren.",     
          #          "BurnTrt",     
           #         "Species.Name",     
            #        "Species.Biomass..paren.g.per.m2.paren.",     
             #       "Maximum.plant.height..paren.cm.paren.",     
              #      "Median.plant.height..paren.cm.paren."    ), check.names=TRUE, stringsAsFactors = F)              
#rm(list = c("inUrl1", "infile1"))
#cdr2 <- cdr2[,1:9] #select solumns of interest
#names(cdr2) <- c("Date", "Field", "Exp", "Plot", "Ntrt", "Nadd", "BurnTrt", "Species", "Biomass")
               
###Alternately, read in from Google Drive
## I deleted meta data from csv files provided

## Read in e001
## download e001
#drive_download(file = "https://drive.google.com/file/d/1kuSyZIYBBmxMvenMHzmzNJeCpunFSP0v/view?usp=share_link")

cdr1 <- read.csv("cdr_e001.csv") %>%
  tbl_df()

## Read in e002
## download e002
drive_download(file = "https://drive.google.com/file/d/1nuRjSbE90ldZd0HyT-yM2LVvRkjxqDY4/view?usp=share_link")

cdr2 <- read.csv("cdr_e002.csv") %>%
  tbl_df()


## Clean up data e001
cdr1_clean <- cdr1 %>%
  tbl_df() %>%
  # In E001 NitrAdd is the same as NAdd column on e002, so I got rid of NAdd
  dplyr::select(-NAdd, -NAtm.NAdd, -NTrt) %>%
  tbl_df() %>%
  ## format column names to match the rest of the datasets
  ## In this case, habitat refers to Nitrogen Added
  mutate(site = "cdr",
         project = Exp,
         year = Year,
         plot = Field, 
         subplot = Plot,
         abundance = Biomass, 
         habitat = NitrAdd,
         unitAbund = "biomass_g_m2",
         scaleAbund = "m2",
         species = Species,
         uniqueID = paste(site, habitat, project, plot, subplot, sep = "_")) %>%
 dplyr::select(year, site, habitat, project, plot, subplot, uniqueID, species, abundance, unitAbund, scaleAbund) 


## Clean up data e002
## For e002 the date is formated as sampling date rather than year
## Extract the year from the character string
cdr2$Date <- as.character(cdr2$Date)
len <- nchar(cdr2$Date)
a <- substr(cdr2$Date, len-1, len)
a <- if_else(a > 50, paste0(19, a), paste0(20, a))
cdr2$Year <- a

cdr2_clean <- cdr2 %>%
  tbl_df() %>%
  ## No Burn Trt for 001, so I took it out from e002
  ## Ntrt
  dplyr::select(-BurnTrt, -Ntrt, -Date) %>%
  tbl_df() %>%
  ## format column names to match the rest of the datasets
  ## In this case, habitat refers to Nitrogen Added
  mutate(site = "cdr",
         project = Exp,
         year = Year,
         plot = Field, 
         subplot = Plot,
         abundance = Biomass, 
         habitat = Nadd,
         unitAbund = "biomass_g_m2",
         scaleAbund = "0.3m2",
         species = Species,
         uniqueID = paste(site, habitat, project, plot, subplot, sep = "_")) %>%
  dplyr::select(year, site, habitat, project, plot, subplot, uniqueID, species, abundance, unitAbund, scaleAbund) 

## Putting the two experiments in the same data frame
cdr_clean <- rbind(cdr1_clean,cdr2_clean)

cdr_clean <- cdr_clean %>%
  filter(species != "Forb seedlings",
         species != "Fungi",
         species != "Lichens",
         species != "Grass seedlings",
         species != "Miscellaneous herbs",
         species != "Miscellaneous herb",
         species != "Miscellaneous forb",
         species != "Miscellaneous litter",
         species != "Miscellaneous grasses",
         species != "Miscellaneous grasses 2",
         species != "Miscellaneous sp.",
         species != "Miscellaneous woody",
         species != "Miscellaneous Woody",
         species != "Miscellaneous woody plants",
         species != "Miscellaneous legumes",
         species != "Miscellaneous sedges",
         species != "Misc. forb",
         species != "Mosses",
         species != "Mosses & lichens",
         species != "Mosses & lichens 2",
         species != "Mosses and",
         species != "Mosses and lichens",
         species != "Pine cones",
         species != "Pine needles",
         species != "ambrosia sp.")%>%
  filter(habitat == "0") 

# These are other things we could take out if we want species, fut could keep for functional group
#species != "Allium sp.",
#species != "Amelanchier sp.",
#species != "Antennaria sp.",
#species != "Apocynum sp.",
#species != "Arabis sp.",
#species != "Aristida sp.",
#species != "Asclepias sp.",
#species != "Aster sp.",
#species != "Bromus sp.",
#species != "Calamovilfa sp.",
#species != "Carex sp.",
#species != "carex sp.",
#species != "Chenopodium sp.",
#species != "Cirsium sp.",
#species != "Cyperus sp.",
#species != "cyperus sp.",
#species != "Euphorbia sp.",
#species != "Equisetum sp.",
#species != "Erigeron sp.",
#species != "Galium sp.",
#species != "Helianthus sp.",
#species != "Hieracium sp.",
#species != "Juncus sp.",
#species != "Lactuca sp.",
#species != "Lithospermum sp.",
#species != "Melilotus sp.",
#species != "Oenothera sp.",
#species != "Oxalis sp.",
#species != "Panicum sp.",
#species != "Parthenocissus sp.",
#species != "Physalis sp.",
#species != "Pinus sp.",
#species != "Pinus strobus",
#species != "Prunus sp.",
#species != "Poa sp.",
#species != "Polygala sp.",
#species != "Polygonatum sp.",
#species != "Polygonum sp.",
#species != "Potentilla sp.",
#species != "Prunus sp.",
#species != "Quercus borealis",
#species != "Quercus borealis-ellipsoidalis",
#species != "Quercus ellipsoidalis",
#species != "Quercus macrocarpa",
#species != "Quercus sp.",
#species != "Rhus sp.",
#species != "Rubus sp.",
#species != "Rumex sp.",
#species != "Salix sp.",
#species != "Senecio sp.",
#species != "Silene sp.",
#species != "Setaria sp.",
#species != "Solanum sp.",
# species != "Solidago sp.",
# species != "Sporobolus sp.",
# species != "Tradescantia sp.",
# species != "Trifolium sp.",
# species != "Viola sp.")



## Get rid of plots that don't have 90% of the time series, no consecutive gaps
# Checking field A
cdr_a <- subset(cdr_clean, cdr_clean$plot == "A")
unique(cdr_a$year, incomparables = FALSE)
# A goes only to 2004 continously

cdr_b <- subset(cdr_clean, cdr_clean$plot == "B")
unique(cdr_b$year, incomparables = FALSE)
# A goes only to 2004 continously

cdr_c <- subset(cdr_clean, cdr_clean$plot == "C")
unique(cdr_c$year, incomparables = FALSE)
# A goes only to 2011 continously!

cdr_d <- subset(cdr_clean, cdr_clean$plot == "D")
unique(cdr_d$year, incomparables = FALSE)
# A goes only to 2004 continously

## We decided to use field C and only use the control plots
cdr_clean <- subset(cdr_clean, cdr_clean$plot == "C")
cdr_clean <- subset(cdr_clean, cdr_clean$year != "2014")

rm(cdr_a,
   cdr_b,
   cdr_c,
   cdr_d,
   cdr1_clean,
   cdr2_clean)

## Which experiment in Field C to use
cdr_c_1 <- subset(cdr_clean, cdr_clean$project == "1")
cdr_c_2 <- subset(cdr_clean, cdr_clean$project == "2")

unique(cdr_c_1$year, incomparables = FALSE)
# A goes only to 2011 continously!

unique(cdr_c_2$year, incomparables = FALSE)
# Too many gaps in C2

cdr_clean <- subset(cdr_clean, cdr_clean$project == "1") %>%
  group_by(year, site, habitat, project, plot, subplot, uniqueID,  species, unitAbund, scaleAbund) %>%
  summarize(abundance = mean(abundance))

rm(cdr_c_1,
   cdr_c_2)

# Q HERE ####
  ## what are the data from the USDA

## Read in the data from USDA
usda_spp <- read_csv_gdrive("0BwguSiR80XFZd08xLTIxbC1KSEU") %>%
  tbl_df() %>%
  # select relevant columns
  dplyr::select(Scientific.Name, Symbol, Genus, Species, Category, Family, Duration, Growth.Habit, Native.Status, Synonym.Symbol, Accepted.Symbol) %>%
  # create a detailed column
  mutate(growthDetail = Growth.Habit,
         durationDetail = Duration) %>%
  # create a general "growth" column
  # assuming the order is from most to least common
  separate(Growth.Habit, c("growth", "v2", "v3", "v4"), by =",") %>%
  dplyr::select(-v2, -v3, -v4) %>%
  # create a general "status" column
  separate(Native.Status, c("location", "status")) %>%
  dplyr::select(-location) %>%
  # create a general "duration" column
  separate(Duration, c("duration"), sep = ",") %>%
  mutate(symbol = Symbol, 
         speciesName = Species,
         genusName = Genus,
         family = Family,
         altSymbol = Synonym.Symbol,
         acceptedSymbol = Accepted.Symbol) %>%
  dplyr::select(Scientific.Name, symbol, altSymbol, acceptedSymbol, speciesName, genusName, family, duration, growth, status, growthDetail, durationDetail)

## Genus Cleaning ##


# unique species names from cdr
cdr_spp1 <- cdr_clean %>%
  dplyr::select(species) %>%
  unique() %>%
  mutate(species2 = species) %>%
  separate(species2, c("genusName", "speciesName")) 

# modify the usda_spp to link to cdr 
usda_spp_cdr <- usda_spp %>%
  mutate(species = Scientific.Name) %>%
  dplyr::select(-altSymbol, -acceptedSymbol, -symbol) %>%
  mutate(duration = as.character(duration),
         durationDetail = as.character(durationDetail),
         growth = as.character(growth),
         growthDetail = as.character(growth),
         family = as.character(family))

# merge those in
cdr_spp2 <- left_join(cdr_spp1, usda_spp_cdr, by=c("species", "genusName", "speciesName"))


# for the ones that are just by genus, create a linking usda file
usda_spp_cdr_genus <- usda_spp_cdr %>%
  dplyr::select(-Scientific.Name, -speciesName, -species, -growthDetail, -speciesName,
         -durationDetail, -status, -duration) %>%
  unique()


# pull out just the ones that have a genus but not species
cdr_spp_gen <- cdr_spp1 %>%
  dplyr::select(species, genusName, speciesName)


# merge with usda and retain as much info as possible
cdr_spp_gen2 <- left_join(cdr_spp_gen, usda_spp_cdr_genus) %>%
  tbl_df() %>%
  # go with most likely scenario when multiple growth forms
  mutate(growth = ifelse(family == "Asteraceae", "Forb", growth)) %>%
  mutate(growth = ifelse(genusName == "Potentilla", "Forb", growth)) %>%
  mutate(growth = ifelse(genusName == "Rumex", "Forb", growth)) %>%
  mutate(growth = ifelse(genusName == "Crataegus", "Shrub", growth)) %>%
  mutate(growth = ifelse(genusName == "Prunus", "Tree", growth)) %>%
  mutate(growth = ifelse(genusName == "Geum", "Forb", growth)) %>%
  mutate(growth = ifelse(genusName == "Rosa", "Subshrub", growth)) %>%
  mutate(growth = ifelse(genusName == "Plantago", "Subshrub", growth)) %>%
  mutate(growth = ifelse(genusName == "Rosa", "Forb", growth)) %>%
  mutate(growth = ifelse(genusName == "Veronica", "Forb", growth)) %>%
  mutate(growth = ifelse(genusName == "cyperus", "Graminoid", growth)) %>%
  mutate(growth = ifelse(genusName == "Carex", "Graminoid", growth)) %>%
  mutate(growth = ifelse(family == "Fabaceae", "Legume", growth)) %>%
  mutate(growth = ifelse(family == "Anacardiaceae", "Subshrub", growth)) %>%
  mutate(growth = ifelse(family == "Apocynaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Asclepiadaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Betulaceae", "Shrub", growth)) %>%
  mutate(growth = ifelse(family == "Boraginaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Brassicaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Campanulaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Caryophyllaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Chenopodiaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Cistaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Commelinaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Cyperaceae", "Graminoid", growth)) %>%
  mutate(growth = ifelse(family == "Equisetaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Euphorbiaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Fagaceae", "Tree", growth)) %>%
  mutate(growth = ifelse(family == "Geraniaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Iridaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Lamiaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Liliaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Molluginaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Onagraceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Oxalidaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Pinaceae", "Tree", growth)) %>%
  mutate(growth = ifelse(family == "Equisetaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Plantaginaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Poaceae", "Graminoid", growth)) %>%
  mutate(growth = ifelse(family == "Polemoniaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Polygalaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Rosaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Salicaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Santalaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Scrophulariaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Solanaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Violaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Polygonaceae", "Forb", growth)) %>%
  mutate(growth = ifelse(family == "Ranunculaceae", "Forb", growth)) %>%
  unique() #%>%
#  mutate(growthDetail = NA,
#         duration = NA,
#         durationDetail = NA,
#         status = NA) 

cdr_spp_gen3 <- cdr_spp_gen2 %>%
  tbl_df() %>%
  mutate(growth = ifelse(genusName == "carex", "Graminoid", growth)) %>%
  mutate(growth = ifelse(genusName == "cyperus", "Graminoid", growth)) %>%
  mutate(growth = ifelse(species == "Taraxicum officinalis", "Forb", growth)) %>%
  mutate(growth = ifelse(species == "Petalostemum purpureum", "Forb", growth)) %>%
  mutate(growth = ifelse(species == "Oxybaphus hirsutus", "Forb", growth)) %>%
  unique() 

## Format species list
cdr_spp <- cdr_spp_gen3

## Put it all together by Functional Group
cdr_func <- left_join(cdr_clean, cdr_spp_gen3) %>%
    group_by(year, site, habitat, project, plot, subplot, uniqueID, growth, unitAbund, scaleAbund) %>%
    summarize(abundance = sum(abundance))
cdr_func <- subset(cdr_func, cdr_func$growth != "")

## Cross site data set
cdr_crosssite <-  cdr_clean%>%
  tbl_df() 



## Species cleaning ##

# # retain the ones that are okay
# cdr_spp2.5 <- cdr_spp2 %>%
#   filter(!is.na(Scientific.Name)) %>%
#   dplyr::select(-Scientific.Name)


# # need to look through the ones that didn't line up
# # some species names are misspelled compared to USDA
# cdr_spp3 <- cdr_spp2 %>%
#  filter(is.na(Scientific.Name)) %>%
#  mutate(speciesFromSite = species,
#         genus = " ") %>%
# dplyr::select(speciesFromSite, genusName, speciesName) %>%
#  # update genus or species name to align
#  mutate(speciesName = as.character(speciesName),
#       genusName = as.character(genusName),
#       speciesName = ifelse(speciesFromSite == "Achillea millefolium(lanulosa)", "lanulosa", speciesName),
#       genusName = ifelse(speciesFromSite == "Agropyron repens", "Elymus", genusName),
#       speciesName = ifelse(speciesFromSite == "Agrostis alba", "lanulosa", speciesName),
#       speciesName = ifelse(speciesFromSite == "Achillea millefolium(lanulosa)", "lanulosa", speciesName),
#       speciesName = ifelse(speciesFromSite == "Achillea millefolium(lanulosa)", "lanulosa", speciesName),
#       speciesName = ifelse(speciesFromSite == "Achillea millefolium(lanulosa)", "lanulosa", speciesName),
#       speciesName = ifelse(speciesFromSite == "Rosa arkansana", "arkansana", speciesName),
#       speciesName = ifelse(speciesFromSite == "Rudbeckia serotina", "serotina", speciesName),
#       speciesName = ifelse(speciesFromSite == "Rumex acetosella", "acetosella", speciesName),
#       speciesName = ifelse(speciesFromSite == "Salsola kali", "kali", speciesName),
#       speciesName = ifelse(speciesFromSite == "Sargastrum Nutans", "nutans", speciesName),
#       genusName = ifelse(speciesFromSite == "Sargastrum Nutans", "Sorghastrum", genusName),
#       speciesName = ifelse(speciesFromSite == "Schizachyrium scoparium", "scoparium", speciesName),
#       speciesName = ifelse(speciesFromSite == "Scleria triglomerata", "triglomerata", speciesName),
#      )
# 
# 
# 
# ## This is where I got stuck fixing the names :(
# mutate(speciesName = as.character(speciesName),
# genusName = as.character(genusName),
# speciesName = ifelse(speciesFromSite == "Agropyron repens", "Elymus", genusName),
# speciesName = ifelse(speciesFromSite == "Agropyron repens", "repens", speciesName))


rm(cdr_clean, cdr_spp_gen, cdr_spp_gen2, cdr_spp_gen3, cdr_spp1, cdr_spp2, cdr_spp2.5, cdr_spp3, cdr1, cdr2, cdr1_clean, cdr2_clean, usda_spp, usda_spp_cdr, usda_spp_cdr_genus)
rm(a, len)


