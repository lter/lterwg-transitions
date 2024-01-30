### HAY Grassland Plant Data ###

## Data manipulation packages
library(tidyverse)
source("utilities/data_import.R")

## Read in the data
## NOTE: HAY is not a current site and has not been updated since previous synthesis
## Pulling the data in from the previous synthesis as a result
## If we want to go back to the original files (which also have temp and precip) they are at
## http://esapubs.org/archive/ecol/E088/161/

# Source USDA species cleaning code
source("Data_cleaning/sppcodes_cleaning.R")

## Read in data from old LTER master
hay <- read_csv_gdrive("0BwguSiR80XFZSnlSMWJWY1hVUTQ") %>%
  tbl_df() %>%
  filter(location == "HAY")

## Format for current synthesis
hay_clean <- hay %>%
  mutate(project = experiment,
         site = tolower(location),
         uniqueID = sitesubplot,
         habitat = NA,
         scaleAbund = "1m2") %>%
  dplyr::select(year, site, habitat, project, plot, subplot, uniqueID, species, abundance, unitAbund, scaleAbund) 

hay_crosssite <- hay_clean%>%
  tbl_df()

## Format species list
hay_spp <- hay %>%
  dplyr::select(species, family, duration, growth, durationDetail, growthDetail) %>%
  unique()

## Summarize by functional group
hay_func <- left_join(hay_clean, hay_spp) %>%
  group_by(year, site, habitat, project, plot, subplot, uniqueID, growth, unitAbund, scaleAbund) %>%
  summarize(abundance = sum(abundance))

rm(hay, hay_clean)



# Set cutoff for the proportion of sites occupied by a focal taxon
cutoff <- 0

# Subset and format the input data
hay.plot.avg <-  hay_crosssite %>%
  filter(year > 1900) %>%  # Remove first year
  group_by(year, plot, uniqueID, species) %>%
  dplyr::summarise(abundance = mean(abundance, na.rm = T), 
                   presence = ifelse(abundance>0, 1, 0)) %>%
  ungroup()

hay.prop.pres <- hay.plot.avg %>%
  group_by(species) %>%
  dplyr::summarise(tot.pres = sum(presence), 
                   n=length(presence)) %>%
  mutate(prop.pres = tot.pres/n) %>%
  arrange(-tot.pres) %>%
  ungroup() %>%
  filter(prop.pres > cutoff)

#Check data completeness of quadrats
ggplot(data = hay.plot.avg, aes(x = year, y = uniqueID)) +
  geom_point(size = 3) +
  theme_bw() +
  xlab("Year") +
  ylab("subplot")

# Drop taxa that comprise less than x percent of biomass

# Add up the total abundance of each taxon across all years and plots
#spp.total.abund <- sbc.plot.avg %>%
#  group_by(species) %>%
#  summarise(total.abundance = sum(abundance)) %>%
#  ungroup() %>%
# Sort and calculate cumulative sum
#  arrange(-total.abundance) %>%
#  mutate(cumsum.abundance = cumsum(total.abundance)) %>%  # Calculate cumulative sum
#  mutate(prop.total.abund = cumsum.abundance / sum(total.abundance)) %>%  # Calculate cumulative proportion
#  filter(prop.total.abund < cutoff)

# Restrict time series to the dominant taxa identified earlier
hay.dom.taxa <- hay.plot.avg %>%
  filter(species %in% hay.prop.pres$species) %>%
  mutate(log.abund = log(abundance + 1))

# Plot timeseries of each taxon at each site
library(ggplot2)

ggplot(data = hay.dom.taxa, aes(x = year, y = abundance, color = species)) +
  facet_wrap(~ uniqueID) +
  geom_line()

# Spread data to wide format
hay.dom.wide <-  hay.dom.taxa %>%
  mutate(yr_plot = paste(year, plot, sep = "_")) %>%
  dplyr::select(-abundance, -year, -plot, -presence) %>%
  spread(species, log.abund, fill = 0) %>%
  arrange(yr_plot)

# # Run an NMDS
# # --- Work in progress! ----
# row.names(hay.dom.wide) <- hay.dom.wide$yr_plot
# library(vegan)
# sort(unique(hay.dom.wide$yr_plot))
# mybraycurt <- vegdist(hay.dom.wide[,-1])
# my.nmds <- nmds(mybraycurt)
# summary(my.nmds)

#Calculate community synchrony for each site
#install.packages("codyn")
library("codyn")

Lsync <-  synchrony(df = hay.dom.taxa, 
                    time.var = "year", 
                    species.var="species", 
                    abundance.var="log.abund", 
                    replicate.var = "uniqueID")

#Import Coords 
#Importantantly, these are not actual lat and long coords,
#but rather relative plotting coords from eachother 
hay_coordinates <- read_csv_gdrive("0B27EzQgmuGLRQXlwLUZOY3lOdlU") %>%
  tbl_df()
#Match colnames to Lsync 
colnames(hay_coordinates) <- c("name","uniqueID","long","lat")


LZGeosync <- merge(Lsync, hay_coordinates, by = "uniqueID", all=T)
