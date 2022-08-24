#---------------------------
## Script name: Rarity Categorization
##
## Purpose of script: categorize species by rarity according to Wilfhart et al. paper
##
## Author: Carmen Ebel
##
## Email: cebel2@uoregon.edu
#---------------------------

## load packages
library(tidyverse)

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## set graphics theme
theme_set(theme_bw())

# Read in data ####
source("shape-shifting-subordinates/sev_edge_data_cleaning.R")
dat <- sev_edge_clean ## create new object

## info about the data
    ## years 2013 - 2021
    ## two fields - EDGE_black, EDGE_blue
    ## plots 1-32
    ## subplots 1-4
    ## three rainfall treatments: E, C, D
    ## C = control; E = event reduction by 66%; D = delayed monsoon season
    ## looks like 96 total species; currently includes EMPTY; UNKFORB1
    ## abundance data is biomass in g/m2

# Filter to Controls ####
sevC <- dat %>%
  filter(rainfall == "C") ## remove both rainfall treatments to only classify temporal strategy of species in control plots

## what should we do about species that are present only in drought plots, not control plots?

# Classify spatial & temporal rarity ####
## Rank sp by abundance ####
datrank = sevC %>%
  group_by(field, plot, subplot, rainfall) %>%
  mutate(rank = rank(abundance, na.last = NA, ties.method = "average"), percrank = percent_rank(abundance)) 
## this is the rank for each species in each subplot
## currently rank and percrank are opposing. 1 is the lowest rank, while 1.00 percrank means the highest biomass.
## greater rank percentile means that it is more abundant.


## Calc mean rank & yrs present ####
# mean rank percentile, sum number of years present, for each species in each plot.
categorydat <- datrank %>%
  group_by(field, plot, subplot, rainfall, species) %>%
  summarize(mean_rank = mean(percrank), # mean rank% cover for all 8 years (does not include zeros)
            yrs_present = length(percrank), # number of years species was present = number of rows for Taxon
            # (no zeros in working_coverdat dataset)
            yrs_present_list = paste0("yr", as.character(year), collapse = ", "),
            .groups = "keep")

# how many years per plot?
nyearsdat <- datrank %>%
  group_by(field, plot, subplot, rainfall) %>%
  summarize(n_years = length(unique(year)), .groups = "keep")

summary(nyearsdat)
# 7-9 years

## Create nicknames & categories ####
catdat <- categorydat %>%
  merge(y = nyearsdat) %>%
  mutate(persist_pct = yrs_present / n_years,
         core_cat = case_when(persist_pct < 0.5 ~ "transient",
                              persist_pct >= 0.5 ~ "core"), # if present half of the time or more, core
         core_cat = factor(core_cat, levels = c("transient", "core")),
         dom_cat = case_when(mean_rank < 0.5 ~ "subordinate",
                             mean_rank >= 0.5 ~ "dominant"), # if, on average, more dominant than half of the spp in the plots, dominant
         dom_cat = factor(dom_cat, levels = c("subordinate", "dominant"))) %>%
  unite(combo, core_cat, dom_cat, remove = FALSE, sep = "_") %>%
  merge(y = data.frame(combo = c("transient_subordinate", "transient_dominant", "core_subordinate", "core_dominant"),
                       nickname = c("TransSub", "TransDom", "CoreSub", "CoreDom")) ) %>%
  mutate(nickname = factor(nickname, levels = c("CoreDom", "CoreSub", "TransDom", "TransSub"))) #%>%
#arrange(site_code, block, plot_ordered, plot, trt, NPK, Fence, Taxon)

## this currently has everything classified at the subplot level. I believe I need to do this at the field level so that there is one classification for each species. 

## Retrying classification at the field level.
field_catdat <- categorydat %>%
  merge(y = nyearsdat) %>%
  mutate(persist_pct = yrs_present / n_years) %>%
  group_by(field, species) %>%
  summarise(mrank = mean(mean_rank), mpersist_pct = mean(persist_pct)) %>%
  mutate(core_cat = case_when(mpersist_pct < 0.5 ~ "transient",
                              mpersist_pct >= 0.5 ~ "core"), # if present half of the time or more, core
       core_cat = factor(core_cat, levels = c("transient", "core")),
       dom_cat = case_when(mrank < 0.5 ~ "subordinate",
                           mrank >= 0.5 ~ "dominant"), # if, on average, more dominant than half of the spp in the plots, dominant
       dom_cat = factor(dom_cat, levels = c("subordinate", "dominant"))) %>%
  unite(combo, core_cat, dom_cat, remove = FALSE, sep = "_") %>%
  merge(y = data.frame(combo = c("transient_subordinate", "transient_dominant", "core_subordinate", "core_dominant"),
                       nickname = c("TransSub", "TransDom", "CoreSub", "CoreDom")) ) %>%
  mutate(nickname = factor(nickname, levels = c("CoreDom", "CoreSub", "TransDom", "TransSub")))


# Separate dataframes by site ####
Edge_blue <- catdat %>%
  filter(field == "EDGE_blue") %>%
  mutate(unique_ID = paste(plot, subplot, rainfall,species, sep = "_"))

Edge_black <- catdat %>%
  filter(field == "EDGE_black") %>%
  mutate(unique_ID = paste(plot, subplot, rainfall,species, sep = "_"))


## clean up environment
rm(list = c("categorydat", "datrank", "sev_edge_clean", "nyearsdat"))
