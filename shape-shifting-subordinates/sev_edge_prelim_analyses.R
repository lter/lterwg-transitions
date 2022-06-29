---------------------------
## Script name: Shape-shifting subordinates, prelim analyses
##
## Purpose of script: data exploration; categorize sp by rarity
##
## Author: Carmen Ebel
##
## Email: cebel2@uoregon.edu
---------------------------

## load packages
library(tidyverse)

# graphics
library(ggplot2)
library(cowplot)
library(ggridges)
library(plotly)
library(ggExtra)
library(ggpubr)

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}


# Read in data ####
source("sev_edge_data_cleaning.R")
dat <- sev_edge_clean ## create new object

## info about the data
    ## years 2013 - 2021
    ## two fields - EDGE_black, EDGE_blue
    ## plots 1-32
    ## subplots 1-4
    ## three rainfall levels: E, C, D
    ## looks like 96 total species; currently includes EMPTY; UNKFORB1
    ## abundance data is biomass in g/m2


# Classify spatial & temporal rarity ####
## sensu Wilfhart et al. 

## Rank sp by abundance ####
datrank = dat %>%
  group_by(field, plot, subplot, rainfall) %>%
  mutate(rank = rank(abundance, na.last = NA, ties.method = "average"), percrank = percent_rank(abundance)) 
## this is the rank for each species in each subplot
## currently rank and percrank are opposing. 1 is the lowest rank, while 1.00 percrank means the highest biomass.
## greater rank percentile means that it is more abundant.

sort(unique(datrank$rank))
## ranks species 1-108, with half steps in between most numbers

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

## clean up environment
rm(list = c("categorydat", "datrank", "sev_edge_clean", "nyearsdat"))

## Separate dataframes by site ####
Edge_blue <- catdat %>%
  filter(field == "EDGE_blue")

Edge_black <- catdat %>%
  filter(field == "EDGE_black")


# Avg sp across rainfall trt ####
## For Edge_black field ####
avg_blk_rainfall <- Edge_black %>%
  mutate(num_transsub = ifelse(nickname == "TransSub", 1, 0),
         num_coredom = ifelse(nickname == "CoreDom", 1, 0), 
         num_coresub = ifelse(nickname == "CoreSub", 1, 0), 
         num_transdom = ifelse(nickname == "TransDom", 1, 0)) %>%
  group_by(rainfall, species) %>%
  summarise(perc_transsub = sum(num_transsub)/n(),
            perc_coredom = sum(num_coredom)/n(),
            perc_coresub = sum(num_coresub)/n(),
            perc_transdom = sum(num_transdom)/n())
  
avg_blk_long <- avg_blk_rainfall %>%
  pivot_longer(cols = perc_transsub:perc_transdom, names_to = "category_name", values_to = "category_percent") %>%
  filter(category_percent > 0, species != "EMPTY")

## For Edge_blue field ####
avg_blu_rainfall <- Edge_blue %>%
  mutate(num_transsub = ifelse(nickname == "TransSub", 1, 0),
         num_coredom = ifelse(nickname == "CoreDom", 1, 0), 
         num_coresub = ifelse(nickname == "CoreSub", 1, 0), 
         num_transdom = ifelse(nickname == "TransDom", 1, 0)) %>%
  group_by(rainfall, species) %>%
  summarise(perc_transsub = sum(num_transsub)/n(),
            perc_coredom = sum(num_coredom)/n(),
            perc_coresub = sum(num_coresub)/n(),
            perc_transdom = sum(num_transdom)/n())

avg_blu_long <- avg_blu_rainfall %>%
  pivot_longer(cols = perc_transsub:perc_transdom, names_to = "category_name", values_to = "category_percent") %>%
  filter(category_percent > 0, species != "EMPTY")

## Vis sp category patterns across rainfall trts ####
ggplot(avg_blk_long, aes(x=rainfall, y=category_percent, color = category_name)) +
  geom_point() +
  facet_wrap(~species) +
  ylab("Percent per Category") + xlab("Rainfall Treatment")

ggplot(avg_blu_long, aes(x=rainfall, y=category_percent, color = category_name)) +
  geom_point() +
  facet_wrap(~species) +
  ylab("Percent per Category") + xlab("Rainfall Treatment")


# ID where cat transitions occur ####

#blk_transitions <- avg_blk_long %>%
  #group_by(species) %>%
 # mutate(trans_occurs = ifelse())
## filter if category_percent is different between rainfall treatments

## for each species, I want to look at all three rainfall conditions and determine whether the values of category_percent are equal or different.

blk_species <- unique(avg_blk_long$species) ## create vector of unique species

## create empty dataframe
transitions <- data.frame(matrix(ncol=5, nrow=0))
colnames(transitions) <- c("rainfall", "species", "category_name", "category_percent", "trans_occurs")


for (i in 1:length(blk_species)) {
  temp_sp <- avg_blk_long %>%
    filter(species == blk_species[i])
  
  ## find unique categories
  catnum <- unique(temp_sp$category_name)
  
  ## categorize whether a transition occurs by evaluating whether the number of categories is 1 (no transition, coded as 0) or greater than 1 (transition, coded as 1)
  temp <- temp_sp %>%
    mutate(trans_occurs = ifelse(length(catnum) > 1, 1, 0))
  
  ## append to the empty dataframe
  transitions <- rbind(transitions, temp)
  
}

## clean this up and get just one row for each species.
trans <- transitions %>%
  group_by(species) %>%
  summarise(trans = median(trans_occurs))


## could also calculate the percent of times that each species falls into a category for each given rainfall treatment. That might be helpful to get a sense of whether species are consistently in one category or another or whether there is a lot of variation.
## currently each plot, sub combo has a unique category for each species. Might need to summarise across rainfall treatment and species to get the average condition of a species in these categories.
## then from there, I could track the response of the species abundance to drought and then how species in each category tend to respond to drought.


# Visually explore data ####

theme_set(theme_bw())


## Visualize rarity continuously ####
  ## prep data
alltrtdat.graph <- catdat %>%
  mutate(mean_rank_bin = cut(mean_rank, breaks = seq(from = 0, to = 1, by = 0.05), labels = FALSE),
         persist_pct_bin = cut(persist_pct, breaks = seq(from = 0, to = 1, by = 0.05), labels = FALSE)) %>%
  group_by(mean_rank_bin, persist_pct_bin, rainfall) %>%
  summarize(Number.Taxa = length(species), .groups = "keep") %>%
  merge(y = data.frame(mean_rank_bin = 1:20,
                       mean_rank = seq(from = 0.025, to = 0.975, by = 0.05))) %>%
  merge(y = data.frame(persist_pct_bin = 1:20,
                       persist_pct = seq(from = 0.025, to = 0.975, by = 0.05)))


  ## visualize
all_continuous <- ggplot(alltrtdat.graph, aes(x = mean_rank, y = persist_pct, color = rainfall)) +
  geom_hline(linetype = "longdash", yintercept = 0.5, colour = "grey40") +
  geom_vline(linetype = "longdash", xintercept = 0.5, colour = "grey40") +
  geom_point(aes(size = Number.Taxa), pch = 21, alpha = 0.3, color = 'darkslategray', fill = 'navy', show.legend = T)+
  scale_x_continuous(limits = c(0, 1))+
  scale_y_continuous(limits = c(0, 1))+
  labs(x ="Average Rank Abundance", y = "Proportion of Years Present")+
  theme(legend.position = 'none',
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(size = 15), 
        axis.title = element_text(size = 10),panel.grid.minor = element_blank())+
  theme_classic() + 
  scale_size_continuous(breaks = c(25, 50, 75, 100)) +
  labs(size = "N") +
  facet_wrap(~rainfall, ncol = 3)
all_continuous