#---------------------------
## Script name: Rarity Categorization
##
## Purpose of script: categorize species by rarity according to Wilfhart et al. paper
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu
#---------------------------

# Set up Data & Env ####
## load packages
library(tidyverse)

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## set graphics theme
theme_set(theme_bw())

# Read in data 
source("shape-shifting-subordinates/sev_edge_data_cleaning.R")
dat <- sev_edge_clean ## create new object



# Classify spatial & temporal rarity ####
## Biomass Data ####

### Filter to Controls ####
sevC <- dat %>%
  filter(rainfall == "C") 
    ## remove rainfall treatment 
    ## classify temporal strategy of species in control plots only

#### Q here ####
    ## what should we do about species that are present only in drought plots, not controls?

### Rank Sp by Abundance ####
datrank_bio = sevC %>%
  group_by(field, plot, subplot, rainfall) %>%
  mutate(rank = rank(abundance, na.last = NA, ties.method = "average"), percrank = percent_rank(abundance)) 
    ## this is the rank for each species in each subplot
    ## currently rank and percrank are opposing. 1 is the lowest rank, while 1.00 percrank means the highest biomass.
    ## greater rank percentile means that it is more abundant.

### Calc Mean Rank & Yrs Present ####
# mean rank percentile, sum number of years present, for each species in each plot.
categorydat_bio <- datrank_bio %>%
  group_by(field, plot, subplot, rainfall, species) %>%
  summarize(mean_rank = mean(percrank), # mean rank% cover for all years (does not include zeros)
            yrs_present = length(percrank), # number of years species was present = number of rows for Taxon
            # (no zeros in working_coverdat dataset)
            yrs_present_list = paste0("yr", as.character(year), collapse = ", "),
            .groups = "keep")

# how many years per plot?
nyearsdat <- datrank_bio %>%
  group_by(field, plot, subplot, rainfall) %>%
  summarize(n_years = length(unique(year)), .groups = "keep")

summary(nyearsdat)
# 7-9 years

### Create Categories ####
## Retrying classification at the field level.
field_catdat_bio <- categorydat_bio %>%
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



## Cover Data ####
### Rank Sp by Abundance ####
#datrank_cov = sevC %>%
 # group_by(field, plot, subplot, rainfall) %>%
#  mutate(rank = rank(cover, na.last = NA, ties.method = "average"), percrank = percent_rank(cover)) 
## this is the rank for each species in each subplot
## currently rank and percrank are opposing. 1 is the lowest rank, while 1.00 percrank means the highest biomass.
## greater rank percentile means that it is more abundant.

### Calc Mean Rank & Yrs Present ####
# mean rank percentile, sum number of years present, for each species in each plot.
#categorydat_cov <- datrank_cov %>%
 # group_by(field, plot, subplot, rainfall, species) %>%
 # summarize(mean_rank = mean(percrank), # mean rank% cover for all 8 years (does not include zeros)
      #      yrs_present = length(percrank), # number of years species was present = number of rows for Taxon
            # (no zeros in working_coverdat dataset)
       #     yrs_present_list = paste0("yr", as.character(year), collapse = ", "),
       #     .groups = "keep")

### Create Categories ####
## Retrying classification at the field level.
#field_catdat_cov <- categorydat_cov %>%
 # merge(y = nyearsdat) %>%
#  #mutate(persist_pct = yrs_present / n_years) %>%
#  group_by(field, species) %>%
 # summarise(mrank = mean(mean_rank), mpersist_pct = mean(persist_pct)) %>%
#  mutate(core_cat = case_when(mpersist_pct < 0.5 ~ "transient",
                             # mpersist_pct >= 0.5 ~ "core"), # if present half of the time or more, core
       #  core_cat = factor(core_cat, levels = c("transient", "core")),
       #  dom_cat = case_when(mrank < 0.5 ~ "subordinate",
                    #         mrank >= 0.5 ~ "dominant"), # if, on average, more dominant than half of the spp in the plots, dominant
      #   dom_cat = factor(dom_cat, levels = c("subordinate", "dominant"))) %>%
#  unite(combo, core_cat, dom_cat, remove = FALSE, sep = "_") %>%
#  merge(y = data.frame(combo = c("transient_subordinate", "transient_dominant", "core_subordinate", "core_dominant"),
  #                     nickname = c("TransSub", "TransDom", "CoreSub", "CoreDom")) ) %>%
#  mutate(nickname = factor(nickname, levels = c("CoreDom", "CoreSub", "TransDom", "TransSub")))


# Clean up environment ####
rm(list = c("categorydat_cov", "categorydat_bio", "datrank_cov", "datrank_bio", "sev_edge_clean", "nyearsdat"))
