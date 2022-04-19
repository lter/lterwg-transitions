## ---------------------------
##
## Script name: Shape-shifting subordinates, prelim analyses
##
## Purpose of script: data exploration; categorize sp by rarity
##
## Author: Carmen Ebel
##
## Email: cebel2@uoregon.edu
##
## ---------------------------


## load packages
library(tidyverse)

# graphics
library(ggplot2)
library(cowplot)
library(ggridges)
library(plotly)
library(ggExtra)

## read in data
source("sev_edge_data_cleaning.R")

dat <- sev_edge_clean

## info about the data
    ## years 2013 - 2021
    ## two fields - EDGE_black, EDGE_blue
    ## plots 1-32
    ## subplots 1-4
    ## three rainfall levels: E, C, D
    ## looks like 96 total species; currently includes EMPTY; UNKFORB1
    ## abundance data is biomass in g/m2

## visually explore the data
theme_set(theme_bw())

ggplot(dat, aes(x=rainfall, y=abundance)) +
  geom_boxplot()

trt_avg <- dat %>%
  group_by(year, field, rainfall, species) %>%
  summarise(avg_abundance = mean(abundance))
## not that informative

ggplot(trt_avg, aes(x=year, y=avg_abundance, color = species)) +
  geom_line() +
  facet_wrap(~rainfall) +
  coord_cartesian(ylim = c(0,150))
## what is the huge increase in abundance of one species in the E treatment for just one year?
## setting the ylim to 150 so that can see the majority of the species better


## Rank species in order of abundance
datrank = dat %>%
  group_by(field, plot, subplot, rainfall) %>%
  mutate(rank = rank(abundance, na.last = NA, ties.method = "average"), 
         percrank = percent_rank(abundance)) 

sort(unique(datrank$rank))
## ranks speceis 1-108, with half steps in between most numbers


## Classifying by spatial & temporal rarity, sensu Wilfhart et al. 
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
# 5-12 years (11 years treatment)

# Nicknames and categories: 
categorydat <- categorydat %>%
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


## visualize categories
ggplot(categorydat, aes(x=nickname)) +
  geom_bar() +
  facet_wrap(~rainfall) +
  theme_bw()

ggplot(categorydat, aes(x=rainfall)) +
  geom_bar() +
  facet_wrap(~nickname) +
  theme_bw()


## visualize continuously
  ## prep data
alltrtdat.graph <- categorydat %>%
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






### OLD ###

#########################################################.
# 3.3 ~ Figure 1: Scatterplot (Persist ~ Abund)  ####
### ONLY CONTROLS
scatterplot <- ggplot(ctrlcatdat.graph, aes(x = mean_rank, y = persist_pct)) +
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
  labs(size = "N")
scatterplot

ggsave("persist_abund_controls.png", height = 5, width = 5.5)


drought_scatterplot <- ggplot(Dcatdat.graph, aes(x = mean_rank, y = persist_pct)) +
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
  labs(size = "N")
drought_scatterplot








#########################################################.
# 3.4 ~ SI Figure 4: cor(persist ~ abund, by site) ####


ctrlcatdat_predict <- ctrlcatdat[,c('field', 'plot','persist_pct', 'mean_rank', 'nickname')]
ctrlcatdat_predict$fit <- predict(persistabund_corr_mod)

mainfit <- data.frame(x = seq(0,1,0.1))
mainfit$y <- mainfit$x * summary(persistabund_corr_mod)$coef[2,1] + summary(persistabund_corr_mod)$coef[1,1]

persistabund_corr_plot <-
  scatterplot +
  geom_line(data = ctrlcatdat_predict, aes(y = fit, x = mean_rank, group = site_code), color= 'blue', alpha = 0.25) +
  geom_line(data = mainfit, aes(y = y, x = x), color = 'black', lwd = 1.5) +
  ggtitle("Correlation Between Persistence and Abundance")  +
  labs(size = "N")
persistabund_corr_plot


## prep for continuous visualization
ctrlcatdat <- categorydat %>%
  filter(rainfall == "C") 

ctrlcatdat.graph <- ctrlcatdat %>%
  mutate(mean_rank_bin = cut(mean_rank, breaks = seq(from = 0, to = 1, by = 0.05), labels = FALSE),
         persist_pct_bin = cut(persist_pct, breaks = seq(from = 0, to = 1, by = 0.05), labels = FALSE)) %>%
  group_by(mean_rank_bin, persist_pct_bin) %>%
  summarize(Number.Taxa = length(species), .groups = "keep") %>%
  merge(y = data.frame(mean_rank_bin = 1:20,
                       mean_rank = seq(from = 0.025, to = 0.975, by = 0.05))) %>%
  merge(y = data.frame(persist_pct_bin = 1:20,
                       persist_pct = seq(from = 0.025, to = 0.975, by = 0.05)))


Dcatdat <- categorydat %>%
  filter(rainfall == "D") 

Dcatdat.graph <- Dcatdat %>%
  mutate(mean_rank_bin = cut(mean_rank, breaks = seq(from = 0, to = 1, by = 0.05), labels = FALSE),
         persist_pct_bin = cut(persist_pct, breaks = seq(from = 0, to = 1, by = 0.05), labels = FALSE)) %>%
  group_by(mean_rank_bin, persist_pct_bin) %>%
  summarize(Number.Taxa = length(species), .groups = "keep") %>%
  merge(y = data.frame(mean_rank_bin = 1:20,
                       mean_rank = seq(from = 0.025, to = 0.975, by = 0.05))) %>%
  merge(y = data.frame(persist_pct_bin = 1:20,
                       persist_pct = seq(from = 0.025, to = 0.975, by = 0.05)))

Ecatdat <- categorydat %>%
  filter(rainfall == "E") 

Ecatdat.graph <- Ecatdat %>%
  mutate(mean_rank_bin = cut(mean_rank, breaks = seq(from = 0, to = 1, by = 0.05), labels = FALSE),
         persist_pct_bin = cut(persist_pct, breaks = seq(from = 0, to = 1, by = 0.05), labels = FALSE)) %>%
  group_by(mean_rank_bin, persist_pct_bin) %>%
  summarize(Number.Taxa = length(species), .groups = "keep") %>%
  merge(y = data.frame(mean_rank_bin = 1:20,
                       mean_rank = seq(from = 0.025, to = 0.975, by = 0.05))) %>%
  merge(y = data.frame(persist_pct_bin = 1:20,
                       persist_pct = seq(from = 0.025, to = 0.975, by = 0.05)))




