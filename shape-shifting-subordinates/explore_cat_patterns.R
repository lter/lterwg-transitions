## Script name: Exploring Rarity Category Patterns
##
## Purpose of script: explore patterns in categories' responses to drought
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

library(tidyverse)
theme_set(theme_bw())


# Load Data ####
source("shape-shifting-subordinates/sev_rarity_categorization.R")

# Merge Ctrl & Tx Data ####
## first combine the field averaged control data with the drought treatment data
alldat <- left_join(dat, field_catdat_bio, by = c("field", "species"))


# Temporal Patterns ####
cats_grouped <- alldat %>%
  group_by(field, nickname, rainfall, year, SPEI.comp, season.precip) %>%
  summarise(mean_abund = mean(abundance, na.rm = T))

## Through time ####
ggplot(cats_grouped[cats_grouped$field == "EDGE_black",], aes(x=year, y=mean_abund, color = nickname)) +
  geom_line(size = 0.8) +
  facet_wrap(~rainfall) +
  ggtitle("EDGE Black Field") +
  geom_vline(xintercept = 2017, linetype = "dashed") +
  xlab("Year") + ylab("Mean Abundance of Category (g)")
#ggsave("shape-shifting-subordinates/preliminary_figs/black_cats_temporal.png", width = 5, height = 3)

ggplot(cats_grouped[cats_grouped$field == "EDGE_blue",], aes(x=year, y=mean_abund, color = nickname)) +
  geom_line(size = 0.8) +
  facet_wrap(~rainfall) +
  ggtitle("EDGE Blue Field") +
  geom_vline(xintercept = 2017, linetype = "dashed") +
  xlab("Year") + ylab("Mean Abundance per Category (g)")
#ggsave("shape-shifting-subordinates/preliminary_figs/blue_cats_temporal.png", width = 5, height = 3)



## By SPEI ####
ggplot(cats_grouped[!is.na(cats_grouped$nickname),], aes(x=SPEI.comp, y=mean_abund, color = rainfall)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#70a494", "#ca562c")) +
  facet_wrap(~nickname*field, ncol = 4, nrow = 2)
ggsave("shape-shifting-subordinates/preliminary_figs/spei-cats.png", width = 6, height = 3.5)

#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c

## By Precip ####
ggplot(cats_grouped[!is.na(cats_grouped$nickname),], aes(x=season.precip, y=mean_abund, color = rainfall)) +
  geom_point() +
  #geom_line() +
  scale_color_manual(values = c("#70a494", "#ca562c")) +
  facet_wrap(~nickname*field, ncol = 4, nrow = 2) +
  geom_smooth(method = "lm")
ggsave("shape-shifting-subordinates/preliminary_figs/precip-cats.png", width = 6, height = 3.5)

# Avg Drought Response ####
d_resp <- alldat %>% ## calc drought response
  group_by(field, rainfall, species, nickname) %>%
  summarise(mean_abund = mean(abundance, na.rm = T)) %>%
  pivot_wider(names_from = rainfall, values_from = mean_abund) %>%
  mutate(abund_diff = C - E)


#f6d2a9,#f5b78e,#f19c7c,#ea8171,#dd686c,#ca5268,#b13f64

ggplot(d_resp, aes(x=nickname, y=abund_diff, color = field)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("") + ylab("Control - Drought Biomass (g)") +
  scale_color_manual(values = c("#ca5268", "#f5b78e")) +
  theme(axis.text.x = element_text(angle = 25))
ggsave("shape-shifting-subordinates/preliminary_figs/d_sensitivity.png", width = 4.5, height = 2.75)


# Drought vs. Recovery ####
recovery <- alldat %>%
  group_by(field, rainfall, nickname, treatment_year) %>%
  summarise(mean_abund = mean(abundance, na.rm = T), SE_abund = calcSE(abundance))

ggplot(recovery, aes(x=nickname, y=mean_abund, fill = treatment_year)) +
  geom_errorbar(aes(ymin = mean_abund - SE_abund, ymax = mean_abund + SE_abund), width = 0.45) +
  geom_point(size = 4, pch = 21) +
  facet_wrap(~field) +
  scale_fill_manual(values = c("#70a494","#ca562c","#f6edbd")) +
  xlab(" ")


ggplot(recovery[recovery$field == "EDGE_black",], aes(x=treatment_year, y=mean_abund, fill = nickname)) +
  geom_point(size = 3, pch = 21) +
  facet_wrap(~nickname) +
  ggtitle("EDGE Black Field") + xlab(" ")
ggsave("shape-shifting-subordinates/preliminary_figs/recov_blk.png", width = 6.5, height = 4)

ggplot(recovery[recovery$field == "EDGE_blue",], aes(x=treatment_year, y=mean_abund, fill = nickname)) +
  geom_point(size = 3, pch = 21) +
  facet_wrap(~nickname) +
  ggtitle("EDGE Blue Field") +
  xlab(" ")
ggsave("shape-shifting-subordinates/preliminary_figs/recov_blu.png", width = 6.5, height = 4)

## separate out subordinate species
sub_recovery <- recovery %>%
  filter(nickname != "CoreDom", nickname != "TransDom")

ggplot(sub_recovery, aes(x=nickname, y= mean_abund, fill = treatment_year)) +
  geom_errorbar(aes(ymin = mean_abund - SE_abund, ymax = mean_abund + SE_abund), width = 0.25) +
  geom_point(size = 3, pch = 21) +
  facet_wrap(~field) +
  scale_fill_manual(values = c("#70a494","#ca562c","#f6edbd")) +
  xlab(" ")
#ggsave("shape-shifting-subordinates/preliminary_figs/sub_sp_recov.png", height = 3, width = 5)

## separate out dominant species
dom_recovery <- recovery %>%
  filter(nickname != "CoreSub", nickname != "TransSub")

ggplot(dom_recovery, aes(x=nickname, y= mean_abund, fill = treatment_year)) +
  geom_errorbar(aes(ymin = mean_abund - SE_abund, ymax = mean_abund + SE_abund), width = 0.25) +
  geom_point(size = 3, pch = 21) +
  facet_wrap(~field) +
  scale_fill_manual(values = c("#70a494","#ca562c","#f6edbd")) +
  xlab(" ") + ylab("Mean Abundance (g)")

#ggsave("shape-shifting-subordinates/preliminary_figs/dom_sp_recov.png", height = 3, width = 5)









# OLD ####
## Other Exploratory Vis ####

## filter fields separately
blu_dat <- alldat %>%
  filter(field == "EDGE_blue")
blk_dat <- alldat %>%
  filter(field == "EDGE_black")

## Categories by Tx ####
ggplot(blu_dat, aes(x = rainfall)) +
  geom_bar() +
  facet_wrap(~combo) +
  ggtitle("EDGE Blue Field")
#ggsave("shape-shifting-subordinates/preliminary_figs/blu-cats-by-trt.png", width = 4.25, height = 3)

ggplot(blk_dat, aes(x = rainfall)) +
  geom_bar() +
  facet_wrap(~combo) +
  ggtitle("EDGE Black Field")
#ggsave("shape-shifting-subordinates/preliminary_figs/blk-cats-by-trt.png", width = 4.25, height = 3)


# Sp Abund Changes ####

## Through time ####
### Black field ####
## average data across each species
blkavg <- blk_dat %>%
  group_by(year, rainfall, nickname, species) %>%
  summarise(mean_abund = mean(abundance, na.rm = T), rank = mean(mrank), ppct = mean(mpersist_pct)) %>%
  filter(!is.na(nickname))

## graph abundance through time for each category
ggplot(blkavg, aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~nickname*rainfall)
    ## hard to make comparisons from this fig with all cats in one df
    ## particularly hard to see the low abundance species


## graph categories separately
ggplot(blkavg[blkavg$nickname=="TransSub",], aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)
ggsave("shape-shifting-subordinates/preliminary_figs/blk_TS_timeseries.png", width = 6, height = 3.5)

ggplot(blkavg[blkavg$nickname=="TransDom",], aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)
ggsave("shape-shifting-subordinates/preliminary_figs/blk_TD_timeseries.png", width = 6, height = 3.5)

ggplot(blkavg[blkavg$nickname=="CoreSub",], aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)
ggsave("shape-shifting-subordinates/preliminary_figs/blk_CS_timeseries.png", width = 6, height = 3.5)

ggplot(blkavg[blkavg$nickname=="CoreDom",], aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)
ggsave("shape-shifting-subordinates/preliminary_figs/blk_CD_timeseries.png", width = 6, height = 3.5)



### Blue Field ####
bluavg <- blu_dat %>%
  group_by(year, rainfall, nickname, species) %>%
  summarise(mean_abund = mean(abundance, na.rm = T), rank = mean(mrank), ppct = mean(mpersist_pct)) %>%
  filter(!is.na(nickname))

ggplot(bluavg, aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~nickname*rainfall)

## graph categories separately
ggplot(bluavg[bluavg$nickname=="TransSub",], aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)
ggsave("shape-shifting-subordinates/preliminary_figs/blu_TS_timeseries.png", width = 6, height = 3.5)


ggplot(bluavg[bluavg$nickname=="TransDom",], aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)
ggsave("shape-shifting-subordinates/preliminary_figs/blu_TD_timeseries.png", width = 6, height = 3.5)

ggplot(bluavg[bluavg$nickname=="CoreSub",], aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)
ggsave("shape-shifting-subordinates/preliminary_figs/blu_CS_timeseries.png", width = 6, height = 3.5)

ggplot(bluavg[bluavg$nickname=="CoreDom",], aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)
ggsave("shape-shifting-subordinates/preliminary_figs/blu_CD_timeseries.png", width = 6, height = 3.5)



# Future Directions ####
## Annual vs. Perennial? ####
## Pre vs. Post Drought ####
## Abund vs. Persistence? ####
ggplot(blk_dat, aes(y=abundance, x=mrank)) +
  geom_point()
ggplot(blk_dat, aes(y=abundance, x=mpersist_pct)) +
  geom_point()
ggplot(blk_dat, aes(x=mrank, y=mpersist_pct, color = nickname)) +
  geom_point()
ggplot(blk_dat, aes(y=abundance, x=mrank, size = mpersist_pct)) +
  geom_point() +
  facet_wrap(~rainfall)

