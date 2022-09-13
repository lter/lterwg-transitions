
## Script name: Exploring Rarity Category Patterns
##
## Purpose of script: explore patterns in categories' responses to drought
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu


# Load Data ####
source("shape-shifting-subordinates/sev_rarity_categorization.R")

# Merge Ctrl & Tx Data ####
## first combine the field averaged control data with the drought treatment data
alldat <- left_join(dat, field_catdat, by = c("field", "species")) %>%
  filter(rainfall != "D") ## remove monsoon timing treatment

# Exploratory Visualization ####
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
ggsave("shape-shifting-subordinates/preliminary_figs/blu-cats-by-trt.png", width = 4.25, height = 3)

ggplot(blk_dat, aes(x = rainfall)) +
  geom_bar() +
  facet_wrap(~combo) +
  ggtitle("EDGE Black Field")
ggsave("shape-shifting-subordinates/preliminary_figs/blk-cats-by-trt.png", width = 4.25, height = 3)

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
#ggsave("shape-shifting-subordinates/ts_bk_timeseries.png", width = 6, height = 3.5)

ggplot(blkavg[blkavg$nickname=="TransDom",], aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)
#ggsave("shape-shifting-subordinates/td_bk_timeseries.png", width = 6, height = 3.5)

ggplot(blkavg[blkavg$nickname=="CoreSub",], aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)
#ggsave("shape-shifting-subordinates/cs_bk_timeseries.png", width = 6, height = 3.5)

ggplot(blkavg[blkavg$nickname=="CoreDom",], aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)
#ggsave("shape-shifting-subordinates/cd_bk_timeseries.png", width = 6, height = 3.5)



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

ggplot(bluavg[bluavg$nickname=="TransDom",], aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)

ggplot(bluavg[bluavg$nickname=="CoreSub",], aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)

ggplot(bluavg[bluavg$nickname=="CoreDom",], aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)


## Control - Drought ####
### Black Field ####
blk <- blk_dat %>%
  group_by(rainfall, species, nickname) %>%
  summarise(mean_abund = mean(abundance, na.rm = T)) %>%
  pivot_wider(names_from = rainfall, values_from = mean_abund) %>%
  mutate(abund_diff = C - E)

ggplot(blk, aes(x=nickname, y=abund_diff)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("") + ylab("Control - Drought Biomass (g)") +
  ggtitle("Black Field") +
  coord_cartesian(ylim = c(-51,40))
ggsave("shape-shifting-subordinates/preliminary_figs/d_sensitivity_box_blk.png", width = 4, height = 3.5)

### Blue Field ####
blu <- blu_dat %>%
  group_by(rainfall, species, nickname) %>%
  summarise(mean_abund = mean(abundance, na.rm = T)) %>%
  pivot_wider(names_from = rainfall, values_from = mean_abund) %>%
  mutate(abund_diff = C - E)

ggplot(blu, aes(x=nickname, y=abund_diff)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("") + ylab("Control - Drought Biomass (g)") +
  ggtitle("Blue Field") +
  coord_cartesian(ylim = c(-51,40))
ggsave("shape-shifting-subordinates/preliminary_figs/d_sensitivity_box_blu.png", width = 4, height = 3.5)


# Future Directions ####
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

