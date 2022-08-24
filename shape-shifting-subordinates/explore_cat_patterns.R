
# Load Data ####
source("shape-shifting-subordinates/sev_rarity_categorization.R")

# Combine Ctrl w/Tx ####
## first combine the field averaged control data with the drought treatment data
alldat <- left_join(dat, field_catdat, by = c("field", "species")) %>%
  filter(rainfall != "D") ## remove monsoon timing treatment

# Exploratory Visualization ####
blu_dat <- alldat %>%
  filter(field == "EDGE_blue")
blk_dat <- alldat %>%
  filter(field == "EDGE_black")

## Categories by Tx ####
ggplot(blu_dat, aes(x = rainfall)) +
  geom_bar() +
  facet_wrap(~combo) +
  ggtitle("EDGE Blue Field")
ggsave("shape-shifting-subordinates/blu-cats-by-trt.png", width = 4.25, height = 3)

ggplot(blk_dat, aes(x = rainfall)) +
  geom_bar() +
  facet_wrap(~combo) +
  ggtitle("EDGE Black Field")
ggsave("shape-shifting-subordinates/blk-cats-by-trt.png", width = 4.25, height = 3)

## Sp Abund Changes ####
## then look at how species abundances in each category change?

### Through time ####
blkavg <- blk_dat %>%
  group_by(year, rainfall, nickname, species) %>%
  summarise(mean_abund = mean(abundance, na.rm = T), rank = mean(mrank), ppct = mean(mpersist_pct))

bk_ts <- blkavg %>%
  filter(nickname == "TransSub")
bk_td <- blkavg %>%
  filter(nickname == "TransDom")
bk_cs <- blkavg %>%
  filter(nickname == "CoreSub")
bk_cd <- blkavg %>%
  filter(nickname == "CoreDom")


ggplot(blkavg, aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~nickname*rainfall)

ggplot(bk_ts, aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)
ggsave("shape-shifting-subordinates/ts_bk_timeseries.png", width = 6, height = 3.5)

ggplot(bk_td, aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)
ggsave("shape-shifting-subordinates/td_bk_timeseries.png", width = 6, height = 3.5)

ggplot(bk_cs, aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)
ggsave("shape-shifting-subordinates/cs_bk_timeseries.png", width = 6, height = 3.5)

ggplot(bk_cd, aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~rainfall)
ggsave("shape-shifting-subordinates/cd_bk_timeseries.png", width = 6, height = 3.5)




bluavg <- blu_dat %>%
  group_by(year, rainfall, nickname, species) %>%
  summarise(mean_abund = mean(abundance, na.rm = T), rank = mean(mrank), ppct = mean(mpersist_pct))


ggplot(bluavg, aes(x=year, y=mean_abund, color = species)) +
  geom_line() +
  facet_wrap(~nickname*rainfall)

### Control - Drought ####
#### Black Field ####
blk <- blk_dat %>%
  group_by(rainfall, species, nickname) %>%
  summarise(mean_abund = mean(abundance, na.rm = T)) %>%
  pivot_wider(names_from = rainfall, values_from = mean_abund) %>%
  mutate(abund_diff = C - E)

ggplot(blk, aes(x=nickname, y=abund_diff)) +
  geom_point(size = 3, pch = 21) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("") + ylab("Control - Drought Biomass (g)") +
  ggtitle("Black Field")

ggsave("shape-shifting-subordinates/d_sensitivity_blk.png", width = 4, height = 3.5)




#### Blue Field ####
blu <- blu_dat %>%
  group_by(rainfall, species, nickname) %>%
  summarise(mean_abund = mean(abundance, na.rm = T)) %>%
  pivot_wider(names_from = rainfall, values_from = mean_abund) %>%
  mutate(abund_diff = C - E)

ggplot(blu, aes(x=nickname, y=abund_diff)) +
  geom_point(size = 3, pch = 21) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("") + ylab("Control - Drought Biomass (g)") +
  ggtitle("Blue Field")

ggsave("shape-shifting-subordinates/d_sensitivity_blu.png", width = 4, height = 3.5)


ggplot(blk, aes(x=nickname, y=abund_diff)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("") + ylab("Control - Drought Biomass (g)") +
  ggtitle("Black Field") +
  coord_cartesian(ylim = c(-51,40))
ggsave("shape-shifting-subordinates/d_sensitivity_box_blk.png", width = 4, height = 3.5)
ggplot(blu, aes(x=nickname, y=abund_diff)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("") + ylab("Control - Drought Biomass (g)") +
  ggtitle("Blue Field") +
  coord_cartesian(ylim = c(-51,40))
ggsave("shape-shifting-subordinates/d_sensitivity_box_blu.png", width = 4, height = 3.5)


## Abund vs. Persistence ####
ggplot(blk_dat, aes(y=abundance, x=mrank)) +
  geom_point()
ggplot(blk_dat, aes(y=abundance, x=mpersist_pct)) +
  geom_point()
ggplot(blk_dat, aes(x=mrank, y=mpersist_pct, color = nickname)) +
  geom_point()
ggplot(blk_dat, aes(y=abundance, x=mrank, size = mpersist_pct)) +
  geom_point() +
  facet_wrap(~rainfall)


## showing some sort of difference from control would be ideal.
## also look at species level variance?
