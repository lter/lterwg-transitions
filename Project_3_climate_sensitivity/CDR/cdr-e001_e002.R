### 

cdr <- read.csv(here::here("Project_3_climate_sensitivity", "CDR", "cdr_clean.csv"))

cdr1 <- cdr %>%
  filter(ncess == 0) %>% ##only want N addition
  rename(n = nadd) %>%
  mutate(site_code = rep("CDR")) %>%
  group_by(year, site_code, plot, uniqueID, n) %>%
  summarize(anpp = sum(abundance)) %>%
  ungroup()
cdr1$p <- rep(20, times = nrow(cdr1))
