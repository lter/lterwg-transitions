### SEV Nfert Data Cleaning to Corre ###

## Data manipulation packages
library(tidyverse)

source("utilities/data_import.R")

## Data import
sev_nfert <- read_csv_gdrive("1nMrJK_K3AJwqVbCDMEjRf1bo_W2MqiQ_")%>%
  as_tibble()

## Working data frame
sevnfert <- sev_nfert

unique(sevnfert$site)
# only 1 site

unique(sevnfert$treatment)
# 2 unique trts

unique(sevnfert$year)

#Twice a year sampling
unique(sevnfert$season)

#Subset only fall sampling
sevnfert_fall <- subset(sevnfert, season=="fall")

## Calculate anpp by summing biomass across 4 quads in each plot and divide by 4m^2 to get g/m^2 estimate per plot
## Use biomass.BM (best model)
sev_nfert_anpp <- sevnfert_fall %>%
  select(year, plot, treatment, biomass.BM) %>%
  group_by(plot, year, treatment) %>%
  summarise(anpp = sum(biomass.BM)/4)

# Re-do treatment variables
trt.dat<-data.frame(treatment = c("C", "F"),
                    trt_type = c("control", "N"))
sev_nfert_anpp <- left_join(sev_nfert_anpp, trt.dat, by = "treatment") 

# Make N/P variables
## 100kg/ha is 10g/m2 
np.dat <- data.frame(treatment = c("C", "F"),
                     n = c(0, 10),
                     p = c(0, 0))
sev_nfert_anpp <- left_join(sev_nfert_anpp, np.dat, by = "treatment") 


## Reformat to CoRRE
sev_nfert_corre <- sev_nfert_anpp %>%
  ## format column names to match the rest of the datasets
  dplyr::mutate(site_code = "sev.nfert",
                project_name = "Nfert",
                treatment_year = year-2003,
                calendar_year = year,
                treatment = treatment,
                plot_id = plot, 
                anpp = anpp, 
                community_type = "0",
                block = "0",
                public = "1",
                trt_type = trt_type,
                n = n,
                p = p, 
                k = "0",
                precip = "0",
                successional = "0",
                pulse = "0",
                plant_mani = "0") 
sev_nfert_corre <- sev_nfert_corre[, -c(1:3)]

write.csv(sev_nfert_corre, "Project_3_climate_sensitivity/SEV_data/sev_nfert_corre.csv")
