rm(list = ls())
#install.packages("tidync", type = "binary")
library(RNetCDF)
#library(ncdf4)
library(tidync)
library(dplyr)
library(ggplot2)
library(here)
library(ggh4x)
library(tidyverse)
library(nlme)
library(vegan)
library(emmeans)
library(MuMIn)
library(visreg)
library(car)
library(piecewiseSEM)


all_SPEI_raw<-
  readRDS(here::here("SPEI","SPEI_all_trt_yr_total.rds")) ## both these files are in the GDrive in the Project 3 folder

corre_data <- 
  read.csv(here::here("SPEI", "corre_all.csv"))

unique(corre_data$site_code)
## look at column names
names(all_SPEI_raw)
names(corre_data)

corre_data <- corre_data %>%
  rename(year = calendar_year) ## change to make joining easier
unique(all_SPEI_raw$year) ## SPEI years -- 1901 - 2021
unique(corre_data$year) ## CoRRE years -- 1986 - 2022

names(all_SPEI_raw)
all_SPEI_raw <- all_SPEI_raw %>%
  dplyr::select(site_code, year, spei, lon, lat, month)
  

## full join with CoRRE data
comb_data <- full_join(corre_data, all_SPEI_raw, by = c("year", "site_code"))

unique(comb_data$trt_type)
comb_data1 <- comb_data %>%
  filter(trt_type == "control" | trt_type == "N" | trt_type == "P" | trt_type == "N*P") %>% ## only keep fert treatments
  distinct()
unique(comb_data1$site_code) ## 14 total sites



comb_data2 <- comb_data1 %>%
  unite(uniqueID, c("year", "treatment", "block", "plot_id"), remove = FALSE) ## make a unique ID column for random effects


## assess whether each project in each site has (at least) both a control and N
control_n <- comb_data2 %>%
  group_by(site_code) %>%
  unite(experiment_id, c("project_name", "trt_type")) %>%
  dplyr::select(site_code, experiment_id) %>%
  distinct() %>%
  arrange(site_code)
## experiments without a control and N
dont_include <- c("watering", "e001", "e002", "PHACE", "Yu", "IRG", "TMECE", "Nfert", "RaMPs")
'%!in%' <- function(x,y)!('%in%'(x,y))

sites_with_n <- comb_data2 %>%
  filter(trt_type == "N") %>%
  filter(project_name %!in% dont_include) %>% ## see above
  filter(site_code != "temple.us") ## temple only has a control treatment
keep_sites <- unique(sites_with_n$site_code)
print(keep_sites) ## 9 total sites




## data frame for looking at effects of N and SPEI
##ugly and long way of making sure the right months' spei goes with the right sites
n_sites <- comb_data2 %>%
  filter(site_code %in% keep_sites) %>%
  filter(project_name %!in% dont_include)

## setting the end of the SPEI window for the month when harvest occurs
## see "Site Metadata" in the Google Drive folder for more info
## sites that harvest in August
august_gs <- c("KNZ", "KBS", "CDR", "cbgb.us", "KUFS", "NWT")
z_score <- function(x) (x - mean(x))/sd(x)
## Yarra (Australia) is April, Sierra Foothills is May
n_sites <- n_sites %>%
  mutate(month_keep = ifelse(site_code %in% august_gs, 8, 
                                    ifelse(site_code == 'yarra.us', 4,
                                           ifelse(site_code == "SERC", 10, 5)))) %>%
  filter(month == month_keep) %>%
  dplyr::select(-month_keep) %>%
  group_by(site_code, project_name) %>%
  mutate(anpp_standardized = z_score(anpp)) %>%
  ungroup()
## done -- file needed for site-level analyses is n_sites

## process CoRRE MAP and MAT data
clim <- read.csv(here::here("SPEI", "corre_map_mat.csv"))
sites <- unique(n_sites$site_code)
names(clim)

clim1 <- clim %>%
  filter(site_code %in% sites) %>%
  dplyr::select(site_code, project_name, treatment_year, calendar_year, MAT, MAP) %>%
  rename(year = calendar_year) %>%
  group_by(site_code, treatment_year, year) %>%
  summarize(mean_map = mean(MAP),
            mean_mat = mean(MAT))


