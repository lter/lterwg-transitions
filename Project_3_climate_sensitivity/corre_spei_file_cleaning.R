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


## Yarra is the southern hemisphere, so need a different range for SPEI

## data frame for looking at effects of N and SPEI
##ugly and long way of making sure the right months' spei goes with the right sites
n_sites <- comb_data2 %>%
  filter(site_code %in% keep_sites) %>%
  filter(project_name %!in% dont_include)

n_sites_april <- n_sites %>%
  filter(site_code == "yarra.au") %>%
  filter(month == 4)
n_sites_october <- n_sites %>%
  filter(site_code != 'yarra.au') %>%
  filter(month == 10)
n_sites <- bind_rows(n_sites_april, n_sites_october)

## done -- file needed for site-level analyses is n_sites