## making histogram again 

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

dont_include <- c("watering", "e001", "e002", "PHACE", "Yu", "IRG", "TMECE", "Nfert", "RaMPs")
'%!in%' <- function(x,y)!('%in%'(x,y)) ## from file cleaning

corre_sites <- corre_data %>%
  filter(trt_type == "control" | trt_type == "N" | trt_type == "P" | trt_type == "N*P") %>% ## only keep fert treatments
  filter(project_name %!in% dont_include) %>% ## see above
  filter(site_code != "temple.us") 
corre_site_names <- unique(corre_sites$site_code)

## sites that harvest in August
august_gs <- c("KNZ", "KBS", "CDR", "cbgb.us", "KUFS", "NWT")

data_corre <- left_join(corre_sites, all_SPEI_raw)  %>%
  mutate(month_keep = ifelse(site_code %in% august_gs, 8, 
                             ifelse(site_code == 'yarra.us', 4, 
                                    ifelse(site_code == "SERC", 10, 5)))) %>%
  filter(month == month_keep) %>%
  dplyr::select(-month_keep) %>%
  group_by(site_code, year, treatment_year, month) %>%
  summarize(spei = mean(spei)) %>%
  ungroup()

spei_100 <- all_SPEI_raw %>%
  filter(site_code %in% corre_site_names) %>%
  mutate(month_keep = ifelse(site_code %in% august_gs, 8, 
                             ifelse(site_code == 'yarra.us', 4, 
                                    ifelse(site_code == "SERC", 10, 5)))) %>%
  filter(month == month_keep) %>%
  dplyr::select(-month_keep)

ggplot() +
  geom_histogram(data = spei_100, aes(x = spei), fill = "white", color = "black") +
  geom_histogram(data = data_corre, aes(x = spei), fill = "blue", color = "black") +
  facet_wrap(~site_code, nrow = 3) +
  theme_bw()
  