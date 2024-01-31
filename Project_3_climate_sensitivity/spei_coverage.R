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
