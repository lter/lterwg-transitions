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

all_SPEI_raw<-
  readRDS(here::here("SPEI","SPEI_all_trt_yr_total.rds")) ## both these files are in the GDrive in the Project 3 folder

corre_data <- 
  read.csv(here::here("Project_3_climate_sensitivity", "corre_all.csv"))


## look at column names
names(all_SPEI_raw)
names(corre_data)

corre_data <- corre_data %>%
  rename(year = calendar_year)

comb_data <- full_join(corre_data, all_SPEI_raw, by = c("year", "site_code"))

unique(comb_data$trt_type)
names(comb_data)
comb_data1 <- comb_data %>%
  filter(month == 10) %>% ##filtering to October for the 9 month SPEI calculations
  filter(trt_type == "control" | trt_type == "N" | trt_type == "P" | trt_type == "N*P") %>%
  filter(trt_yr == "treated") %>%
  group_by(site_code, year, plot_id, treatment_year, treatment, trt_type, spei) %>%
  summarize(anpp = mean(anpp))

unique(comb_data1$site_code) ## we have six sites

comb_data1 %>%
  filter(trt_type == "N*P") %>%
  ggplot(aes(x = spei, y = anpp)) +
  geom_point() +
  facet_wrap(~site_code, scales = "free_y") +
  geom_smooth(method = "loess", se = F) +
  theme_bw() 

standardized_data <- comb_data1 %>%
  ungroup() %>%
  drop_na() %>%
  group_by(site_code, trt_type, year, spei) %>%
  summarize(z_score_anpp = mean(anpp)/sd(anpp))

ggplot(standardized_data, aes(x = spei, y = z_score_anpp)) +
  geom_point() +
  facet_grid(site_code~trt_type, scales = "free_y") +
  geom_smooth(method = "loess", se = F) +
  theme_bw() 
