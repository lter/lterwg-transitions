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
  read.csv(here::here("Project_3_climate_sensitivity", "corre_all.csv"))


## look at column names
names(all_SPEI_raw)
names(corre_data)

corre_data <- corre_data %>%
  rename(year = calendar_year)
unique(all_SPEI_raw$year)
unique(corre_data$year)
comb_data <- full_join(corre_data, all_SPEI_raw, by = c("year", "site_code"))

unique(comb_data$trt_type)
comb_data1 <- comb_data %>%
  #filter(month == 10) %>% ##filtering to October for the 9 month SPEI calculations
  filter(trt_type == "control" | trt_type == "N" | trt_type == "P" | trt_type == "N*P") %>%
  distinct()
unique(comb_data1$site_code) 


# comb_data1 %>%
#   filter(trt_type == "N") %>%
#   ggplot(aes(x = spei, y = anpp)) +
#   geom_point() +
#   facet_wrap(~site_code, scales = "free_y") +
#   geom_smooth(method = "loess", se = F) +
#   theme_bw() 
# 
# standardized_data <- comb_data1 %>%
#   ungroup() %>%
#   drop_na() %>%
#   group_by(site_code, trt_type, year, spei, treatment) %>%
#   summarize(z_score_anpp = mean(anpp)/sd(anpp))
# 
# standardized_data %>%
#   filter(trt_type == "control") %>%
#   ggplot(aes(x = spei, y = z_score_anpp, color = treatment)) +
#   geom_point() +
#   facet_grid(~site_code, scales = "free_y") +
#   geom_smooth(method = "loess", se = F) +
#   theme_bw() 

comb_data2 <- comb_data1 %>%
  unite(uniqueID, c("year", "treatment", "block", "plot_id"), remove = FALSE)

#Model selection----------------
#Candidate model set
#L=linear, Q=quadratic, C=cubic, a=additive, i=interaction

#### figure out which sites have an anpp vs. spei relationship ####

## filter out sites that don't have an N treatment
unique(comb_data2$site_code)


## assess whether each project in each site has both a control and N
control_n <- comb_data2 %>%
  group_by(site_code) %>%
  unite(experiment_id, c("project_name.x", "trt_type")) %>%
  dplyr::select(site_code, experiment_id) %>%
  distinct() %>%
  arrange(site_code)
## experiments without a control and N
dont_include <- c("watering", "e001", "e002", "PHACE", "Yu", "IRG", "TMECE", "Nfert", "RaMPs")
'%!in%' <- function(x,y)!('%in%'(x,y))

sites_with_n <- comb_data2 %>%
  filter(trt_type == "N") %>%
  filter(project_name.x %!in% dont_include) %>% ## see above
  filter(site_code != "temple.us") ## temple only has a control treatment
keep_sites <- unique(sites_with_n$site_code)
print(keep_sites) ## 9 total sites


## Yarra is the southern hemisphere, so need a different range for SPEI

## data frame for looking at effects of N and SPEI
##ugly and long way of making sure the right months' spei goes with the right sites
n_sites <- comb_data2 %>%
  filter(site_code %in% keep_sites) %>%
  filter(project_name.x %!in% dont_include)

n_sites_april <- n_sites %>%
  filter(site_code == "yarra.au") %>%
  filter(month == 4)
n_sites_october <- n_sites %>%
  filter(site_code != 'yarra.au') %>%
  filter(month == 10)
n_sites <- bind_rows(n_sites_april, n_sites_october)

##blank for seeing which sites have SPEI in the model
not_null_sites <- tibble(site_code = NA,
                         keep = NA)



for (i in unique(n_sites$site_code)) {
  dat <- n_sites[n_sites$site_code == i, ]
  
  m.null <- lme(anpp ~ year*n, data = dat, random = ~1|uniqueID, method="ML")
  m.La <- lme(anpp ~ spei + n, data = dat,random = ~1|uniqueID, method="ML")
  m.Li <- lme(anpp ~ spei*n, data = dat,random = ~1|uniqueID, method="ML")
  m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = dat, random = ~1|uniqueID, method="ML")
  m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = dat,random=~1|uniqueID,method="ML")
  m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = dat,random=~1|uniqueID,method="ML")
  m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = dat, random = ~1|uniqueID, method="ML")
  
  null_aic <- AICc(m.null)
  minimum_aic <- min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
  
  keep <- ifelse(null_aic == minimum_aic, "no", "yes") ## is null is the best model
  site_code <- i
  pdata <- as.data.frame(site_code)
  pdata$keep <- keep
  
  not_null_sites <- bind_rows(not_null_sites, pdata)
}

spei_sites <- not_null_sites %>%
  filter(keep == "yes")
print(spei_sites)
## being more precise with which treatments we're keeping does not change which sites
## have SPEI in the best fit model


########
## sites where a model with SPEI, year, and N is the best fit == CDR, SERC, KUFS, yarra.au
## for Yarra models later on --
comb_data_south <- comb_data2 %>%
  filter(month == 4) %>% 
  filter(trt_type == "control" | trt_type == "N" | trt_type == "P" | trt_type == "N*P") %>%
  distinct() %>%
  filter(project_name.x %!in% dont_include) 
comb_data_north <- comb_data2 %>%
  filter(month == 10) %>% 
  filter(trt_type == "control" | trt_type == "N" | trt_type == "P" | trt_type == "N*P") %>%
  distinct() %>%
  filter(project_name.x %!in% dont_include) 
