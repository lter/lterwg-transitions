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
dont_include <- c("watering", "e001", "e002", "PHACE", "Yu", "IRG", "TMECE", "Nfert")
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
  filter(site_code %in% keep_sites)

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
  distinct()
comb_data_north <- comb_data2 %>%
  filter(month == 10) %>% 
  filter(trt_type == "control" | trt_type == "N" | trt_type == "P" | trt_type == "N*P") %>%
  distinct()

#### Yarra, Australia ####

yarra <- filter(comb_data_south, site_code == "yarra.au")

m.null <- lme(anpp ~ year*n, data = yarra, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = yarra,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = yarra,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = yarra, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = yarra,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = yarra,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = yarra, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.Ca, but within 1 of m.Ci
## additive models are better fits than interactive models, generally

# AR1 - autocorrelation 1, AR1 - autocorrelation 2 to best model from above
#This is only necessary if expyear includes non-integer numbers
# int.year <- exp.clim$expyear*2-1 #convert to integer
# exp.clim$expyear <- int.year
#Fit temporal autocorrelation models
m.AR1 <- update(m.Ca,correlation = corAR1(form= ~treatment_year))
m.AR2 <- update(m.Ca,correlation = corARMA(form= ~treatment_year,p=2))
# model selection
AICc(m.Ca, m.AR1, m.AR2)
# best model m.Ca
rsquared(m.Ca)
#Marginal R2:  the proportion of variance explained by the fixed factor(s) alone == 0.39
#Conditional R2: he proportion of variance explained by both the fixed and random factors == 0.92

#Evaluate model assumptions
plot(m.Ca) #pretty linear but with a wide spread
qqPlot(residuals(m.Ca)) ## some deviations at the high end of the range
hist(residuals(m.Ca)) 

#Do sketchy frequentist tests on best model
anova(m.Ca,type = "marginal") #F test
Anova(m.Ca,type = 2)# Chisq test. Differnet values for p-values, but same significance

#Param estimates and post-hoc pairwise comparisons
emtrends(m.Ca,~ n | degree, "spei", max.degree = 3) ## error message
pairs(emtrends(m.Ca,~ n | degree , "spei", max.degree = 3)) ## getting NaNs
#The quadratic slope is the most different

#Visualize CSF results---
# get a plot of estimated values from the model, by each depth
# visreg with ggplot graphics
# visreg(fit = m.Qa, xvar = "spei", type = "conditional", by = "n", data = yarra, gg = TRUE, partial = F, rug = F) + 
#   geom_point(aes(x = spei, y = anpp, col = treatment_year), alpha = 0.2, data = yarra) +
#   theme_bw() +
#   labs(x="SPEI",
#        y="ANPP")

#Alternative visualization code if the above doesn't work
yarra$predicted <- predict(m.Ca, yarra)
yarra_plot <- ggplot(yarra, aes(x=spei, y = predicted, color = treatment)) +
 # facet_wrap(~treatment)+
  geom_point(aes(x = spei, y = anpp),  size = 0.5) +
  geom_smooth(aes(y = predicted, fill = treatment), se = F) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP")

####  CEDAR CREEK, USA ####
cdr <- comb_data_north %>%
  filter(site_code == "CDR") %>%
  group_by(site_code, uniqueID, block, plot_id, project_name.x, treatment, treatment_year, trt_type, year, n, p, k, spei) %>%
  summarize(anpp = mean(anpp))
##currently, we only have actual N addition data from Cedar Creek in NutNet and BioCON; the 
## e001 and e002 data are just control

m.null <- lme(anpp ~ year*n, data = cdr, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = cdr,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = cdr,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = cdr, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = cdr,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = cdr,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = cdr, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.Ci

# AR1 - autocorrelation 1, AR1 - autocorrelation 2 to best model from above
#This is only necessary if expyear includes non-integer numbers
# int.year <- exp.clim$expyear*2-1 #convert to integer
# exp.clim$expyear <- int.year
#Fit temporal autocorrelation models
m.AR1 <- update(m.Ci,correlation = corAR1(form= ~treatment_year))
m.AR2 <- update(m.Ci,correlation = corARMA(form= ~treatment_year,p=2))
# model selection
AICc(m.Ci, m.AR1, m.AR2)
# best model m.Ci
rsquared(m.Ci)
#Marginal R2:  the proportion of variance explained by the fixed factor(s) alone == 0.02
#Conditional R2: he proportion of variance explained by both the fixed and random factors == 0.88

#Evaluate model assumptions
plot(m.Ci) #pretty linear 
qqPlot(residuals(m.Ci)) ## concave up
hist(residuals(m.Ci)) ## slightly right-skewed

#Do sketchy frequentist tests on best model
anova(m.Ci,type = "marginal") #F test -- cubic SPEI, N, interaction
Anova(m.Ci,type = 2)# Chisq test. Differnet values for p-values, but same significance

#Param estimates and post-hoc pairwise comparisons
emtrends(m.Ci,~ n | degree, "spei", max.degree = 3) ## error message
pairs(emtrends(m.Ci,~ n | degree , "spei", max.degree = 3)) ## error message
#The quadratic slope is the most different

#Visualize CSF results---
# get a plot of estimated values from the model, by each depth
# visreg with ggplot graphics
# visreg(fit = m.Ci, xvar = "spei", type = "conditional", by = "n", data = cdr, gg = TRUE, partial = F, rug = F) + 
#   geom_point(aes(x = spei, y = anpp, col = treatment_year), alpha = 0.2, data = cdr) +
#   theme_bw() +
#   labs(x="SPEI",
#        y="ANPP")

#Alternative visualization code if the above doesn't work
cdr$predicted <- predict(m.Ci, cdr)
cdr_plot <- ggplot(cdr, aes(x=spei, y = predicted)) +
  facet_wrap(~treatment, scales = "free_y")+
  geom_point(aes(x = spei, y = anpp), color="gray60", size = 0.5) +
  geom_smooth(aes(y = predicted), color = "gray20") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP")


#### Kansas univ. field station ####
kufs <- filter(comb_data_north, site_code == "KUFS")

kufs %>%
  unite(experiment_id, c("project_name.x", "trt_type")) %>%
  dplyr::select(experiment_id) %>%
  distinct()
## projects = E2 and E6
## E2 is control and N, E6 is control, N, P, NP

m.null <- lme(anpp ~ year*n, data = kufs, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = kufs,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = kufs,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = kufs, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = kufs,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = kufs,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = kufs, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.Ca, but within 1 of m.Ci
## additive models are better fits than interactive models, generally

# AR1 - autocorrelation 1, AR1 - autocorrelation 2 to best model from above
#This is only necessary if expyear includes non-integer numbers
# int.year <- exp.clim$expyear*2-1 #convert to integer
# exp.clim$expyear <- int.year
#Fit temporal autocorrelation models
m.AR1 <- update(m.Ca,correlation = corAR1(form= ~treatment_year))
m.AR2 <- update(m.Ca,correlation = corARMA(form= ~treatment_year,p=2))
# model selection
AICc(m.Ca, m.AR1, m.AR2)
# best model m.Ca
rsquared(m.Ca)
#Marginal R2:  the proportion of variance explained by the fixed factor(s) alone == 0.25
#Conditional R2: he proportion of variance explained by both the fixed and random factors == 0.91

#Evaluate model assumptions
plot(m.Ca) #clumpy but linear but with a wide spread
qqPlot(residuals(m.Ca)) ## some deviations at the high end of the range
hist(residuals(m.Ca)) ## slightly right-skewed

#Do sketchy frequentist tests on best model
anova(m.Ca,type = "marginal") #F test -- spei for 2 and 3 sig, n
Anova(m.Ca,type = 2)# Chisq test. Differnet values for p-values, but same significance

#Param estimates and post-hoc pairwise comparisons
emtrends(m.Ca,~ n | degree, "spei", max.degree = 3) 
pairs(emtrends(m.Ca,~ n | degree , "spei", max.degree = 3)) ## getting NaNs
#The quadratic slope is the most different

#Visualize CSF results---
# get a plot of estimated values from the model, by each depth
# visreg with ggplot graphics
# visreg(fit = m.Ca, xvar = "spei", type = "conditional", by = "n", data = kufs, gg = TRUE, partial = F, rug = F) + 
#   geom_point(aes(x = spei, y = anpp, col = treatment_year), alpha = 0.2, data = kufs) +
#   theme_bw() +
#   labs(x="SPEI",
#        y="ANPP")

#Alternative visualization code if the above doesn't work
kufs$predicted <- predict(m.Ca, kufs)
kufs_plot <- ggplot(kufs, aes(x=spei, y = predicted)) +
  facet_wrap(~treatment, scales = "free_y")+
  geom_point(aes(x = spei, y = anpp), color="gray60", size = 0.5) +
  geom_smooth(aes(y = predicted), color = "gray20") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP")

#### SERC ####
serc <- comb_data_north %>%
  filter(site_code == "SERC") %>%
  group_by(site_code, uniqueID, block, plot_id, project_name.x, treatment, treatment_year, trt_type, year, n, p, k, spei) %>%
  summarize(anpp = mean(anpp)) %>%
  ungroup() 

serc %>%
  unite(experiment_id, c("project_name.x", "trt_type")) %>%
  dplyr::select(experiment_id) %>%
  distinct()

## two experiments = TMCE and CXN
## CXN has control and N, TMCE only has control


m.null <- lme(anpp ~ year*n, data = serc, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = serc,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = serc,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = serc, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = serc,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = serc,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = serc, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.Ci

# AR1 - autocorrelation 1, AR1 - autocorrelation 2 to best model from above
#This is only necessary if expyear includes non-integer numbers
# int.year <- exp.clim$expyear*2-1 #convert to integer
# exp.clim$expyear <- int.year
#Fit temporal autocorrelation models
m.AR1 <- update(m.Ci,correlation = corAR1(form= ~treatment_year))
m.AR2 <- update(m.Ci,correlation = corARMA(form= ~treatment_year,p=2))
# model selection
AICc(m.Ci, m.AR1, m.AR2)
# best model m.Ci
rsquared(m.Ci)
#Marginal R2:  the proportion of variance explained by the fixed factor(s) alone == 0.17
#Conditional R2: he proportion of variance explained by both the fixed and random factors == 0.90

#Evaluate model assumptions
plot(m.Ci) #clumpy but linear but with a wide spread
qqPlot(residuals(m.Ci)) ## some deviations at the high end of the range
hist(residuals(m.Ci)) ## pretty symmetrical

#Do sketchy frequentist tests on best model
anova(m.Ci,type = "marginal") #F test -- sig everything except spei^2
Anova(m.Ci,type = 2)# Chisq test. Differnet values for p-values, but same signifiCince

#Param estimates and post-hoc pairwise comparisons
emtrends(m.Ci,~ n | degree, "spei", max.degree = 3) 
pairs(emtrends(m.Ci,~ n | degree , "spei", max.degree = 3))
#The quadratic slope is the most different

#Visualize CSF results---
# get a plot of estimated values from the model, by each depth
# visreg with ggplot graphics
# visreg(fit = m.Ci, xvar = "spei", type = "conditional", by = "n", data = serc, gg = TRUE, partial = F, rug = F) + 
#   geom_point(aes(x = spei, y = anpp, col = treatment_year), alpha = 0.2, data = serc) +
#   theme_bw() +
#   labs(x="SPEI",
#        y="ANPP")

#Alternative visualization code if the above doesn't work
serc$predicted <- predict(m.Ci, serc)
serc_plot <- ggplot(serc, aes(x=spei, y = predicted)) +
  facet_wrap(~treatment)+
  geom_point(aes(x = spei, y = anpp), color="gray60", size = 0.5) +
  geom_smooth(aes(y = predicted), color = "gray20") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP")


#### All plots ####
yarra_plot
cdr_plot
kufs_plot
serc_plot

## looking at all sites even if null is the best model
konza <- filter(comb_data_south, site_code == "KNZ")
m.null <- lme(anpp ~ year*n, data = konza, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = konza,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = konza,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = konza, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = konza,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = konza,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = konza, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.Ca, but within 1 of m.Ci
## additive models are better fits than interactive models, generally

# AR1 - autocorrelation 1, AR1 - autocorrelation 2 to best model from above
#This is only necessary if expyear includes non-integer numbers
# int.year <- exp.clim$expyear*2-1 #convert to integer
# exp.clim$expyear <- int.year
#Fit temporal autocorrelation models
m.AR1 <- update(m.Ca,correlation = corAR1(form= ~treatment_year))
m.AR2 <- update(m.Ca,correlation = corARMA(form= ~treatment_year,p=2))
# model selection
AICc(m.Ca, m.AR1, m.AR2)
# best model m.Ca
rsquared(m.Ca)
#Marginal R2:  the proportion of variance explained by the fixed factor(s) alone == 0.39
#Conditional R2: he proportion of variance explained by both the fixed and random factors == 0.92

#Evaluate model assumptions
plot(m.Ca) #pretty linear but with a wide spread
qqPlot(residuals(m.Ca)) ## some deviations at the high end of the range
hist(residuals(m.Ca)) 

#Do sketchy frequentist tests on best model
anova(m.Ca,type = "marginal") #F test
Anova(m.Ca,type = 2)# Chisq test. Differnet values for p-values, but same significance

#Param estimates and post-hoc pairwise comparisons
emtrends(m.Ca,~ n | degree, "spei", max.degree = 3) ## error message
pairs(emtrends(m.Ca,~ n | degree , "spei", max.degree = 3)) ## getting NaNs
#The quadratic slope is the most different

#Visualize CSF results---
# get a plot of estimated values from the model, by each depth
# visreg with ggplot graphics
# visreg(fit = m.Qa, xvar = "spei", type = "conditional", by = "n", data = yarra, gg = TRUE, partial = F, rug = F) + 
#   geom_point(aes(x = spei, y = anpp, col = treatment_year), alpha = 0.2, data = yarra) +
#   theme_bw() +
#   labs(x="SPEI",
#        y="ANPP")

#Alternative visualization code if the above doesn't work
konza$predicted <- predict(m.null, konza)
konza_plot <- ggplot(konza, aes(x=spei, y = predicted)) +
  facet_wrap(~treatment, scales = "free_y")+
  geom_point(aes(x = spei, y = anpp), color="gray60", size = 0.5) +
  geom_smooth(aes(y = predicted), color = "gray20") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP")

