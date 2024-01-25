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
  distinct() %>%
  filter(project_name.x %!in% dont_include) 
comb_data_north <- comb_data2 %>%
  filter(month == 10) %>% 
  filter(trt_type == "control" | trt_type == "N" | trt_type == "P" | trt_type == "N*P") %>%
  distinct() %>%
  filter(project_name.x %!in% dont_include) 

#### Yarra, Australia ####

yarra <- filter(comb_data_south, site_code == "yarra.au")

m.null <- lme(anpp ~ year*n, data = yarra, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = yarra,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = yarra,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = yarra, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = yarra,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = yarra,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = yarra, random = ~1|uniqueID, method="ML")

summary(m.Ca)

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

# Visualize CSF results---
# get a plot of estimated values from the model, by each depth
# visreg with ggplot graphics
visreg(fit, xvar = "spei", type = "conditional", by = "treatment", data = yarra, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) +
  geom_point(aes(x = spei, y = anpp), alpha = 0.2, data = yarra) +
  theme_bw() +
  labs(x="SPEI",
       y="ANPP")

visreg(fit = m.Ca, "spei", type = "contrast")
fit <- lm(anpp ~ spei+treatment + I(spei^2) + I(spei^3),data = yarra)

visreg(fit, "spei")
AICc(fit)
AICc(m.Ca)
visreg(alt_m.Ca, "spei")
#Alternative visualization code if the above doesn't work
yarra$predicted <- predict(m.Ca, yarra)
#yarra_plot <- 
ggplot(yarra, aes(x=spei, y = predicted, color = treatment)) +
 # facet_wrap(~treatment)+
  geom_point(aes(x = spei, y = anpp),  size = 0.5) +
  geom_smooth(aes(y = predicted, fill = treatment), se = F) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP") +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_vline(xintercept = 0, linetype = 3)

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
lm.Ci <- lm(anpp ~ spei*treatment + I(spei^2)*treatment + I(spei^3)*treatment, data = cdr) ## convert model to lm so that visreg will run
visreg(lm.Ci, xvar = "spei", type = "conditional", by = "treatment", data = cdr, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) +
  geom_point(aes(x = spei, y = anpp), alpha = 0.2, data = yarra) +
  theme_bw() +
  labs(x="SPEI",
       y="ANPP")

#Alternative visualization code if the above doesn't work
cdr$predicted <- predict(m.Ci, cdr)
#cdr_plot <- 
cdr %>%
  filter(treatment == "Camb_Namb" | treatment == "Camb_Nenrich") %>%
 # filter(treatment == "Control" | treatment == "N" | treatment == "P" | treatment == "NP") %>%
  ggplot(aes(x=spei, y = predicted, color = treatment)) +
  #facet_wrap(~treatment, scales = "free_y")+
  geom_point(aes(x = spei, y = anpp), color="gray60", size = 0.5) +
  geom_smooth(aes(y = predicted), se = F) +
  scale_color_manual(labels = c("Control", "N"), values = c("#F8766D", "#7CAE00")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP") +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_vline(xintercept = 0, linetype = 3)


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
lm.Ca <- lm(anpp ~ spei*n + I(spei^2) + I(spei^3), data = kufs)
visreg(lm.Ca, xvar = "spei", type = "conditional", by = "n", data = kufs, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) +
  geom_point(aes(x = spei, y = anpp), alpha = 0.2, data = yarra) +
  theme_bw() +
  labs(x="SPEI",
       y="ANPP")

#Alternative visualization code if the above doesn't work
kufs$predicted <- predict(m.Ca, kufs)

#kufs_plot <- 
## experiment 2
kufs %>%
  filter(project_name.x == "E2") %>%
  ggplot(aes(x=spei, y = predicted, color = treatment)) +
  facet_wrap(~treatment, scales = "free_y")+
  geom_point(aes(x = spei, y = anpp), color="gray60", size = 0.5) +
  geom_smooth(aes(y = predicted), se = T) +
  theme_bw() +
  scale_color_manual(labels = c("Control", "N"), values = c("#F8766D", "#7CAE00")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP") +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_vline(xintercept = 0, linetype = 3)

e6 <- filter(kufs, project_name.x == "E6")
## make new factors so that we can facet by them 
e6$n_levels <- factor(e6$n, levels = c(0,4,8,16))
e6$p_levels <- factor(e6$p, levels = c(0,8))

ggplot(e6, aes(x=spei, y = predicted)) +
  facet_grid(p_levels ~ n_levels)+
  geom_point(aes(x = spei, y = anpp), color="gray60", size = 0.5) +
  geom_smooth(aes(y = predicted), se = T, color = "black") +
  theme_bw() +
 # scale_color_manual(labels = c("Control", "N"), values = c("#F8766D", "#7CAE00")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP") +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_vline(xintercept = 0, linetype = 3)

#### SERC ####
serc <- comb_data_north %>%
  filter(site_code == "SERC") %>%
  group_by(site_code, uniqueID, block, plot_id, project_name.x, treatment, treatment_year, trt_type, year, n, p, k, spei) %>%
  summarize(anpp = mean(anpp)) %>%
  ungroup() 

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
#Best model is m.Ca

# AR1 - autocorrelation 1, AR1 - autocorrelation 2 to best model from above
#This is only necessary if expyear includes non-integer numbers
# int.year <- exp.clim$expyear*2-1 #convert to integer
# exp.clim$expyear <- int.year
#Fit temporal autocorrelation models
m.AR1 <- update(m.Ca,correlation = corAR1(form= ~treatment_year))
m.AR2 <- update(m.Ca,correlation = corARMA(form= ~treatment_year,p=2))
# model selection
AICc(m.Ca, m.AR1, m.AR2)
# best model m.Ci
rsquared(m.Ca)
#Marginal R2:  the proportion of variance explained by the fixed factor(s) alone == 0.17
#Conditional R2: he proportion of variance explained by both the fixed and random factors == 0.90

#Evaluate model assumptions
plot(m.Ca) #clumpy but linear but with a wide spread
qqPlot(residuals(m.Ca)) ## some deviations at the high end of the range
hist(residuals(m.Ca)) ## pretty symmetrical

#Do sketchy frequentist tests on best model
anova(m.Ca,type = "marginal") #F test -- sig everything except spei^2
Anova(m.Ca,type = 2)# Chisq test. Differnet values for p-values, but same signifiCince

#Param estimates and post-hoc pairwise comparisons
emtrends(m.Ca,~ n | degree, "spei", max.degree = 3) 
pairs(emtrends(m.Ca,~ n | degree , "spei", max.degree = 3))
#The quadratic slope is the most different

#Visualize CSF results---
# get a plot of estimated values from the model, by each depth
# visreg with ggplot graphics
lm.Ca <- lm(anpp ~ spei*treatment + I(spei^2) + I(spei^3), data = serc)
visreg(lm.Ca, xvar = "spei", type = "conditional", by = "treatment", data = serc, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) +
  geom_point(aes(x = spei, y = anpp), alpha = 0.2, data = yarra) +
  theme_bw() +
  labs(x="SPEI",
       y="ANPP") +
  scale_color_manual(labels = c("Control", "N"), values = c("#F8766D", "#7CAE00")) 

#Alternative visualization code if the above doesn't work
serc$predicted <- predict(m.Ci, serc)
serc_plot <- ggplot(serc, aes(x=spei, y = predicted, color = trt_type)) +
 # facet_wrap(~treatment)+
  geom_point(aes(x = spei, y = anpp), color="gray60", size = 0.5) +
  geom_smooth(aes(y = predicted), se = T) +
  scale_color_manual( values = c("#F8766D", "#7CAE00")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP")


#### All plots ####
yarra_plot
cdr_plot
kufs_plot
serc_plot

print(keep_sites)
## looking at all sites even if null is the best model
### KONZA, USA ####
konza <- filter(comb_data_north, site_code == "KNZ")
konza %>%
  unite(experiment_id, c("project_name.x", "trt_type")) %>%
  dplyr::select(experiment_id) %>%
  distinct()

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

konza$predicted <- predict(m.null, konza)
unique(konza$n)
unique(konza$p)
konza$n_levels <- factor(konza$n, levels = c(0,10))
konza$p_levels <- factor(konza$p, levels = c(0.0, 1.0, 2.5, 5.0, 10.0))
#konza_plot <- 
ggplot(konza, aes(x=spei, y = predicted, color = project_name.x)) +
  facet_grid(p_levels~n_levels, scales = "free")+
  geom_point(aes(x = spei, y = anpp), color="gray60", size = 0.5) +
  geom_smooth(aes(y = predicted)) +
  theme_bw() +
#  scale_y_continuous(limits = c(0,2000)) + ##some outliers that make trends hard to see
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP")


print(keep_sites)
#### SIER.US ####
sierra <- filter(comb_data_north, site_code == "sier.us")
sierra %>%
  unite(experiment_id, c("project_name.x", "trt_type")) %>%
  dplyr::select(experiment_id) %>%
  distinct()
### this is just a NutNet site -- straightforward

m.null <- lme(anpp ~ year*n, data = sierra, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = sierra,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = sierra,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = sierra, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = sierra,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = sierra,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = sierra, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])

sierra$predicted <- predict(m.null, sierra)


ggplot(sierra, aes(x=spei, y = predicted, color = treatment)) +
  geom_point(aes(x = spei, y = anpp), color="gray60", size = 0.5) +
  geom_smooth(aes(y = predicted), se = F) +
  theme_bw() +
  #  scale_y_continuous(limits = c(0,2000)) + ##some outliers that make trends hard to see
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP")

print(keep_sites)
#### Chichaqua Bottoms, Iowa, USA ####
cbgb <- filter(comb_data_north, site_code == "cbgb.us")
cbgb %>%
  unite(experiment_id, c("project_name.x", "trt_type")) %>%
  dplyr::select(experiment_id) %>%
  distinct()
### this is just a NutNet site -- straightforward

m.null <- lme(anpp ~ year*n, data = cbgb, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = cbgb,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = cbgb,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = cbgb, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = cbgb,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = cbgb,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = cbgb, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])

cbgb$predicted <- predict(m.null, cbgb)

ggplot(cbgb, aes(x=spei, y = predicted, color = treatment)) +
  geom_point(aes(x = spei, y = anpp), color="gray60", size = 0.5) +
  geom_smooth(aes(y = predicted), se = F) +
  theme_bw() +
  #  scale_y_continuous(limits = c(0,2000)) + ##some outliers that make trends hard to see
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP")


print(keep_sites)
#### Niwot Ridge, USA ####
niwot <- filter(comb_data_north, site_code == "NWT")
niwot %>%
  unite(experiment_id, c("project_name.x", "trt_type")) %>%
  dplyr::select(experiment_id) %>%
  distinct()

m.null <- lme(anpp ~ year*n, data = niwot, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = niwot,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = niwot,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = niwot, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = niwot,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = niwot,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = niwot, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])

niwot$predicted <- predict(m.null, niwot)

ggplot(niwot, aes(x=spei, y = predicted, color = trt_type)) +
  geom_point(aes(x = spei, y = anpp), color="gray60", size = 0.5) +
  geom_smooth(aes(y = predicted), se = T) +
  theme_bw() +
  #  scale_y_continuous(limits = c(0,2000)) + ##some outliers that make trends hard to see
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP")

print(keep_sites)
#### Kellogg Biological Station, USA ####
kbs <- filter(comb_data_north, site_code == "KBS")
kbs %>%
  unite(experiment_id, c("project_name.x", "trt_type")) %>%
  dplyr::select(experiment_id) %>%
  distinct()

m.null <- lme(anpp ~ year*n, data = kbs, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = kbs,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = kbs,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = kbs, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = kbs,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = kbs,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = kbs, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])

kbs$predicted <- predict(m.null, kbs)

ggplot(kbs, aes(x=spei, y = predicted, color = trt_type)) +
  geom_point(aes(x = spei, y = anpp), color="gray60", size = 0.5) +
  geom_smooth(aes(y = predicted), se = T) +
  theme_bw() +
  #  scale_y_continuous(limits = c(0,2000)) + ##some outliers that make trends hard to see
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP")


### null model that doesn't include year ####

##blank for seeing which sites have SPEI in the model
not_null_sites1 <- tibble(site_code = NA,
                         keep = NA)



for (i in unique(n_sites$site_code)) {
  dat <- n_sites[n_sites$site_code == i, ]
  
  m.null <- lme(anpp ~ n, data = dat, random = ~1|uniqueID, method="ML")
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
  
  not_null_sites1 <- bind_rows(not_null_sites1, pdata)
}

spei_sites1 <- not_null_sites1 %>%
  filter(keep == "yes")
print(spei_sites1)
### ALL turn to yes if the null model doesn't include year

## re-run the above code, but include a second null model that doesn't have year in it

### yarra ####

## prior best was m.Ca
m.null <- lme(anpp ~ year*n, data = yarra, random = ~1|uniqueID, method="ML")
m.null1 <- lme(anpp ~ n, data = yarra, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = yarra,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = yarra,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = yarra, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = yarra,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = yarra,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = yarra, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
### still m.Ca
### cedar creek ####
## prior best was m.Ci
m.null <- lme(anpp ~ year*n, data = cdr, random = ~1|uniqueID, method="ML")
m.null1 <- lme(anpp ~ n, data = cdr, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = cdr,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = cdr,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = cdr, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = cdr,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = cdr,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = cdr, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
## still m.Ci

### kufs ####
## prior best was m.Ca
m.null <- lme(anpp ~ year*n, data = kufs, random = ~1|uniqueID, method="ML")
m.null1 <- lme(anpp ~ n, data = kufs, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = kufs,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = kufs,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = kufs, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = kufs,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = kufs,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = kufs, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
## still m.Ca

###serc ####
## prior best was m.Ca
m.null <- lme(anpp ~ year*n, data = serc, random = ~1|uniqueID, method="ML")
m.null1 <- lme(anpp ~ n, data = serc, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = serc,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = serc,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = serc, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = serc,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = serc,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = serc, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
## still m.Ca

###konza ####
## prior best was m.null
m.null <- lme(anpp ~ year*n, data = konza, random = ~1|uniqueID, method="ML")
m.null1 <- lme(anpp ~ n, data = konza, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = konza,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = konza,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = konza, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = konza,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = konza,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = konza, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
## m.null has the lowest AIC
## but, the for loop didn't run with the original m.null model, so I think the 
## question here is: if the year*n model isn't the null, which other model is the best?
## in that case, it's m.Ci
konza$predicted1 <- predict(m.Ci, konza)

ggplot(konza, aes(x=spei, y = predicted1, color = project_name.x)) +
  facet_grid(p_levels~n_levels, scales = "free")+
  geom_point(aes(x = spei, y = anpp), color="gray60", size = 0.5) +
  geom_smooth(aes(y = predicted1)) +
  theme_bw() +
  #  scale_y_continuous(limits = c(0,2000)) + ##some outliers that make trends hard to see
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP")

###sierra ####
## prior best was m.null
m.null <- lme(anpp ~ year*n, data = sierra, random = ~1|uniqueID, method="ML")
m.null1 <- lme(anpp ~ n, data = sierra, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = sierra,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = sierra,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = sierra, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = sierra,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = sierra,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = sierra, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
## null is still best, cubic additive is second best
sierra$predicted1 <- predict(m.Ca, sierra)


ggplot(sierra, aes(x=spei, y = predicted, color = treatment)) +
  geom_point(aes(x = spei, y = anpp), color="gray60", size = 0.5) +
  geom_smooth(aes(y = predicted), se = F) +
  theme_bw() +
  #  scale_y_continuous(limits = c(0,2000)) + ##some outliers that make trends hard to see
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(x="SPEI",
       y="Aboveground NPP")
###cbgb ####
m.null <- lme(anpp ~ year*n, data = cbgb, random = ~1|uniqueID, method="ML")
m.null1 <- lme(anpp ~ n, data = cbgb, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = cbgb,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = cbgb,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = cbgb, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = cbgb,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = cbgb,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = cbgb, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
## null is still best, linear additive is second best
cbgb$predicted1 <- predict(m.Ca, cbgb)

###niwot ####
m.null <- lme(anpp ~ year*n, data = niwot, random = ~1|uniqueID, method="ML")
m.null1 <- lme(anpp ~ n, data = niwot, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = niwot,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = niwot,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = niwot, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = niwot,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = niwot,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = niwot, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
## null is still best, cubic additive is second best
niwot$predicted1 <- predict(m.Ca, niwot)

###kellogg ####
m.null <- lme(anpp ~ year*n, data = kbs, random = ~1|uniqueID, method="ML")
m.null1 <- lme(anpp ~ n, data = kbs, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = kbs,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = kbs,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = kbs, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = kbs,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = kbs,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = kbs, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.null1, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
## null is still best, linear additive is second best
kbs$predicted1 <- predict(m.Ca, kbs)

