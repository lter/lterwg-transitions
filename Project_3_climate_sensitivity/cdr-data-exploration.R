setwd("~/Desktop/")
cdr_climate <- read.csv("cdr_weather.csv")

cdr_climate$newdate <- strptime(as.character(cdr_climate$Date), "%m/%d/%y")
cdr_climate$nd <- format(cdr_climate$newdate, "%Y-%m-%d")
### R automatically makes years ending in 00 - 68 to 2000s, so need to change 62 - 68 to 1900s

cdr_climate$year <- substr(cdr_climate$nd, 1, 4) ##just the year

group_1900 <- filter(cdr_climate, year > 2017)
years_1900 <- group_1900[,10]
correct_years_1900 <- gsub("20", "19", years_1900)
group_2000 <- filter(cdr_climate, year <= 2017)

group_1900$year <- correct_years_1900

cdr_climate_final <- rbind(group_1900, group_2000)
names(cdr_climate_final)

cdrclim <- cdr_climate_final[,c(1,4,10)]

cdrclim1 <- cdrclim %>%
  group_by(year) %>%
  summarize(MAP_mm = sum(25.4*Precip.inches.)) ## change inches to millimeters

cdrclim1$year <- as.numeric(cdrclim1$year)

ggplot(cdrclim1, aes(MAP_mm)) +
  geom_density() +
  theme_bw()
cdr_clean_no_na <- na.omit(cdr_clean)
cdr_clean_no_na$year <- as.numeric(cdr_clean_no_na$year)
cdr_anpp_full <- cdr_clean_no_na %>%
  group_by(year, site, plot, nadd, ncess, uniqueID) %>%
  summarize(NPP = sum(abundance)) 
  
cdr_clim_anpp_full <- left_join(cdrclim1, cdr_anpp_full, by = "year") ##use for models
cdr_clim_anpp_full <- na.omit(cdr_clim_anpp_full)

cdr_anpp_plot <- cdr_anpp_full %>%
  group_by(year, site, nadd, ncess) %>%
  summarize(ANPP = mean(NPP))

cdr_anpp_plot$year <- as.numeric(cdr_anpp_plot$year)
str(cdr_anpp_plot)
cdr_clim_anpp_plot <- left_join(cdrclim1, cdr_anpp_plot, by = "year")
cca <- na.omit(cdr_clim_anpp_plot)
str(cca)
cca$N <- ifelse(cca$nadd == 0, 0, 1)

ggplot(cca, aes(x = MAP_mm, y = ANPP)) +
  geom_point() +
  facet_wrap(~nadd) +
  #geom_smooth(method = "loess", se = F) +
  theme_bw() 
ccaf <- cdr_clim_anpp_full



library(here)
library(vegan)
library(tidyverse)
library(nlme)
library(emmeans)
library(MuMIn)
library(visreg)
library(car)
library(piecewiseSEM)

#Candidate model set
#L=linear, Q=quadratic, C=cubic, a=additive, i=interaction
m.null<-lme(NPP~year*nadd,data=ccaf,random=~1|uniqueID,method="ML")
m.La<-lme(NPP~MAP_mm+nadd,data=ccaf,random=~1|uniqueID,method="ML")
m.Li<-lme(NPP~MAP_mm*nadd,data=ccaf,random=~1|uniqueID,method="ML")
m.Qa<-lme(NPP~MAP_mm+nadd+I(MAP_mm^2),data=ccaf,random=~1|uniqueID,method="ML")
m.Qi<-lme(NPP~MAP_mm*nadd+I(MAP_mm^2)*nadd,data=ccaf,random=~1|uniqueID,method="ML")
m.Ca<-lme(NPP~MAP_mm+nadd+I(MAP_mm^2)+I(MAP_mm^3),data=ccaf,random=~1|uniqueID,method="ML")
m.Ci<-lme(NPP~MAP_mm*nadd+I(MAP_mm^2)*nadd+I(MAP_mm^3)*nadd,data=ccaf,random=~1|uniqueID,method="ML")

# model selection
AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)
min(AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)[,2])
#Best model is m.Ci, but within 22 of m.Qi
m.Ci
# AR1 - autocorrelation 1, AR1 - autocorrelation 2 to best model from above
#This is only necessary if expyear includes non-integer numbers
int.year <- ccaf$expyear*2-1 #convert to integer
exp.clim$expyear <- int.year
#Fit temporal autocorrelation models
m.AR1<-update(m.Ci,correlation=corAR1(form=~year))
m.AR2<-update(m.Ci,correlation=corARMA(form=~year,p=2))
# model selection
AICc(m.Ci,m.AR1,m.AR2)
# best model m.AR2
rsquared(m.AR2)
#Marginal R2:  the proportion of variance explained by the fixed factor(s) alone
#Conditional R2: he proportion of variance explained by both the fixed and random factors

#Evaluate model assumptions
plot(m.AR2)#a bit funnel shaped
qqPlot(residuals(m.AR2))
hist(residuals(m.AR2))

#Do sketchy frequentist tests on best model
anova(m.AR2,type="marginal")#F test
Anova(m.AR2,type=2)# Chisq test. Mostly similar except for significance of "SPEI.comp"

#Param estimates and post-hoc pairwise comparisons
emtrends(m.AR2,~ nadd | degree, "SPEI.comp", max.degree = 3)
pairs(emtrends(m.AR2,~ nadd | degree , "SPEI.comp", max.degree = 3))
#The quadratic slope is the most different

#Visualize CSF results---------------------
# get a plot of estimated values from the model, by each depth
# visreg with ggplot graphics
visreg(fit=m.Ci,"MAP_mm",type="conditional",by="nadd",gg=TRUE,partial=F,rug=F)+ 
  geom_point(aes(x=MAP_mm,y=NPP,col=year),alpha=0.2,data=ccaf)+
  theme_bw()+
  labs(x="MAP_mm",
       y="ANPP")

#Alternative visualization code if the above doesn't work
ccaf$predicted<-predict(m.Ci, ccaf)
ggplot(ccaf, aes(x=MAP_mm, y=predicted)) +
  facet_wrap(~nadd)+
  geom_point(aes(x=MAP_mm, y=NPP), color="gray60", size=0.5) +
  geom_smooth(aes(y=predicted), color="gray20")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  labs(x="MAP_mm",
       y="ANPP") +
  scale_y_continuous(limits = c(0,1000))


### starting just looking at key species

## "andropogon gerardi" also "Andropogon gerardi"
## "poa pratensis" also "Poa pratensis"
## "Schizachyrium scoparium"
## "Agropyron repens" alos "agropyron repens"

andro <- filter(cdr_clean_no_na, species == "andropogon gerardi" | species == "Andropogon gerardi")
agro <- filter(cdr_clean_no_na, species == "agropyron repens" | species == "Agropyron repens")
schiz <- filter(cdr_clean_no_na, species == "Schizachyrium scoparium")
poa <- filter(cdr_clean_no_na, species == "poa pratensis" | species == "Poa pratensis")

### andro ####
andro$year <- as.numeric(andro$year)
andro_full <- andro %>%
  group_by(year, site, plot, nadd, ncess, uniqueID) %>%
  summarize(NPP = sum(abundance)) 

andro_clim_anpp_full <- left_join(cdrclim1, andro_full, by = "year") ##use for models
andro_clim_anpp_full <- na.omit(andro_clim_anpp_full)

andro_anpp_plot <- andro_full %>%
  group_by(year, site, nadd, ncess) %>%
  summarize(ANPP = mean(NPP))

andro_anpp_plot$year <- as.numeric(andro_anpp_plot$year)
str(andro_anpp_plot)
andro_clim_anpp_plot <- left_join(cdrclim1, andro_anpp_plot, by = "year")
andro_cca <- na.omit(andro_clim_anpp_plot)
str(andro_cca)
andro_cca$N <- ifelse(cca$nadd == 0, 0, 1)

ggplot(andro_cca, aes(x = MAP_mm, y = ANPP)) +
  geom_point() +
  facet_wrap(~nadd) +
  #geom_smooth(method = "loess", se = F) +
  theme_bw() 
andro_ccaf <- andro_clim_anpp_full

m.null<-lme(NPP~year*nadd,data=andro_ccaf,random=~1|uniqueID,method="ML")
m.La<-lme(NPP~MAP_mm+nadd,data=andro_ccaf,random=~1|uniqueID,method="ML")
m.Li<-lme(NPP~MAP_mm*nadd,data=andro_ccaf,random=~1|uniqueID,method="ML")
m.Qa<-lme(NPP~MAP_mm+nadd+I(MAP_mm^2),data=andro_ccaf,random=~1|uniqueID,method="ML")
m.Qi<-lme(NPP~MAP_mm*nadd+I(MAP_mm^2)*nadd,data=andro_ccaf,random=~1|uniqueID,method="ML")
m.Ca<-lme(NPP~MAP_mm+nadd+I(MAP_mm^2)+I(MAP_mm^3),data=andro_ccaf,random=~1|uniqueID,method="ML")
m.Ci<-lme(NPP~MAP_mm*nadd+I(MAP_mm^2)*nadd+I(MAP_mm^3)*nadd,data=andro_ccaf,random=~1|uniqueID,method="ML")

# model selection
AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)
min(AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)[,2])
#Best model is m.null, but within 3 of m.Qa and m.Ca
m.Ci

andro_ccaf$predicted<-predict(m.null, andro_ccaf)
andro_plot <- ggplot(andro_ccaf, aes(x=MAP_mm, y=predicted)) +
  facet_wrap(~nadd)+
  geom_point(aes(x=MAP_mm, y=NPP), color="gray60", size=0.5) +
  geom_smooth(aes(y=predicted), color="gray20")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  labs(x="MAP_mm",
       y="ANPP") +
  scale_y_continuous(limits = c(0,400))

#### agro ####

agro$year <- as.numeric(agro$year)
agro_full <- agro %>%
  group_by(year, site, plot, nadd, ncess, uniqueID) %>%
  summarize(NPP = sum(abundance)) 

agro_clim_anpp_full <- left_join(cdrclim1, agro_full, by = "year") ##use for models
agro_clim_anpp_full <- na.omit(agro_clim_anpp_full)

agro_anpp_plot <- agro_full %>%
  group_by(year, site, nadd, ncess) %>%
  summarize(ANPP = mean(NPP))

agro_anpp_plot$year <- as.numeric(agro_anpp_plot$year)
str(agro_anpp_plot)
agro_clim_anpp_plot <- left_join(cdrclim1, agro_anpp_plot, by = "year")
agro_cca <- na.omit(agro_clim_anpp_plot)
str(agro_cca)
agro_cca$N <- ifelse(cca$nadd == 0, 0, 1)

ggplot(agro_cca, aes(x = MAP_mm, y = ANPP)) +
  geom_point() +
  facet_wrap(~nadd) +
  #geom_smooth(method = "loess", se = F) +
  theme_bw() 
agro_ccaf <- agro_clim_anpp_full

m.null<-lme(NPP~year*nadd,data=agro_ccaf,random=~1|uniqueID,method="ML")
m.La<-lme(NPP~MAP_mm+nadd,data=agro_ccaf,random=~1|uniqueID,method="ML")
m.Li<-lme(NPP~MAP_mm*nadd,data=agro_ccaf,random=~1|uniqueID,method="ML")
m.Qa<-lme(NPP~MAP_mm+nadd+I(MAP_mm^2),data=agro_ccaf,random=~1|uniqueID,method="ML")
m.Qi<-lme(NPP~MAP_mm*nadd+I(MAP_mm^2)*nadd,data=agro_ccaf,random=~1|uniqueID,method="ML")
m.Ca<-lme(NPP~MAP_mm+nadd+I(MAP_mm^2)+I(MAP_mm^3),data=agro_ccaf,random=~1|uniqueID,method="ML")
m.Ci<-lme(NPP~MAP_mm*nadd+I(MAP_mm^2)*nadd+I(MAP_mm^3)*nadd,data=agro_ccaf,random=~1|uniqueID,method="ML")

# model selection
AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)
min(AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)[,2])
#Best model is m.Ci, but within 54 of m.Ca
m.Ci

agro_ccaf$predicted<-predict(m.Ci, agro_ccaf)
agro_plot <- ggplot(agro_ccaf, aes(x=MAP_mm, y=predicted)) +
  facet_wrap(~nadd)+
  geom_point(aes(x=MAP_mm, y=NPP), color="gray60", size=0.5) +
  geom_smooth(aes(y=predicted), color="gray20")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  labs(x="MAP_mm",
       y="ANPP") +
  scale_y_continuous(limits = c(0,1000))

### schiz ####
schiz$year <- as.numeric(schiz$year)
schiz_full <- schiz %>%
  group_by(year, site, plot, nadd, ncess, uniqueID) %>%
  summarize(NPP = sum(abundance)) 

schiz_clim_anpp_full <- left_join(cdrclim1, schiz_full, by = "year") ##use for models
schiz_clim_anpp_full <- na.omit(schiz_clim_anpp_full)

schiz_anpp_plot <- schiz_full %>%
  group_by(year, site, nadd, ncess) %>%
  summarize(ANPP = mean(NPP))

schiz_anpp_plot$year <- as.numeric(schiz_anpp_plot$year)
str(schiz_anpp_plot)
schiz_clim_anpp_plot <- left_join(cdrclim1, schiz_anpp_plot, by = "year")
schiz_cca <- na.omit(schiz_clim_anpp_plot)
str(schiz_cca)
schiz_cca$N <- ifelse(schiz_cca$nadd == 0, 0, 1)

ggplot(schiz_cca, aes(x = MAP_mm, y = ANPP)) +
  geom_point() +
  facet_wrap(~nadd) +
  #geom_smooth(method = "loess", se = F) +
  theme_bw() 
schiz_ccaf <- schiz_clim_anpp_full

m.null<-lme(NPP~year*nadd,data=schiz_ccaf,random=~1|uniqueID,method="ML")
m.La<-lme(NPP~MAP_mm+nadd,data=schiz_ccaf,random=~1|uniqueID,method="ML")
m.Li<-lme(NPP~MAP_mm*nadd,data=schiz_ccaf,random=~1|uniqueID,method="ML")
m.Qa<-lme(NPP~MAP_mm+nadd+I(MAP_mm^2),data=schiz_ccaf,random=~1|uniqueID,method="ML")
m.Qi<-lme(NPP~MAP_mm*nadd+I(MAP_mm^2)*nadd,data=schiz_ccaf,random=~1|uniqueID,method="ML")
m.Ca<-lme(NPP~MAP_mm+nadd+I(MAP_mm^2)+I(MAP_mm^3),data=schiz_ccaf,random=~1|uniqueID,method="ML")
m.Ci<-lme(NPP~MAP_mm*nadd+I(MAP_mm^2)*nadd+I(MAP_mm^3)*nadd,data=schiz_ccaf,random=~1|uniqueID,method="ML")

# model selection
AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)
min(AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)[,2])
#Best model is m.Ci, but within 7 of m.Ca
m.Ci

schiz_ccaf$predicted<-predict(m.Ci, schiz_ccaf)
schiz_plot <- ggplot(schiz_ccaf, aes(x=MAP_mm, y=predicted)) +
  facet_wrap(~nadd)+
  geom_point(aes(x=MAP_mm, y=NPP), color="gray60", size=0.5) +
  geom_smooth(aes(y=predicted), color="gray20")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  labs(x="MAP_mm",
       y="ANPP") +
  scale_y_continuous(limits = c(0,400))
schiz_plot

### poa ####
poa$year <- as.numeric(poa$year)
poa_full <- poa %>%
  group_by(year, site, plot, nadd, ncess, uniqueID) %>%
  summarize(NPP = sum(abundance)) 

poa_clim_anpp_full <- left_join(cdrclim1, poa_full, by = "year") ##use for models
poa_clim_anpp_full <- na.omit(poa_clim_anpp_full)

poa_anpp_plot <- poa_full %>%
  group_by(year, site, nadd, ncess) %>%
  summarize(ANPP = mean(NPP))

poa_anpp_plot$year <- as.numeric(poa_anpp_plot$year)
str(poa_anpp_plot)
poa_clim_anpp_plot <- left_join(cdrclim1, poa_anpp_plot, by = "year")
poa_cca <- na.omit(poa_clim_anpp_plot)
str(poa_cca)
poa_cca$N <- ifelse(poa_cca$nadd == 0, 0, 1)

ggplot(poa_cca, aes(x = MAP_mm, y = ANPP)) +
  geom_point() +
  facet_wrap(~nadd) +
  #geom_smooth(method = "loess", se = F) +
  theme_bw() 
poa_ccaf <- poa_clim_anpp_full

m.null<-lme(NPP~year*nadd,data=poa_ccaf,random=~1|uniqueID,method="ML")
m.La<-lme(NPP~MAP_mm+nadd,data=poa_ccaf,random=~1|uniqueID,method="ML")
m.Li<-lme(NPP~MAP_mm*nadd,data=poa_ccaf,random=~1|uniqueID,method="ML")
m.Qa<-lme(NPP~MAP_mm+nadd+I(MAP_mm^2),data=poa_ccaf,random=~1|uniqueID,method="ML")
m.Qi<-lme(NPP~MAP_mm*nadd+I(MAP_mm^2)*nadd,data=poa_ccaf,random=~1|uniqueID,method="ML")
m.Ca<-lme(NPP~MAP_mm+nadd+I(MAP_mm^2)+I(MAP_mm^3),data=poa_ccaf,random=~1|uniqueID,method="ML")
m.Ci<-lme(NPP~MAP_mm*nadd+I(MAP_mm^2)*nadd+I(MAP_mm^3)*nadd,data=poa_ccaf,random=~1|uniqueID,method="ML")

# model selection
AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)
min(AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)[,2])
#Best model is m.null, but within 54 of m.Ca
m.Ci

poa_ccaf$predicted<-predict(m.null, poa_ccaf)
poa_plot <- ggplot(poa_ccaf, aes(x=MAP_mm, y=predicted)) +
  facet_wrap(~nadd)+
  geom_point(aes(x=MAP_mm, y=NPP), color="gray60", size=0.5) +
  geom_smooth(aes(y=predicted), color="gray20")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  labs(x="MAP_mm",
       y="ANPP") +
  scale_y_continuous(limits = c(0,400))
poa_plot


### all plots####

andro_plot
agro_plot
schiz_plot
poa_plot
