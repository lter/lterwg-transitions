#Example code using data product from data cleaning/harmonized coed
#Fit CSFs for SEV NFert experiment
#AC 220222
#minor changes by LPB 10272022

library(here)
library(vegan)
library(tidyverse)
library(nlme)
library(emmeans)
library(MuMIn)
library(visreg)
library(car)
library(piecewiseSEM)

#Read in cleaned experiment data and climate data
exp.dat<-read.csv(here::here("Project_3_climate_sensitivity","sev_nfert_clean.csv"))
clim.dat2<-read.csv(here::here("Project_3_climate_sensitivity","sev_nfert_clim.csv"))

#Data reshaping and combining----------------------
#get rid of first column if row names read in as first column
exp.dat<-exp.dat[,-1]

#Reshape exp data into wide form
exp.wide<- exp.dat %>%
  pivot_wider(names_from = species, values_from = abundance,
              values_fill = 0)

#Calculate total cover
Var_1 <- "Bouteloua eriopoda" #First plant
Var_2 <- "Machaeranthera tanacetifolia" #Last plant
exp.tot<- exp.wide %>%
  mutate(tot.cover = select(., all_of(Var_1):all_of(Var_2)) %>% rowSums(na.rm = TRUE))

#Add climate data
exp.clim <- merge.data.frame(exp.tot,clim.dat,by.x="year",by.y="year_seas")

#Model selection----------------
#Candidate model set
#L=linear, Q=quadratic, C=cubic, a=additive, i=interaction
m.null<-lme(tot.cover~year*nadd,data=exp.clim,random=~1|uniqueID,method="ML")
m.La<-lme(tot.cover~SPEI.comp+nadd,data=exp.clim,random=~1|uniqueID,method="ML")
m.Li<-lme(tot.cover~SPEI.comp*nadd,data=exp.clim,random=~1|uniqueID,method="ML")
m.Qa<-lme(tot.cover~SPEI.comp+nadd+I(SPEI.comp^2),data=exp.clim,random=~1|uniqueID,method="ML")
m.Qi<-lme(tot.cover~SPEI.comp*nadd+I(SPEI.comp^2)*nadd,data=exp.clim,random=~1|uniqueID,method="ML")
m.Ca<-lme(tot.cover~SPEI.comp+nadd+I(SPEI.comp^2)+I(SPEI.comp^3),data=exp.clim,random=~1|uniqueID,method="ML")
m.Ci<-lme(tot.cover~SPEI.comp*nadd+I(SPEI.comp^2)*nadd+I(SPEI.comp^3)*nadd,data=exp.clim,random=~1|uniqueID,method="ML")

# model selection
AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)
min(AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)[,2])
#Best model is m.Ci, but within 2 of m.Qi

# AR1 - autocorrelation 1, AR1 - autocorrelation 2 to best model from above
#This is only necessary if expyear includes non-integer numbers
int.year <- exp.clim$expyear*2-1 #convert to integer
exp.clim$expyear <- int.year
#Fit temporal autocorrelation models
m.AR1<-update(m.Ci,correlation=corAR1(form=~expyear))
m.AR2<-update(m.Ci,correlation=corARMA(form=~expyear,p=2))
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
visreg(fit=m.AR2,"SPEI.comp",type="conditional",by="nadd",gg=TRUE,partial=F,rug=F)+ 
  geom_point(aes(x=SPEI.comp,y=tot.cover,col=year),alpha=0.2,data=exp.clim)+
  theme_bw()+
  labs(x="SPEI",
       y="Aboveground total cover")

#Alternative visualization code if the above doesn't work
exp.clim$predicted<-predict(m.AR2, exp.clim)
ggplot(exp.clim, aes(x=SPEI.comp, y=predicted)) +
  facet_wrap(~nadd)+
  geom_point(aes(x=SPEI.comp, y=tot.cover), color="gray60", size=0.5) +
  geom_smooth(aes(y=predicted), color="gray20")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  labs(x="SPEI",
       y="Aboveground total cover")

