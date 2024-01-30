### KBS Early Successional Microplot ANPP — Main Cropping System Experiment (MCSE) Data Cleaning ###

## Data manipulation packages
library(tidyverse)
library(SPEI)
library(ggpp)
library(nlme)
library(emmeans)
library(MuMIn)
library(visreg)
library(car)
library(piecewiseSEM)
#https://lter.kbs.msu.edu/datatables/448
kbs_monthly_MET= read.table(here::here("Project_3_climate_sensitivity","KBS_data","448-monthly+precipitation+and+air+temperature+1666860249.csv"),
                            sep = ",", header = T)
head(kbs_monthly_MET)
dim(kbs_monthly_MET)
#417   6

#Calculate balances

kbs_monthly_MET$PET <- thornthwaite(kbs_monthly_MET$air_temp_mean, 42.415329)
kbs_monthly_MET$BAL <- kbs_monthly_MET$precipitation-kbs_monthly_MET$PET
summary(kbs_monthly_MET)
ggplot(kbs_monthly_MET,aes(x=year, y=BAL))+geom_point(aes(color=month))
kbs_monthly_MET_ts=ts(kbs_monthly_MET[,-c(1,2)],end = c(2022,9), frequency=12)

#6 month spei 
spei_KBS6=spei(kbs_monthly_MET_ts[,"BAL"], 6)
spei_KBS6_df=try_data_frame(spei_KBS6$fitted)
colnames(spei_KBS6_df)=c("time","SPEI_6m")
head(spei_KBS6_df)
spei_KBS6_df$year=format(as.Date(spei_KBS6_df$time),"%Y")
spei_KBS6_df$month=format(as.Date(spei_KBS6_df$time),"%m")
spei_KBS6_df$time=NULL


#https://lter.kbs.msu.edu/datatables/686
source("Data_cleaning/kbs_early_succ_microplot_msce_cleaning.R") # extra column is disturbance treatment

head(kbs_mcse_e_succ)
dim(kbs_mcse_e_succ)
#757   8
kbs_mcse_e_succ$year=format(as.Date(kbs_mcse_e_succ$sample_date),"%Y")
kbs_mcse_e_succ$month=format(as.Date(kbs_mcse_e_succ$sample_date),"%m")

kbs_mcse_e_succ|>group_by(year,method,disturbed,fertilized)|>summarise(n())
kbs_mcse_e_succ|>group_by(month,method,disturbed,fertilized)|>summarise(n())
#some sites have multiple sampling dates

#Let's take the final harvest from each year 

kbs_mcse_e_succ_last=kbs_mcse_e_succ|>group_by(year,treatment,replicate,method,disturbed,fertilized)|>
  top_n(1,as.Date(sample_date))
dim(kbs_mcse_e_succ_last)
#709  10
kbs_mcse_e_succ_last$month

kbs_mcse_e_succ_last_spei=merge(kbs_mcse_e_succ_last,
                                spei_KBS6_df, 
                                by=c("year","month"))
dim(kbs_mcse_e_succ_last_spei)

#numeric year
kbs_mcse_e_succ_last_spei$year=as.numeric(kbs_mcse_e_succ_last_spei$year)

#Experimental year 

kbs_mcse_e_succ_last_spei$expyear=kbs_mcse_e_succ_last_spei$year-min(kbs_mcse_e_succ_last_spei$year)+1
summary(kbs_mcse_e_succ_last_spei)
#There is a split plot design which I am not sure how to handle... 
#Let's make a unique value for plot replicate
kbs_mcse_e_succ_last_spei|>group_by(disturbed,method)|>summarise(n())

kbs_mcse_e_succ_last_spei$uniq_replicate=with(kbs_mcse_e_succ_last_spei, 
                                         interaction(replicate,fertilized,disturbed,method))




#Model selection----------------
#Candidate model set
#L=linear, Q=quadratic, C=cubic, a=additive, i=interaction
m.null<-lme(biomass_g_m2~year*fertilized,data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")
m.La<-lme(biomass_g_m2~SPEI_6m+fertilized,data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")
m.Li<-lme(biomass_g_m2~SPEI_6m*fertilized,data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")
m.Qa<-lme(biomass_g_m2~SPEI_6m+fertilized+I(SPEI_6m^2),data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")
m.Qi<-lme(biomass_g_m2~SPEI_6m*fertilized+I(SPEI_6m^2)*fertilized,data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")
m.Ca<-lme(biomass_g_m2~SPEI_6m+fertilized+I(SPEI_6m^2)+I(SPEI_6m^3),data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")
m.Ci<-lme(biomass_g_m2~SPEI_6m*fertilized+I(SPEI_6m^2)*fertilized+I(SPEI_6m^3)*fertilized,data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")

m.La<-lme(biomass_g_m2~SPEI_6m+fertilized,data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")
m.Li<-lme(biomass_g_m2~SPEI_6m*fertilized,data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")
m.Qa<-lme(biomass_g_m2~SPEI_6m+fertilized+I(SPEI_6m^2),data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")
m.Qi<-lme(biomass_g_m2~SPEI_6m*fertilized+I(SPEI_6m^2)*fertilized,data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")
m.Ca<-lme(biomass_g_m2~SPEI_6m+fertilized+I(SPEI_6m^2)+I(SPEI_6m^3),data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")
m.Ci<-lme(biomass_g_m2~SPEI_6m*fertilized+I(SPEI_6m^2)*fertilized+I(SPEI_6m^3)*fertilized,data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")

# model selection
AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)
AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)[AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)[,2]<=
                                              min(AICc(m.null, m.La,m.Li,m.Qa,m.Qi,m.Ca,m.Ci)[,2])+2,]
#Best model is m.null
#m.null  6 4320.802
# AR1 - autocorrelation 1, AR1 - autocorrelation 2 to best model from above
#This is only necessary if year includes non-integer numbers
int.year <- kbs_mcse_e_succ_last_spei$expyear*2-1 #convert to integer
kbs_mcse_e_succ_last_spei$expyear <- int.year
#Fit temporal autocorrelation models
m.AR1<-update(m.null,correlation=corAR1(form=~expyear))
m.AR2<-update(m.null,correlation=corARMA(form=~expyear,p=2))
# model selection
AICc(m.null,m.AR1,m.AR2)
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
Anova(m.AR2,type=2)# Chisq test. Mostly similar except for significance of "SPEI_6m"

#Param estimates and post-hoc pairwise comparisons
emtrends(m.AR2,~ fertilized | degree, "year", max.degree = 3)
pairs(emtrends(m.AR2,~ fertilized | degree , "year", max.degree = 3))


#Visualize CSF results---------------------
# get a plot of estimated values from the model, by each depth
# visreg with ggplot graphics
#visreg(fit=m.AR2,"SPEI_6m",type="conditional",by="fertilized",gg=TRUE,partial=F,rug=F)+ 
 # geom_point(aes(x=SPEI_6m,y=biomass_g_m2,col=year),alpha=0.2,data=kbs_mcse_e_succ_last_spei)+
#  theme_bw()+
#  labs(x="SPEI",
 #      y="Aboveground total cover")

#Alternative visualization code if the above doesn't work
kbs_mcse_e_succ_last_spei$predicted<-predict(m.AR2, kbs_mcse_e_succ_last_spei)
ggplot(kbs_mcse_e_succ_last_spei, aes(x=year, y=predicted)) +
  facet_wrap(~fertilized)+
  geom_point(aes(x=year, y=biomass_g_m2), color="gray60", size=0.5) +
  geom_smooth(aes(y=predicted), color="gray20")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  labs(x="Year",
       y="Aboveground total cover")


#Remove the effects of year?


m.La.YR<-lme(biomass_g_m2~SPEI_6m+fertilized+year,data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")
m.Li.YR<-lme(biomass_g_m2~SPEI_6m*fertilized+year,data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")
m.Qa.YR<-lme(biomass_g_m2~SPEI_6m+fertilized+I(SPEI_6m^2)+year,data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")
m.Qi.YR<-lme(biomass_g_m2~SPEI_6m*fertilized+I(SPEI_6m^2)*fertilized+year,data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")
m.Ca.YR<-lme(biomass_g_m2~SPEI_6m+fertilized+I(SPEI_6m^2)+I(SPEI_6m^3)+year,data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")
m.Ci.YR<-lme(biomass_g_m2~SPEI_6m*fertilized+I(SPEI_6m^2)*fertilized+I(SPEI_6m^3)*fertilized+year,data=kbs_mcse_e_succ_last_spei,random=~1|uniq_replicate,method="ML")

# model selection
AICc(m.null, m.La.YR,m.Li.YR,m.Qa.YR,m.Qi.YR,m.Ca.YR,m.Ci.YR)
AICc(m.null, m.La.YR,m.Li.YR,m.Qa.YR,m.Qi.YR,m.Ca.YR,m.Ci.YR)[AICc(m.null, m.La.YR,m.Li.YR,m.Qa.YR,m.Qi.YR,m.Ca.YR,m.Ci.YR)[,2]<=
                                              min(AICc(m.null, m.La.YR,m.Li.YR,m.Qa.YR,m.Qi.YR,m.Ca.YR,m.Ci.YR)[,2])+2,]
#Best model is m.null, but m.Ca.YR is within 2
#m.null  6 4320.802
# AR1 - autocorrelation 1, AR1 - autocorrelation 2 to best model from above
#This is only necessary if year includes non-integer numbers
int.year <- kbs_mcse_e_succ_last_spei$expyear*2-1 #convert to integer
kbs_mcse_e_succ_last_spei$expyear <- int.year
#Fit temporal autocorrelation models
m.AR1.YR<-update(m.Ca.YR,correlation=corAR1(form=~expyear))
m.AR2.YR<-update(m.Ca.YR,correlation=corARMA(form=~expyear,p=2))
# model selection
AICc(m.Ca.YR,m.AR1.YR,m.AR2.YR)
# best model m.AR2.YR

rsquared(m.AR2.YR)
#Marginal R2:  the proportion of variance explained by the fixed factor(s) alone
#Conditional R2: he proportion of variance explained by both the fixed and random factors

#Evaluate model assumptions

plot(m.AR2.YR)#a bit funnel shaped
qqPlot(residuals(m.AR2.YR))
hist(residuals(m.AR2.YR))

#Do sketchy frequentist tests on best model
anova(m.AR2.YR,type="marginal")#F test
Anova(m.AR2.YR,type=2)# Chisq test. Mostly similar except for significance of "SPEI_6m"

#Param estimates and post-hoc pairwise comparisons
emtrends(m.AR2.YR,~ fertilized | degree, "year", max.degree = 3)
pairs(emtrends(m.AR2.YR,~ fertilized | degree , "year", max.degree = 3))
#The quadratic slope is the most different

#Visualize CSF results---------------------
# get a plot of estimated values from the model, by each depth
# visreg with ggplot graphics
#visreg(fit=m.AR2.YR,"SPEI_6m",type="conditional",by="fertilized",gg=TRUE,partial=F,rug=F)+ 
# geom_point(aes(x=SPEI_6m,y=biomass_g_m2,col=year),alpha=0.2,data=kbs_mcse_e_succ_last_spei)+
#  theme_bw()+
#  labs(x="SPEI",
#      y="Aboveground total cover")

#Alternative visualization code if the above doesn't work
kbs_mcse_e_succ_last_spei$predicted.YR<-predict(m.AR2.YR, kbs_mcse_e_succ_last_spei)
ggplot(kbs_mcse_e_succ_last_spei, aes(x=SPEI_6m, y=predicted)) +
  facet_wrap(~fertilized)+
  geom_point(aes(x=SPEI_6m, y=biomass_g_m2), color="gray60", size=0.5) +
  geom_smooth(aes(y=predicted), color="gray20")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  labs(x="SPEI (6 month)",
       y="Aboveground total cover")







