


#Beguería S. (2017) SPEIbase: R code used in generating the SPEI global database, doi:10.5281/zenodo.834462.


#Beguería, S., Vicente-Serrano, S.M. y Angulo, M., (2010): A multi-scalar global drought data set: the SPEIbase: A new gridded product for the analysis of drought variability and impacts. Bulletin of the American Meteorological Society. 91, 1351-1354
#Vicente-Serrano, S.M., Beguería, S., López-Moreno, J.I., Angulo, M., El Kenawy, A. (2010): A new global 0.5° gridded dataset (1901-2006) of a multiscalar drought index: comparison with current drought index datasets based on the Palmer Drought Severity Index. Journal of Hydrometeorology. 11: 1033-1043

rm(list = ls())
#install.packages("tidync", type = "binary")
library(RNetCDF)
#library(ncdf4)
library(tidync)
library(dplyr)
library(ggplot2)
library(here)
library(ggh4x)

#https://digital.csic.es/handle/10261/288226
# Global_SPEI_nc_tidy <- tidync(here::here("SPEI","spei06.nc"))
# print(Global_SPEI_nc_tidy)


Global_SPEI_nc<-
ncdf4::nc_open(here::here("SPEI","spei06.nc"))


CORRE_SPEI_raw<-
  read.csv(here::here("SPEI","SPEI_CORRE.csv"))
head(CORRE_SPEI_raw)
CORRE_SPEI<-
  CORRE_SPEI_raw|>
  distinct(site_code,.keep_all = TRUE)

dim(CORRE_SPEI)
#70 16
rm(CORRE_SPEI_raw)
CORRE_SPEI_5th=CORRE_SPEI[1:5,]
SPEI_all<-tibble(spei=numeric(),
                 lon=numeric(),
                 lat=numeric(),
                 time=numeric(),
                 site_code=character())
for (i in 1:length(CORRE_SPEI[,1])){
  Lat <- CORRE_SPEI$Latitude[i]
  Long <- CORRE_SPEI$Longitude[i]
  site <- CORRE_SPEI$site_code[i]
  SPEI_per_site<-
    Global_SPEI_nc_tidy|>
    hyper_filter(lat=lat==nth(lat,which.min(abs(lat-Lat))),
                 lon=lon==nth(lon,which.min(abs(lon-Long))))|>
    hyper_tibble(select_var = "spei")|>
    mutate(site_code=rep(site))
  SPEI_all<-bind_rows(SPEI_all,SPEI_per_site)
}

dim(SPEI_all)
head(SPEI_all)

summary(SPEI_all)


SPEI_all_v2<-
  SPEI_all|>
  group_by(site_code)|>
  arrange(time)|>
  mutate(year=c(rep(1901, 7),rep(1902:2021, each=12)),
         month=c(seq(from=6,to=12), rep(seq(from=1,to=12), times= (2021-1901))))|>
  mutate(Rdate=paste(month,"1",year,sep="/"))|>
  mutate(Rdate=as.Date(Rdate,"%m/%d/%Y"))

summary(SPEI_all_v2$Rdate)

dim(SPEI_all_v2)
  

ggplot(SPEI_all_v2|>
         filter(site_code=="Alberta",
                month==6),
       aes(x=Rdate, 
           y=spei))+
  geom_point()


CoRRE_RawAbundance=read.csv(here::here("SPEI","CoRRE_RawAbundance_20230501.csv"))
head(CoRRE_RawAbundance)
dim(CoRRE_RawAbundance)
#618950     13

CoRRE_RawAbundance[1:100,]

CoRRE_RawAbundance_dup<-
  CoRRE_RawAbundance|>
  distinct()

dim(CoRRE_RawAbundance_dup)

CoRRE_RawAbundance_dup|>
  group_by(site_code,data_type)|>
  summarise(n())





CoRRE_RawAbundance_sub<-
  CoRRE_RawAbundance|>
  distinct(site_code,calendar_year,project_name)
dim(CoRRE_RawAbundance_sub)
#312 
#CoRRE_RawAbundance_sub$trt_yr=rep("treated")

head(SPEI_all_v2)
head(CoRRE_RawAbundance_sub)

CoRRE_RawAbundance_sub$trt_yr=rep("treated")


SPEI_all_trt_yr<-
  merge(SPEI_all_v2,
        CoRRE_RawAbundance_sub,
        by.x = c("site_code","year"),
        by.y = c("site_code","calendar_year"),
        all.x = T)




head(SPEI_all_trt_yr)
SPEI_all_trt_yr_v2<-
  SPEI_all_trt_yr|>
  mutate(trt_yr=case_when(is.na(trt_yr)~"outside_treat_period",
                          TRUE~trt_yr))


SPEI_all_trt_yr_v2$Rdate=NULL
SPEI_all_trt_yr_v2$time=NULL
head(SPEI_all_trt_yr_v2)

dim(SPEI_all_trt_yr_v2)
saveRDS(SPEI_all_trt_yr_v2,file = here::here("SPEI","SPEI_all_trt_yr_total.RDS"))


readRDS()
head(SPEI_all_trt_yr)
SPEI_all_trt_yr|>
  filter(site_code%in%unique(CoRRE_RawAbundance_sub$site_code))|>
  group_by(site_code)|>
  summarise(sum(is.na(trt_yr)))



SPEI_all_trt_yr_sub<-
  SPEI_all_trt_yr|>
  filter(site_code%in%unique(CoRRE_RawAbundance_sub$site_code))



#SPEI_all_trt_yr_sub|>
#  group_by(site_code,trt_yr)|>
#  summarise(n())


summary(SPEI_all_trt_yr_sub)
ggplot(SPEI_all_trt_yr_sub|>
         filter(month==6),
       aes(x=spei,y=1))+
  geom_point()+facet_nested_wrap(~site_code+project_name)


ggplot(SPEI_all_trt_yr_sub|>
         filter(month==6),
       aes(x=spei))+
  geom_histogram()+facet_nested_wrap(~site_code+project_name)+
  theme_bw()



ggplot(SPEI_all_trt_yr_sub|>
         filter(month==6),
       aes(x=spei))+
  geom_density()+facet_nested_wrap(~site_code+project_name)

head(CoRRE_RawAbundance)

CoRRE_RawAbundance_sum<-
  CoRRE_RawAbundance|>
  group_by(site_code, project_name,treatment,plot_id,block)


SPEI_all_trt_yr<-
  merge(SPEI_all_v2,
        CoRRE_RawAbundance_sub,
        by.x = c("site_code","year"),
        by.y = c("site_code","calendar_year"))


ggplot(SPEI_all_trt_yr_sub|>
         filter(month==6),
       aes(x=spei, ))+
  geom_density()+facet_nested_wrap(~site_code+project_name)

#CORRE_SPEI_test<-
 # Global_SPEI_nc_tidy|>
  #hyper_filter(lat=lat==nth(lat,which.min(abs(lat-subset(CORRE_SPEI,site_code=="Alberta")$Latitude))),
   #            lon=lon==nth(lon,which.min(abs(lon-subset(CORRE_SPEI,site_code=="Alberta")$Longitude))))|>
  #hyper_tibble(select_var = "spei")


ggplot(CORRE_SPEI_test,
       aes(x=spei))+
  geom_histogram()
