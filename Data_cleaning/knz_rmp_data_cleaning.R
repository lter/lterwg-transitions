### KNZ RaMPs Data Cleaning ###

## Data manipulation packages
library(tidyverse)
source("utilities/data_import.R")

## Data import
knz_rmp_2012 <- read_csv_gdrive("1rq0huVu-EhTEwyKwpr83pGvPVIuztj1w")%>%
  tbl_df()

knz_rmp_2016 <- read_csv_gdrive("1NDqSKWrGsfSdrUfBDk-Y57oS-p-Lcley")%>%
  tbl_df()

## Working data frame
rmp12 <- knz_rmp_2012
rmp16 <- knz_rmp_2016

## The 2012 dataset from the knz lter website does not have treatment codes
## But... I found data from 2016 only that does have trt codes 
## Here:  http://www.konza.ksu.edu/ramps/data.html
## I will extract Trt codes from here

rmp_trts <- rmp16 %>%
  tbl_df() %>%
  dplyr::select(ramp, subplot, Water, Heat) 

## Back to the 2012 data, will need help finding newer data 
## DataCode, Rectype have 1 unique value
unique(rmp12$season)
# Season column some values are capitalized and some not
# Should have 2 variables "S" and "F"

unique(rmp12$RampNo)
# 15 ramps (plots?)

### Add ExpYear
df <- data.frame(RecYear= 1997:2012,
                 ExpYear= 0:15)
rmp12 <- merge.data.frame(rmp12, df, by="RecYear" )

rm(df, knz_rmp_2016, rmp_trts, rmp16, rmp16_0)

## Clean up data from RaMPs!

knz_rmp_clean <- rmp12 %>%
  tbl_df() %>%
  dplyr::select(-DataCode, -RecType, -Date) %>%  # Remove unwanted columns
  tbl_df() %>%
  ## format column names to match the rest of the datasets
  dplyr::mutate(site = "knz",
                project = "rmp",
                #field = "Field",
                year = RecYear,
                expyear = ExpYear, # starts at 0, pretreatment data
                plot = RampNo, 
                #subplot = Subplot,
                abundance = Average, 
                carbon = "0",
                nadd = "0",
                ncess = "0",
                fence = "1",
                burn = "0",
                #rainfall = "0", 
                #warm = "0",
                unitAbund = "biomass_g_m2",
                scaleAbund = "0.4m2",  # check metadata, 4 x 0.1 m2 clips
                species = Species,
                uniqueID = paste(site, field, project, plot, subplot, sep = "_")) %>%
  dplyr::select(year, site, field, project, plot, subplot, uniqueID, 
                carbon, nadd, ncess, fence, burn, rainfall, warm,
                species, abundance, unitAbund, scaleAbund) 
rm(rmp12, rmp16)
