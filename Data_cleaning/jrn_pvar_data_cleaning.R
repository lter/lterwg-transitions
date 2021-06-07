### Jornada Precip Variability Data Cleaning ###

## Data manipulation packages
library(tidyverse)
source("utilities/data_import.R")

## Data import
jrn_pvar <- read_csv_gdrive("16vTCcumUlMqvpUjyzhJOl9bg7NK0DrKv")%>%
  tbl_df()

## Working data frame
jrnpvar <- jrn_pvar

## This dataset already has total ANPP 
## will remove it for now to keep data in long format

jrnpvar <- jrnpvar %>%
  tbl_df() %>%
  filter(plant_fun_type != "total")
#unique(jrnpvar$plant_fun_type)

### Add ExpYear
df <- data.frame(year= 2009:2018,
                 ExpYear= 1:10)
jrnpvar <- merge.data.frame(jrnpvar, df, by="year" )

## Clean up data for JRN precip variability

jrn_pvar_clean <- jrnpvar %>%
  tbl_df() %>%
  ## format column names to match the rest of the datasets
  dplyr::mutate(site = "jrn",
                project = "pvar",
                field = treat,  # variability TRT? 
                year = year,
                expyear = ExpYear,
                plot = plot, 
                subplot = "1",
                abundance = anpp, 
                carbon = "0",
                nadd = "0",
                ncess = "0",
                fence = "1",  # ask Lau if fenced
                burn = "0", # ask Lau 
                rainfall = rec_ppt, 
                amb_rain = ambient_ppt, 
                warm = "0",
                unitAbund = "biomass_g_m2",
                scaleAbund = "0.3m2",  # double check metadata
                species = plant_fun_type,
                uniqueID = paste(site, field, project, plot, subplot, sep = "_")) %>%
  dplyr::select(year, site, field, project, plot, subplot, uniqueID, 
                carbon, nadd, ncess, fence, burn, rainfall, amb_rain, warm,
                species, abundance, unitAbund, scaleAbund) 

rm(df, jrn_pvar, jrnpvar)
