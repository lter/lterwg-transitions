### CDR E141 Data Cleaning ###
###         BioCON         ###

## Data manipulation packages
library(tidyverse)
source("utilities/data_import.R")

## Data import
cdr_e141 <- read_csv_gdrive("11fzmm-4ciuUi7ML9lEtaZPjw58VS_0vc")%>%
  tbl_df()

## Working data frame
cdr141 <- cdr_e141

## For e141 the date is formated as sampling date rather than year
## Extract the year from the character string
cdr141$Date <- as.character(cdr141$Date)
len <- nchar(cdr141$Date)
a <- substr(cdr141$Date, len-1, len)
a <- if_else(a > 50, paste0(19, a), paste0(20, a))
cdr141$Year <- a

b <- substr(cdr141$Date, 1, 1)
cdr141$month <- b

### Add ExpYear
df <- data.frame(Year= 1998:2018,
                  ExpYear= 1:21)
cdr141 <- merge.data.frame(cdr141, df, by="Year" )

rm(len, a, df, b)

### Originally there were two sampling dates per year
### We will only use the sampling at the end of the summer
### Meaning month 8
### will also remove empty plots and litter 
cdr141 <- cdr141 %>%
  tbl_df() %>%
  filter(Species != "Miscellaneous litter") %>%
  filter(CountOfSpecies != "0") %>%
  filter(month != 6)

#unique(cdr141$month)



## Clean up data e141

cdr_141_clean <- cdr141 %>%
  tbl_df() %>%
  dplyr::select(-Experiment, -monospecies, -Monogroup, -Date) %>%  # Remove unwanted columns
  tbl_df() %>%
  ## format column names to match the rest of the datasets
  dplyr::mutate(site = "cdr",
                project = "141",
                field = CountOfSpecies, ## Planted richness
                year = Year,
                expyear = ExpYear,
                plot = Ring, 
                subplot = Plot,
                abundance = Aboveground_Biomass, 
                carbon = CO2_Treatment,
                nadd = Nitrogen_Treatment,
                ncess = "0",
                fence = "0",
                burn = "1",
                rainfall = Water_Treatment, 
                warm = Temp_Treatment,
                unitAbund = "biomass_g_m2",
                scaleAbund = "0.1m2", ## Double check
                species = Species,
                uniqueID = paste(site, field, project, plot, subplot, sep = "_")) %>%
  dplyr::select(year, site, field, project, plot, subplot, uniqueID, 
                carbon, nadd, ncess, fence, burn, rainfall, warm,
                species, abundance, unitAbund, scaleAbund) 

## Check rainfall and temp trts
unique(cdr_141_clean$rainfall)
unique(cdr_141_clean$warm)

## We should decide on how to code these! 

rm(cdr_e141, cdr141)
