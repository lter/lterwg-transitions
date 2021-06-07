### CDR E002 Data Cleaning ###

## Data manipulation packages
library(tidyverse)
source("utilities/data_import.R")

## Data import
# 1Vbzs9U9yW1Og5YhKcZhTxi5J0LOwU3du
cdr_e002 <- read_csv_gdrive("1Vbzs9U9yW1Og5YhKcZhTxi5J0LOwU3du")%>%
  tbl_df()

## Working data frame
cdr2 <- cdr_e002

## For e002 the date is formated as sampling date rather than year
## Extract the year from the character string
cdr2$Date <- as.character(cdr2$Date)
len <- nchar(cdr2$Date)
a <- substr(cdr2$Date, len-1, len)
a <- if_else(a > 50, paste0(19, a), paste0(20, a))
cdr2$Year <- a

### Add ExpYear
df <- data.frame(Year= 1982:2018,
                 ExpYear= 1:37)
cdr2 <- merge.data.frame(cdr2, df, by="Year" )

### Cessation of N addition
# If "LastYearNtrt" < "Year" then NCess = 1
# NCess = 1  means N was previously added, but stopped
# NCess = 0  means N is being added if Nadd > 0 
# or NCess can be 0 if N was never added

ncess <- if_else(cdr2$LastYrNtrt < cdr2$Year, paste(1), paste(0))
cdr2$NCess <- ncess

rm(len, a, df, ncess)



## Clean up data e002

cdr_clean <- cdr2 %>%
  tbl_df() %>%
  dplyr::select(-MaxHeight, -MedHeight, -Date) %>%  # Remove unwanted columns
  tbl_df() %>%
  ## format column names to match the rest of the datasets
  dplyr::mutate(site = "cdr",
                project = ExpNum,
                field = Field,
                year = Year,
                expyear = ExpYear,
                plot = Plot, 
                subplot = Subplot,
                abundance = Biomass, 
                carbon = "0",
                nadd = Nadd,
                ncess = NCess,
                fence = FencingTrt,
                burn = BurnTrt,
                rainfall = "0", 
                warm = "0",
                unitAbund = "biomass_g_m2",
                scaleAbund = "0.3m2",
                species = Species,
                uniqueID = paste(site, field, project, plot, subplot, sep = "_")) %>%
  dplyr::select(year, site, field, project, plot, subplot, uniqueID, 
                carbon, nadd, ncess, fence, burn, rainfall, warm,
                species, abundance, unitAbund, scaleAbund) 

## Putting the two experiments in the same data frame
# cdr_clean <- rbind(cdr1_clean,cdr2_clean)

cdr_clean <- cdr_clean %>%
  filter(species != "Forb seedlings",
         species != "Fungi",
         species != "Lichens",
         species != "Grass seedlings",
         species != "Miscellaneous herbs",
         species != "Miscellaneous herb",
         species != "Miscellaneous forb",
         species != "Miscellaneous litter",
         species != "Miscellaneous grasses",
         species != "Miscellaneous grasses 2",
         species != "Miscellaneous sp.",
         species != "Miscellaneous woody",
         species != "Miscellaneous Woody",
         species != "Miscellaneous woody plants",
         species != "Miscellaneous legumes",
         species != "Miscellaneous sedges",
         species != "Misc. forb",
         species != "Mosses",
         species != "Mosses & lichens",
         species != "Mosses & lichens 2",
         species != "Mosses and",
         species != "Mosses and lichens",
         species != "Pine cones",
         species != "Pine needles",
         species != "ambrosia sp.")

rm(cdr2, cdr_e002)
