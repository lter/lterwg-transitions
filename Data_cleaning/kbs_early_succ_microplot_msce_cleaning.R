### KBS Early Succession Data Cleaning ###
###         MCSE         ###

## Data manipulation packages
library(tidyverse)
library(googledrive)

#Create directory for file download 
# I put this directory in to the .gitignore so it does not affect the repository when changes are pushed
dir.create("raw_data_GDrive", showWarnings = F)

#Create a tibble with all of the files in the LTER transitions directory
#Don't need to change this since this is where all of the raw files are located
files_ls=drive_ls(as_id("https://drive.google.com/drive/folders/1I_RFbh_YkkYHapP7H0J3gXXkUf6-nmqL"))

#Download the dataset based on the file name in the directory
drive_download(file = subset(files_ls,name=="KBS_early_successional_microplot_split_spp_2019.csv"),
               path = "raw_data_GDrive/KBS_early_successional_microplot_split_spp_2019.csv")


#load data from the directory

KBS_split_spp=read.table("raw_data_GDrive/KBS_early_successional_microplot_split_spp_2019.csv", sep=",",header = T)



## Working data frame
KBS_spp <- KBS_split_spp



## For the date is formatted as sampling date rather than year
## Extract the year from the character string

KBS_spp$Year <- format(as.Date(KBS_spp$sample_date),format = "%Y")

KBS_spp$month <- format(as.Date(KBS_spp$sample_date),format = "%m")


### Add ExpYear
df <- data.frame(Year= 1989:2014,
                  ExpYear= 1:26)
KBS_spp <- merge.data.frame(KBS_spp, df, by="Year" )

rm( df)

### Originally there were two sampling dates per year
### We will only use the sampling at the end of the summer
### Also let's remove the disturbed plots
head(KBS_spp)
KBS_spp<-KBS_spp|>
  group_by(Year,treatment,replicate,fertilized)|>
  top_n(1,as.Date(sample_date))|>
  filter(disturbed=="undisturbed")




## Clean up data kbs early succession

KBS_spp_clean <- KBS_spp %>%
  tbl_df() %>%
  dplyr::select(-method, -disturbed,-sample_date,-month) %>%  # Remove unwanted columns
  tibble::as_tibble() %>%
  ## format column names to match the rest of the datasets
  dplyr::mutate(site = "kbs",
                project = "MCSE_early_succ",
                field = treatment, 
                year = Year,
                expyear = ExpYear,
                plot = replicate, 
                subplot = "1",
                abundance = biomass_g_m2, 
                carbon = "0",
                nadd = fertilized,
                ncess = "0",
                fence = "0",
                burn = "0",
                rainfall = "0", 
                warm = "0",
                unitAbund = "biomass_g_m2",
                scaleAbund = area_sampled, 
                species = species,
                uniqueID = paste(site, field, project, plot, subplot, sep = "_")) %>%
  dplyr::select(year, site, field, project, plot, subplot, uniqueID, 
                carbon, nadd, ncess, fence, burn, rainfall, warm,
                species, abundance, unitAbund, scaleAbund) 

## Check treatment



rm(KBS_spp)

