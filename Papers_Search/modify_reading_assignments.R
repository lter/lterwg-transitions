## ---------------------------
##
## Script name: modify_reading_assignments.R
##
## Purpose of script: A script that adds a column called "total_negative_count" to each group member's April reading assignment in Google Sheets
##
## Authors: Angel Chen 
##
## Emails: anchen@nceas.ucsb.edu
##
## ---------------------------

## ----------------------------------------------------------
## first we create the dataframe with "total_negative_count 
## ----------------------------------------------------------

# loading packages
# install.packages("librarian")
librarian::shelf(tidyverse, readr, readxl, googledrive, googlesheets4)


# Set the path to download the abstracts
search_path <- "Papers_Search"
abstracts_folder <- file.path(search_path, "Search_sesults")
assignements_folder <- file.path(search_path, "Reading_assignments")


# download files from google drive
folder_url<- "https://drive.google.com/drive/folders/1LxFV4HRhiPGvCKJD7TpenAPVlXob7P7U" #this is for all data folder

# identify this folder on Drive
# let googledrive know this is a file ID or URL, as opposed to file name
folder <- drive_get(as_id(folder_url), shared_drive = "RawSearchResults")

# identify the csv files in that folder
csv_files <- drive_ls(folder, type = "csv")

# Create a folder to locally save to
dir.create(abstracts_folder)

# download them
for(csv_id in csv_files$id){
  # Identify file name
  csv_name <- dplyr::filter(csv_files, id == csv_id)$name
  # Download it locally
  drive_download(file = as_id(csv_id), path = file.path(abstracts_folder, csv_name), overwrite=TRUE)
}
# walk(csv_files$id, ~ drive_download(as_id(.x), overwrite=TRUE))

# read in the csv files
## Identify paper names
csv_names <- dir(abstracts_folder, full.names = TRUE)
csv_names

# Read in CSVs as one data frame
papers <- csv_names %>%
  ## Actual reading step
  purrr::map(.f = readr::read_csv) %>%
  ## Then pare down to only needed columns
  purrr::map_dfr(select, DOI, ArticleTitle, Abstract) %>%
  ## Then fix missing DOIs
  dplyr::mutate(DOI = ifelse(test = is.na(DOI),
                             yes = "no doi",
                             no = DOI))

# first filter out the papers with the positive keywords
# then extract the positive and negative keywords from the abstract and make them into 2 new columns
extracted_papers <- papers %>% 
  mutate(positive_keywords_abstract = str_extract_all(Abstract, "[Ee]xperiment|[Oo]bservation") %>%
           map_chr( ~ paste0(tolower(.x), collapse = ";")),  #includes words such as "experimental" and "observations"
         negative_keywords_abstract = str_extract_all(Abstract, "[Mm]eta-analysis|[Mm]eta analysis|[Mm]odeling|[Mm]odelling|[Rr]eview|[Aa]quatic|[Ii]ncubation|[Gg]reenhouse|[Mm]arine|(?<![Dd])[Rr]iver|[Cc]oral") %>%
           map_chr( ~ paste0(tolower(.x), collapse = ";")), # I have (?<![Dd])[Rr]iver so str_detect doesn't confuse "driver" with "river"
         positive_keywords_title = str_extract_all(ArticleTitle, "[Ee]xperiment|[Oo]bservation") %>% 
           map_chr( ~ paste0(tolower(.x), collapse = ";")),
         negative_keywords_title = str_extract_all(ArticleTitle, "[Mm]eta-analysis|[Mm]eta analysis|[Mm]odeling|[Mm]odelling|[Rr]eview|[Aa]quatic|[Ii]ncubation|[Gg]reenhouse|[Mm]arine|(?<![Dd])[Rr]iver|[Cc]oral") %>%
           map_chr( ~ paste0(tolower(.x), collapse = ";"))
  ) %>%
  mutate_all(list(~ na_if(.,""))) %>% # Set empty strings to NAs
  mutate_all(list(~ na_if(.,"NA")))  # Returned when abstract was NA 


# make a column for every positive keyword, counting how many times each positive keyword appeared in the abstract
# make one last column counting the total amount of times a positive keyword has appeared in the abstract
extracted_positive_abstract <- extracted_papers %>%  
  dplyr::select(-negative_keywords_abstract, -negative_keywords_title, -positive_keywords_title) %>%
  tidyr::separate_rows(positive_keywords_abstract, sep = ";") %>%
  dplyr::mutate(positive_keywords_abstract = tolower(positive_keywords_abstract)) %>%
  dplyr::count(DOI, ArticleTitle, Abstract, positive_keywords_abstract) %>%
  dplyr::rename(positive_count = n) %>%
  tidyr::pivot_wider(names_from = positive_keywords_abstract,
                     values_from = positive_count,
                     values_fill = 0) %>%
  dplyr::mutate(total_positive_count = experiment+observation) %>%
  dplyr::select(-`NA`) %>%
  dplyr::rename_at(4:6, ~ paste0(.x, "_abstract"))

# make a column for every negative keyword, counting how many times each negative keyword appeared in the abstract
# make one last column counting the total amount of times a negative keyword has appeared in the abstract
# also rename the meta-analysis column to meta_analysis
extracted_negative_abstract <- extracted_papers %>%
  dplyr::select(-positive_keywords_abstract, -negative_keywords_title, -positive_keywords_title) %>%
  tidyr::separate_rows(negative_keywords_abstract, sep = ";") %>%
  dplyr::mutate(negative_keywords_abstract = tolower(negative_keywords_abstract)) %>%
  dplyr::count(DOI, ArticleTitle, Abstract, negative_keywords_abstract) %>%
  dplyr::rename(negative_count = n) %>%
  tidyr::pivot_wider(names_from = negative_keywords_abstract, values_from = negative_count, values_fill = 0) %>%
  dplyr::mutate(total_negative_count = modeling + river + marine + review + modelling + aquatic + `meta-analysis` + coral + incubation + greenhouse) %>%
  dplyr::rename(meta_analysis = `meta-analysis`) %>%
  dplyr::select(-`NA`) %>%
  dplyr::rename_at(4:14, ~ paste0(.x, "_abstract"))

# make a column for every positive keyword, counting how many times each positive keyword appeared in the title
# make one last column counting the total amount of times a positive keyword has appeared in the title
extracted_positive_title <- extracted_papers %>%  
  dplyr::select(-negative_keywords_abstract, -positive_keywords_abstract, -negative_keywords_title) %>%
  tidyr::separate_rows(positive_keywords_title, sep = ";") %>%
  dplyr::mutate(positive_keywords_title = tolower(positive_keywords_title)) %>%
  dplyr::count(DOI, ArticleTitle, Abstract, positive_keywords_title) %>%
  dplyr::rename(positive_count = n) %>%
  tidyr::pivot_wider(names_from = positive_keywords_title,
                     values_from = positive_count,
                     values_fill = 0) %>%
  dplyr::mutate(total_positive_count = experiment+observation) %>%
  dplyr::select(-`NA`) %>%
  dplyr::rename_at(4:6, ~ paste0(.x, "_title"))

# make a column for every negative keyword, counting how many times each negative keyword appeared in the title
# make one last column counting the total amount of times a negative keyword has appeared in the title
# also rename the meta-analysis column to meta_analysis
extracted_negative_title <- extracted_papers %>%
  dplyr::select(-positive_keywords_abstract, -negative_keywords_abstract, -positive_keywords_title) %>%
  tidyr::separate_rows(negative_keywords_title, sep = ";") %>%
  dplyr::mutate(negative_keywords_title = tolower(negative_keywords_title)) %>%
  dplyr::count(DOI, ArticleTitle, Abstract, negative_keywords_title) %>%
  dplyr::rename(negative_count = n) %>%
  tidyr::pivot_wider(names_from = negative_keywords_title, values_from = negative_count, values_fill = 0) %>%
  dplyr::mutate(total_negative_count = modeling + river + marine + review + modelling + aquatic + `meta-analysis` + coral + incubation + greenhouse) %>%
  dplyr::rename(meta_analysis = `meta-analysis`) %>%
  dplyr::select(-`NA`) %>%
  dplyr::rename_at(4:14, ~ paste0(.x, "_title"))

# combining our results together to get a dataframe that has the total count of positive and negative keywords for each paper's abstract & title
# make one last column called "final_score" which is the amount of positive keywords minus the amount of negative keywords 
# papers with a higher final_score have more positive keywords than negative ones in its abstract & title
result <- left_join(extracted_positive_abstract, extracted_positive_title, by = c("DOI", "ArticleTitle", "Abstract")) %>%
  left_join(extracted_negative_abstract, by = c("DOI", "ArticleTitle", "Abstract")) %>%
  left_join(extracted_negative_title, by = c("DOI", "ArticleTitle", "Abstract")) %>%
  mutate(total_positive_count = total_positive_count_abstract + total_positive_count_title,
         total_negative_count = total_negative_count_abstract + total_negative_count_title,
         final_score = total_positive_count - total_negative_count) %>%
  arrange(desc(final_score))

## ----------------------------------------------------------------
## next we add the column to each group member's reading assignment 
## ----------------------------------------------------------------

# download files from google drive
folder_url<- "https://drive.google.com/drive/folders/1S6-Lcs45eTUi3XjtRSqtGjSdgx7YRmAi" #this is for all data folder

# identify this folder on Drive
# let googledrive know this is a file ID or URL, as opposed to file name
folder <- drive_get(as_id(folder_url), shared_drive = "Reading Assignments April")

# identify the spreadsheets in that folder
sheets <- drive_ls(folder, type = "spreadsheet")

# Create a local folder for them
dir.create(assignements_folder, showWarnings = FALSE)

# download them
for(file_id in sheets$id){
  # Identify file name
  file_name <- dplyr::filter(sheets, id == file_id)$name
  # Download it locally
  drive_download(file = as_id(file_id), path = file.path(assignements_folder, file_name), overwrite=TRUE)
}

# Identify Excel file names
xl_names <- dir(assignements_folder, full.names = TRUE)
xl_names


for (xl_name in xl_names) {
  
  neg_count <- read_excel(xl_name) %>%
    select(ArticleTitle, Abstract) %>%
    left_join(result) %>%
    select(total_negative_count) 
  
  new_xl_name <- str_sub(string = xl_name, start = 35)
  new_xl_name2 <- str_replace(string = new_xl_name, ".xlsx", "")
  xl_id <- filter(sheets, name == new_xl_name2)$id
  
  xl_id %>% range_write(data = neg_count, range = "P")
}

