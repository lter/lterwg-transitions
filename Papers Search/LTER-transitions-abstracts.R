## ---------------------------
##
## Script name: LTER-transitions-abstracts.R
##
## Purpose of script: A script that counts the number of times each positive/negative keyword appears in the abstract and tries to determine which papers to include in our analysis based on those keywords
##
## Authors: Angel Chen & Nick Lyon
##
## Emails: anchen@nceas.ucsb.edu, lyon@nceas.ucsb.edu
##
## ---------------------------

# loading packages
# install.packages("librarian")
librarian::shelf(tidyverse, readr, readxl, googledrive, randomForest, purrr)

# download files from google drive
folder_url<- "https://drive.google.com/drive/folders/1LxFV4HRhiPGvCKJD7TpenAPVlXob7P7U" #this is for all data folder

# identify this folder on Drive
# let googledrive know this is a file ID or URL, as opposed to file name
folder <- drive_get(as_id(folder_url), shared_drive = "RawSearchResults")

# identify the csv files in that folder
csv_files <- drive_ls(folder, type = "csv")

# Create a folder to locally save to
dir.create("Search Results")

# download them
for(csv_id in csv_files$id){
  # Identify file name
  csv_name <- dplyr::filter(csv_files, id == csv_id)$name
  # Download it locally
  drive_download(file = as_id(csv_id), path = file.path("Search Results", csv_name), overwrite=TRUE)
}
# walk(csv_files$id, ~ drive_download(as_id(.x), overwrite=TRUE))

# read in the csv files
## Identify paper names
csv_names <- dir(file.path("Search Results"))
csv_names

# Read in CSVs
papers <- file.path("Search Results", csv_names) %>%
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
  mutate(positive_keywords_abstract = str_extract_all(Abstract, "[Ee]xperiment|[Oo]bservation"),  #includes words such as "experimental" and "observations"
         negative_keywords_abstract = str_extract_all(Abstract, "[Mm]eta-analysis|[Mm]eta analysis|[Mm]odeling|[Mm]odelling|[Rr]eview|[Aa]quatic|[Ii]ncubation|[Gg]reenhouse|[Mm]arine|(?<![Dd])[Rr]iver|[Cc]oral"), # I have (?<![Dd])[Rr]iver so str_detect doesn't confuse "driver" with "river"
         positive_keywords_title = str_extract_all(ArticleTitle, "[Ee]xperiment|[Oo]bservation"),
         negative_keywords_title = str_extract_all(ArticleTitle, "[Mm]eta-analysis|[Mm]eta analysis|[Mm]odeling|[Mm]odelling|[Rr]eview|[Aa]quatic|[Ii]ncubation|[Gg]reenhouse|[Mm]arine|(?<![Dd])[Rr]iver|[Cc]oral")) 

# combine the extracted positive keywords into one string for each paper's abstract
for (i in 1:length(extracted_papers$positive_keywords_abstract)){
  if(length(extracted_papers$positive_keywords_abstract[[i]]) > 1){
    keyword <- (str_c(extracted_papers$positive_keywords_abstract[[i]], collapse = ";"))
    extracted_papers$positive_keywords_abstract[[i]] <- keyword
  }
}

# combine the extracted negative keywords into one string for each paper's abstract
for (i in 1:length(extracted_papers$negative_keywords_abstract)){
  if(length(extracted_papers$negative_keywords_abstract[[i]]) > 1){
    keyword <- (str_c(extracted_papers$negative_keywords_abstract[[i]], collapse = ";"))
    extracted_papers$negative_keywords_abstract[[i]] <- keyword
  }
}

# combine the extracted positive keywords into one string for each paper's title
for (i in 1:length(extracted_papers$positive_keywords_title)){
  if(length(extracted_papers$positive_keywords_title[[i]]) > 1){
    keyword <- (str_c(extracted_papers$positive_keywords_title[[i]], collapse = ";"))
    extracted_papers$positive_keywords_title[[i]] <- keyword
  }
}

# combine the extracted negative keywords into one string for each paper's title
for (i in 1:length(extracted_papers$negative_keywords_title)){
  if(length(extracted_papers$negative_keywords_title[[i]]) > 1){
    keyword <- (str_c(extracted_papers$negative_keywords_title[[i]], collapse = ";"))
    extracted_papers$negative_keywords_title[[i]] <- keyword
  }
}

# replacing all the incidences of "character(0)" in the positive_keywords and negative_keywords columns with NA values
extracted_papers <- extracted_papers %>%
  mutate(positive_keywords_abstract = ifelse(positive_keywords_abstract == "character(0)", NA_character_, positive_keywords_abstract),
         negative_keywords_abstract = ifelse(negative_keywords_abstract == "character(0)", NA_character_, negative_keywords_abstract),
         positive_keywords_title = ifelse(positive_keywords_title == "character(0)", NA_character_, positive_keywords_title),
         negative_keywords_title = ifelse(negative_keywords_title == "character(0)", NA_character_, negative_keywords_title)) 

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


## ---------------------------
##
## Can we use a machine learning algorithm to decide if a paper should be included or not?
## For the rest of this script, I will train a random forest model by using the PIs' actual decisions as our response variable and our positive/negative keyword counts as our predictor variables
##
## ---------------------------


# download files from google drive
folder_url<- "https://drive.google.com/drive/folders/1S6-Lcs45eTUi3XjtRSqtGjSdgx7YRmAi" #this is for all data folder

# identify this folder on Drive
# let googledrive know this is a file ID or URL, as opposed to file name
folder <- drive_get(as_id(folder_url), shared_drive = "Reading Assignments April")

# identify the spreadsheets in that folder
sheets <- drive_ls(folder, type = "spreadsheet")

# Create a local folder for them
dir.create("Reading Assignments", showWarnings = FALSE)

# download them
for(file_id in sheets$id){
  # Identify file name
  file_name <- dplyr::filter(sheets, id == file_id)$name
  # Download it locally
  drive_download(file = as_id(file_id), path = file.path("Reading Assignments", file_name), overwrite=TRUE)
}

# Identify Excel file names
xl_names <- dir(file.path("Reading Assignments"))
xl_names

# read in the google sheets where the PIs have decided whether to include the papers or not
all_sheets <- file.path("Reading Assignments", xl_names) %>%
  purrr::map(.f = read_excel) %>%
  purrr::map_dfr(.f = select, DOI, ArticleTitle, Abstract, Evaluator, Include) %>%
  dplyr::mutate(DOI = ifelse(test = is.na(DOI),
                             yes = "no doi",
                             no = DOI))

# left-join our result from earlier with the PIs' decisions
sanity_check <- left_join(result, all_sheets, by = c("DOI", "ArticleTitle", "Abstract")) %>%
  filter(!is.na(Include)) 

# plot our dataframe to see if there's a pattern on whether a PI decides to include a paper and the paper's total positive/negative keyword count
# notice that there seems to be no pattern 
sanity_check %>%
  ggplot(aes(x=total_positive_count, y=total_negative_count, color=Include)) + 
  geom_point(position = "jitter") +
  labs(title = "Total number of positive keywords vs Total number of negative keywords for each paper",
       color = "Do we include this paper in our analysis?") 

# prepping our dataframe for machine learning by making the categorical variables into factors
sanity_check$Include <- as.factor(sanity_check$Include)
sanity_check$Evaluator <- as.factor(sanity_check$Evaluator)

# keeping only our predictor variables and our labels
sanity_check <- sanity_check %>%
  select(-DOI, -Abstract, -ArticleTitle) 

# splitting into training and test sets
# train set: 60% of the data, test set: the remaining 40%
set.seed(1)
RNGkind(sample.kind="Rejection")
sample1 <- sample(1:nrow(sanity_check), 0.6*nrow(sanity_check)) 
train <- sanity_check[sample1,] 
test <- sanity_check[-sample1,]

# training a random forest on our dataframe
# we have 32 predictors so we use the square root of 32 as the number of variables to use for splitting
set.seed(1)
forest_fit <- randomForest(Include~., data=train, mtry=sqrt(32), importance=TRUE)

# making our predictions on the test set
forest_pred <- predict(forest_fit, newdata = test)

# test error rate for random forest
# turns out that this model classifies incorrectly around 30% of the time
# most of the misclassification comes from predicting a paper as "no, do not include" when in reality, it is actually "yes, include"
table(forest_pred, truth = test$Include)
mean(forest_pred != test$Include)

# plotting the top 5 most important predictors in terms of model accuracy and the Gini index
# the amount of times "review" appears in the abstract seems to greatly influence whether we include the paper
varImpPlot(forest_fit, sort = TRUE, main = "Variable Importance for forest_fit", n.var=5)

# In conclusion: since our misclassification rate is so high, we can improve our model accuracy by considering more keywords 

# get a clean copy of sanity_check with all of its original columns again
sanity_check <- left_join(result, all_sheets, by = c("DOI", "ArticleTitle", "Abstract")) %>%
  filter(!is.na(Include)) 

# find the top 10 papers with the highest number of positive keywords
top_10_positive <- result %>% 
  slice_max(total_positive_count, n = 10) %>% 
  left_join(sanity_check)

# find the top 10 papers with the highest number of negative keywords
top_10_negative <- result %>% 
  slice_max(total_negative_count, n = 10) %>% 
  left_join(sanity_check)

# export
my_path <- file.path("Papers Search")
write.csv(x = top_10_positive, file = file.path(my_path, "top_10_positive.csv"))
write.csv(x = top_10_negative, file = file.path(my_path, "top_10_negative.csv"))
