## ---------------------------
##
## Script name: LTER-transitions-quanteda.R
##
## Purpose of script: A script that uses the text mining package, quanteda, to determine which papers to include in our analysis based on its abstract. This script has the same purpose as LTER-transitions-abstracts.R but accomplishes it in an alternative way.
##
## Authors: Angel Chen
##
## Emails: anchen@nceas.ucsb.edu
##
## ---------------------------

# loading packages
# install.packages("librarian")
librarian::shelf(tidyverse, readr, readxl, googledrive, purrr, quanteda, readtext, quanteda.textmodels, caret)

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

# read in the google sheets where the PIs have decided whether to include the papers or not
all_sheets <- xl_names %>%
  purrr::map(.f = read_excel) %>%
  purrr::map_dfr(.f = select, DOI, ArticleTitle, Abstract, Include) %>%
  dplyr::mutate(DOI = ifelse(test = is.na(DOI),
                             yes = "no doi",
                             no = DOI)) %>%
  tidyr::drop_na(Include)

# construct a corpus 
corp <- corpus(all_sheets, text_field = "Abstract")

# view our corpus
print(corp)
summary(corp, 5)

# generate 432 numbers without replacement (the rows with these numbers will become part of our training set)
set.seed(300)
id_train <- sample(1:721, 432, replace = FALSE)
head(id_train, 10)

# create docvar with ID
corp$id_numeric <- 1:ndoc(corp)

# tokenize texts
toks <- tokens(corp, remove_punct = TRUE, remove_number = TRUE) %>% 
  tokens_tolower() %>%
  tokens_remove(pattern = stopwords("en")) # if you also want to stem the words, then use tokens_wordstem()

# construct a sparse document-feature matrix
dfmt <- dfm(toks)

# get training set
dfmat_training <- dfm_subset(dfmt, id_numeric %in% id_train)

# get test set (documents not in id_train)
dfmat_test <- dfm_subset(dfmt, !id_numeric %in% id_train)

# train the naive Bayes classifier
tmod_nb <- textmodel_nb(dfmat_training, dfmat_training$Include)
summary(tmod_nb)

# naive Bayes can only take features into consideration that occur both in the training set and the test set, but we can make the features identical using dfm_match()
dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))

# actual answers from the PIs
actual_class <- dfmat_matched$Include

# our predictions from the naive Bayes model
predicted_class <- predict(tmod_nb, newdata = dfmat_matched)
predicted_prob <- predict(tmod_nb, newdata = dfmat_matched, type = "prob") # the predicted probabilities

# inspecting how well we did with a confusion matrix
# accuracy is around 75%
tab_class <- table(actual_class, predicted_class)
tab_class
confusionMatrix(tab_class, mode = "everything", positive = "Yes")

# how strongly is each word associated with a "Yes" or "No" decision?
coeff <- coef(tmod_nb) %>% 
  as.data.frame() %>%
  tibble::rownames_to_column(var = "word")

View(coeff %>% arrange(desc(No)))
View(coeff %>% arrange(desc(Yes)))


## ---------------------------
##
##
## What happens if, instead of tokenizing every word, we only keep track of certain keywords in the abstract? 
##
##
## ---------------------------


# tokenize texts
toks <- tokens(corp, remove_punct = TRUE, remove_number = TRUE) %>% 
  tokens_tolower() %>%
  tokens_remove(pattern = stopwords("en")) 

# construct a sparse document-feature matrix
dfmt <- dfm(toks)

# construct a dictionary with the positive and negative keywords 
my_dict <- dictionary(list(negative = c("meta analysis", "modelling", "modeling", "review", "aquatic", "incubation", "greenhouse", "marine", "river", "coral"),
                           positive = c("experiment", "observation", "not aquatic", "not greenhouse", "not review", "not river")))

# examine what happens when we apply our dictionary
dfm_lookup(dfmt, dictionary = my_dict)

# subset our matrix with the dictionary
dfmt <- dfm_select(dfmt, pattern = my_dict, selection = "keep")

# get training set
dfmat_training <- dfm_subset(dfmt, id_numeric %in% id_train)

# get test set (documents not in id_train)
dfmat_test <- dfm_subset(dfmt, !id_numeric %in% id_train)

# train the naive Bayes classifier
tmod_nb2 <- textmodel_nb(dfmat_training, dfmat_training$Include)
summary(tmod_nb2)

# naive Bayes can only take features into consideration that occur both in the training set and the test set, but we can make the features identical using dfm_match()
dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))

# actual answers from the PIs
actual_class <- dfmat_matched$Include

# our predictions from the naive Bayes model
predicted_class <- predict(tmod_nb2, newdata = dfmat_matched)
predicted_prob <- predict(tmod_nb2, newdata = dfmat_matched, type = "prob") # the predicted probabilities

# inspecting how well we did with a confusion matrix
# accuracy is around 67%
tab_class <- table(actual_class, predicted_class)
tab_class
confusionMatrix(tab_class, mode = "everything", positive = "Yes")
