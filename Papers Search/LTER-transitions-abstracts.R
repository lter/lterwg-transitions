## ---------------------------
##
## Script name: LTER-transitions-abstracts.R
##
## Purpose of script: A script that counts the number of times each positive/negative keyword appears in the abstract and tries to determine which papers to include in our analysis based on those keywords
##
## Author: Angel Chen
##
## Email: anchen@nceas.ucsb.edu
##
## ---------------------------

# loading packages
# install.packages("librarian")
librarian::shelf(tidyverse, readr, readxl, googledrive, randomForest)

# download files from google drive
folder_url<- "https://drive.google.com/drive/folders/1LxFV4HRhiPGvCKJD7TpenAPVlXob7P7U" #this is for all data folder

# identify this folder on Drive
# let googledrive know this is a file ID or URL, as opposed to file name
folder <- drive_get(as_id(folder_url), shared_drive = "RawSearchResults")

# identify the csv files in that folder
csv_files <- drive_ls(folder, type = "csv")

# download them
walk(csv_files$id, ~ drive_download(as_id(.x), overwrite=TRUE))

# read in the csv files
papers1 <- read_csv("Review_FEB232022_to_1K.csv") %>% 
  select(DOI, ArticleTitle, Abstract)

papers2 <- read_csv("Review_FEB232022_to_2K.csv") %>% 
  select(DOI, ArticleTitle, Abstract)

papers3 <- read_csv("Review_FEB232022_to_3K.csv") %>% 
  select(DOI, ArticleTitle, Abstract)

papers4 <- read_csv("Review_FEB232022_to_4K.csv") %>% 
  select(DOI, ArticleTitle, Abstract)

# combine them altogether and select the columns of interest
all_papers <- bind_rows(papers1, papers2, papers3, papers4) %>%
  mutate(DOI = ifelse(is.na(DOI), "no doi", DOI))

# first filter out the papers with the positive keywords
# then extract the positive and negative keywords from the abstract and make them into 2 new columns
extracted_papers <- all_papers %>% 
  mutate(positive_keywords = str_extract_all(Abstract, "[Ee]xperiment|[Oo]bservation"),  #includes words such as "experimental" and "observations"
         negative_keywords = str_extract_all(Abstract, "[Mm]eta-analysis|[Mm]eta analysis|[Mm]odeling|[Mm]odelling|[Rr]eview|[Aa]quatic|[Ii]ncubation|[Gg]reenhouse|[Mm]arine|[Rr]iver|[Cc]oral")) 

# combine the extracted positive keywords into one string for each paper
for (i in 1:length(extracted_papers$positive_keywords)){
  if(length(extracted_papers$positive_keywords[[i]]) > 1){
    keyword <- (str_c(extracted_papers$positive_keywords[[i]], collapse = ";"))
    extracted_papers$positive_keywords[[i]] <- keyword
  }
}

# combine the extracted negative keywords into one string for each paper
for (i in 1:length(extracted_papers$negative_keywords)){
  if(length(extracted_papers$negative_keywords[[i]]) > 1){
    keyword <- (str_c(extracted_papers$negative_keywords[[i]], collapse = ";"))
    extracted_papers$negative_keywords[[i]] <- keyword
  }
}

# replacing all the incidences of "character(0)" in the positive_keywords and negative_keywords columns with NA values
extracted_papers <- extracted_papers %>%
  mutate(negative_keywords = ifelse(negative_keywords == "character(0)", NA_character_, negative_keywords),
         positive_keywords = ifelse(positive_keywords == "character(0)", NA_character_, positive_keywords)) 

# make a column for every positive keyword, counting how many times each positive keyword appeared in the abstract
# make one last column counting the total amount of times a positive keyword has appeared in the abstract
extracted_positive <- extracted_papers %>%  
  select(-negative_keywords) %>%
  separate_rows(positive_keywords, sep = ";") %>%
  mutate(positive_keywords = tolower(positive_keywords)) %>%
  count(DOI, ArticleTitle, Abstract, positive_keywords) %>%
  rename(positive_count = n) %>%
  pivot_wider(names_from = positive_keywords, values_from = positive_count, values_fill = 0) %>%
  mutate(total_positive_count = experiment+observation) %>%
  select(-`NA`)

# make a column for every negative keyword, counting how many times each negative keyword appeared in the abstract
# make one last column counting the total amount of times a negative keyword has appeared in the abstract
# also rename the meta-analysis column to meta_analysis
extracted_negative <- extracted_papers %>%
  select(-positive_keywords) %>%
  separate_rows(negative_keywords, sep = ";") %>%
  mutate(negative_keywords = tolower(negative_keywords)) %>%
  count(DOI, ArticleTitle, Abstract, negative_keywords) %>%
  rename(negative_count = n) %>%
  pivot_wider(names_from = negative_keywords, values_from = negative_count, values_fill = 0) %>%
  mutate(total_negative_count = modeling + river + marine + review + modelling + aquatic + `meta-analysis` + coral + incubation + greenhouse) %>%
  rename(meta_analysis = `meta-analysis`) %>%
  select(-`NA`) 


# combining our results together to get a dataframe that has the total count of positive and negative keywords for each paper's abstract
# make one last column called "final_score" which is the amount of positive keywords minus the amount of negative keywords in an abstract
# papers with a higher final_score have more positive keywords than negative ones
result <- left_join(extracted_positive, extracted_negative, by = c("DOI", "ArticleTitle", "Abstract")) %>%
  mutate(final_score = total_positive_count - total_negative_count) %>%
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

# download them
walk(sheets$id, ~ drive_download(as_id(.x), overwrite=TRUE))

# read in the google sheets where the PIs have decided whether to include the papers or not
AnnyApril <- read_excel("AnnyApril.xlsx") %>% 
  select(DOI, ArticleTitle, Abstract, Evaluator, Include) %>% 
  filter(!is.na(Include))

BeatrizApril <- read_excel("BeatrizApril.xlsx") %>% 
  select(DOI, ArticleTitle, Abstract, Evaluator, Include) %>% 
  filter(!is.na(Include))

CarmenApril <- read_excel("CarmenApril.xlsx") %>% 
  select(DOI, ArticleTitle, Abstract, Evaluator, Include) %>% 
  filter(!is.na(Include))

CristyApril <- read_excel("CristyApril.xlsx") %>% 
  select(DOI, ArticleTitle, Abstract, Evaluator, Include) %>% 
  filter(!is.na(Include))

JennApril <- read_excel("JennApril.xlsx") %>% 
  select(DOI, ArticleTitle, Abstract, Evaluator, Include) %>% 
  filter(!is.na(Include))

LauApril <- read_excel("LauApril.xlsx") %>% 
  select(DOI, ArticleTitle, Abstract, Evaluator, Include) %>% 
  filter(!is.na(Include))

LukasApril <- read_excel("LukasApril.xlsx") %>% 
  select(DOI, ArticleTitle, Abstract, Evaluator, Include) %>% 
  filter(!is.na(Include))

# combine the sheets altogether 
all_sheets <- bind_rows(AnnyApril, BeatrizApril, CarmenApril, CristyApril, JennApril, LauApril, LukasApril)  %>%
  mutate(DOI = ifelse(is.na(DOI), "no doi", DOI))

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
# we have 16 predictors so we use the square root of 16 as the number of variables to use for splitting
set.seed(1)
forest_fit <- randomForest(Include~., data=train, mtry=sqrt(16), importance=TRUE)

# making our predictions on the test set
forest_pred <- predict(forest_fit, newdata = test)

# test error rate for random forest
# turns out that this model classifies incorrectly around 30% of the time
table(forest_pred, truth = test$Include)
mean(forest_pred != test$Include)

# plotting the top 5 most important predictors in terms of model accuracy and the Gini index
# the amount of times "review" appears in the abstract seems to greatly influence whether we include the paper
varImpPlot(forest_fit, sort = TRUE, main = "Variable Importance for forest_fit", n.var=5)

# In conclusion: since our misclassification rate is so high, we can improve our model accuracy by considering more keywords 
