## File explanation

### LTER-transitions-abstracts.R
A script that counts the number of times each positive/negative keyword appears in the abstract and tries to determine which papers to include in our analysis based on those keywords.

### top_10_positive.csv
The top 10 papers with the most positive keywords in its title and abstract. This csv is created from LTER-transitions-abstracts.R.

### top_10_negative.csv
The top 10 papers with the most negative keywords in its title and abstract. This csv is created from LTER-transitions-abstracts.R.

For the csv files, the columns for the keyword counts have `_abstract` or `_title` appended at the end so you can know if that keyword was found in the paper's abstract or title. For example, `experiment_abstract` contains the number of times the word 'experiment' shows up in the paper's abstract.

Some other variables:

- `total_positive_count_abstract`: the number of times any positive keyword shows up in a paper's abstract

- `total_positive_count_title`: the number of times any positive keyword shows up in a paper's title

- `total_positive_count`: `total_positive_count_abstract` + `total_positive_count_title`

- `total_negative_count_abstract`: the number of times any negative keyword shows up in a paper's abstract

- `total_negative_count_title`: the number of times any negative keyword shows up in a paper's title

- `total_negative_count`: `total_negative_count_abstract` + `total_negative_count_title`

- `final_score`: `total_positive_count` - `total_negative_count`

- `Evaluator`: who was assigned to read the paper 

- `Include`: the `Evaluator`'s decision on whether to include the paper in the analysis or not

### LTER-transitions-quanteda.R
A script that uses the text mining package, quanteda, to determine which papers to include in our analysis based on its abstract. This script has the same purpose as LTER-transitions-abstracts.R but accomplishes it in an alternative way. 
