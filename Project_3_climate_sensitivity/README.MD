# Project 3 - Climate sensitivity functions
... what is the snappy acronym for this project. 

## Suggested workflow for testing CSF code on other datasets

 Markup : 1. Data download and cleaning
              1. The script to download and clean datasets that are already in our [raw data](https://drive.google.com/drive/folders/1I_RFbh_YkkYHapP7H0J3gXXkUf6-nmqL) folder can be found on our [github](https://github.com/lter/lterwg-transitions)
              2. If I remember correctly, Forest/Cristy volunteered to try a fertilization experiment from CDR, and Joan volunteered for the fertilization experiment from NWT
			  3. Once the raw data passes through the cleaning script, it should be in a harmonized format ready for the CSF analysis
          2. Adding climate data frame
				1. In order to test climate sensitivity, we need to add climate data. This is where your expertise and knowledge of your site is helpful!
				2. Based on your best knowledge for climate data available for your site, make a data frame that has two columns, one with the time variable that corresponds to the plant measurements in the harmonized data (e.g. Year or Season), and the other column with the corresponding climate variable.
				3. The climate variable can be SPEI or just annual/seasonal precipitation
			3. Run the code to produce climate sensitivity functions, focusing on whether best fit functions contain nonlinearities
				1. Once you have the harmonized data frame and the climate data frame, you should be ready to try the analysis code in this repository or in [google drive](https://drive.google.com/drive/folders/1xkxWpWA9NEeEoUmZfkRMsS9s7zodo7xU) (the sev_nfert_csf.R file).
				2. After a few housekeeping/formatting lines, the bulk of the code focuses on using multi-model inference/model selection to determine whether there are nonlinearities in the relationship between climate and plant responses, and if this relationship depends on the fertilization treatment. An overview of the concept can be found [here](https://docs.google.com/presentation/d/1-iBTZRcyEPnQHSTVroXuyDqe5uCFJc-B/edit#slide=id.p2) and [here](https://docs.google.com/presentation/d/1sXETVr4Y3SNf9QIf_6yzv7fK70Q4oousnWkJ2L-gYLo/edit#slide=id.g1071594a51d_0_50). And in [this publication](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.2136).
				3. Please adjust modeling strategy to suit data from your site:
					1. Acceptable climate predictors: SPEI, total precip, etc.
					2. Acceptable plant responses: total cover, NPP, etc.
				4. In the SEV N fertilization experiment, the predictor climate variable is SPEI and the response variable is total plant cover. SEV data are collected twice each year so these are at the seasonal scale. The example input files from SEV can be found [here](https://drive.google.com/drive/folders/1xkxWpWA9NEeEoUmZfkRMsS9s7zodo7xU). 
				5. Currently, the code uses visreg() as the visualization strategy. When Lukas and Beatriz tested, that sometimes breaks depending on the version of R. There’s a chunk of alternative visualization code in the script that may help.
			4. Outputs: If you successfully made it this far (hooray!) for your site, I would love to know…
				1. Best fit model: linear, quadratic, or cubic? Are there significant interactions with the fertilization treatment?
				2. Output a visualization of the fitted climate sensitivity function.
				3. What did you end up doing for climate data? Do you have suggestions for harmonizing the climate data step if we expand to more sites?
					






