
# Towards Justice in Blue Foods

This repository houses the script and data necessary to run the quantitative aspects of the paper. This project is a part of the Blue Food Assessment. 

Description of contents of the subdirectories:
* data_cleaning - includes all the scripts that clean the raw data. Raw data is not included in the repository due to size. 
* model - Scripts associated with the Bayesian models
* model_outputs - Stores the outputs of the Bayesian models
* fig_2 - Script to produce Fig. 2 (Global distribution of benefits from aquatic foods)
* fig_3 - Script to produce Fig. 3 (Recognition of barriers to participation in national policy documents)
* fig_4 - Script to produce Fig. 4 (Bayesian hierarchical models establishing how economic, social, and political barriers are associated with distributions of aquatic food benefits)

The repository also includes some data files used in the analysis: 
* all_national_indicators.csv - All data pulled into the Bayesian model scripts in the model folder. This includes all variables considered, but not all variables are used in the model. 
* bayesian_model_variables.csv - Only includes the data (cleaned and transformed) that are in the predictor, nuisance and response (denoted by _response) variables.
* meow_climatic_zone.csv - Climatic data used in the model folder. 
* requirements.txt - Text file used by Python-based scripts in the model folder. 
