
# Rights and representation support justice across aquatic food systems

This repository houses the script and data necessary to run the quantitative aspects of the paper. This project is a part of the Blue Food Assessment.



Description of contents of the subdirectories:
* data_cleaning - includes all the scripts that clean the raw data. Raw data is not included in the repository due to size. 
* model - Scripts associated with the Bayesian models, including the outputs used to create the multipanel Fig. 4 (Bayesian hierarchical models establishing how economic, social, and political barriers are associated with distributions of aquatic food benefits)
* model_outputs - Stores the outputs of the Bayesian models
* fig_2 - Script to produce Fig. 2 (Global distribution of benefits from aquatic foods)
* fig_3 - Script to produce Fig. 3 (Recognition of barriers to participation in national policy documents)

The repository also includes some data files used in the analysis: 
* all_national_indicators.csv - All data pulled into the Bayesian model scripts in the model folder. This includes all variables considered, but not all variables are used in the model. 
* bayesian_model_variables.csv - Only includes the data (cleaned and transformed) that are in the predictor, nuisance and response (denoted by _response) variables.
* policy_nvivo_keyword_extract.csv - Keywords for the barriers to participation in national policy documents,extracted using NVivo qualitative analysis software. 
* meow_climatic_zone.csv - Climatic data used in the model folder. 
* requirements.txt - Text file used by Python-based scripts in the model folder. 
