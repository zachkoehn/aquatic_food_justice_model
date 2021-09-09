#######################
# Merge national-level data
# Zach Koehn
# zkoehn@stanford.edu
#######################

# load dataset and packages
library(tidyverse)
library(corrr)
library(corrplot)

work_dir <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model"


dat <- read.csv(
  file.path(work_dir,"all_national_indicators.csv"),
  header=TRUE
)

dat$mean_aquaculture_production <- rowSums(cbind(dat$mean_aquaculture_production_brackish,dat$mean_aquaculture_production_marine,dat$mean_aquaculture_production_freshwater),na.rm=TRUE)

head(select(dat,mean_aquaculture_production,mean_aquaculture_production_brackish,mean_aquaculture_production_freshwater,mean_aquaculture_production_marine))


dat_transform <- dat %>%
  mutate(
    unit_exports=mean_exports_USD1000/mean_exports_tonnes,
    unit_imports=mean_imports_USD1000/mean_imports_tonnes,
    sat_model_est_wealth_log = log(sat_model_est_wealth+1),
    eez_total_log = log(eez_total+1),
    inland_water_max_log = log(inland_water_max+1),
    mean_capture_production_log = log(mean_capture_production+1),
    mean_aquaculture_production_log = log(mean_aquaculture_production+1),
    unit_exports_log = log(unit_exports+1),
    unit_imports_log = log(unit_imports+1)
  ) %>%
  select(
    country_name_en,sat_model_est_wealth_log,mean_educ,gender_equality,language_diversity,
    cultural_hegemony,working_percent_un,mean_voice_account,
    mean_capture_production_log,mean_aquaculture_production_log,unit_exports_log,unit_imports_log
    )

dat_transform %>%
  select(-country_name_en) %>%
  rename(
    'Wealth \n(log)'=sat_model_est_wealth_log,
    'Education'=mean_educ,
    'Gender \nequality'=gender_equality,
    'Language \ndiversity'=language_diversity,
    'Cultural \nhegemony'=cultural_hegemony,
    'Working \npopulation'=working_percent_un,
    'Voice and \naccountability'=mean_voice_account,
    'Capture \n(log)'=mean_capture_production_log,
    'Aquaculture \n(log)'=mean_aquaculture_production_log,
    'Exports \n(log)'=unit_exports_log,
    'Imports \n(log)'=unit_imports_log
  ) %>%
  cor(use='pairwise') %>%
  corrplot.mixed(order = 'AOE',tl.cex=.6,tl.col='gray10')
  
ggsave(cor_plot,
       )

