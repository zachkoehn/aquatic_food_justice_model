#######################
# Merge national-level data
# Zach Koehn
# zkoehn@stanford.edu
#######################

# load dataset and packages
library(tidyverse)
library(corrr)
library(corrplot)
library(viridisLite);library(viridis);library(heatmaply)

work_dir <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model"


dat <- read.csv(
  file.path(work_dir,"all_national_indicators.csv"),
  header=TRUE
)

# add climate
climate_dat <- read.csv(
  file.path(
    work_dir,'meow_climatic_zone.csv'),
  sep=';',
  header=TRUE
) %>%
  select(iso3c,LAT_ZONE)

dat <- merge(dat,climate_dat,all.x=TRUE, by='iso3c')  

dat$mean_aquaculture_production <- rowSums(cbind(dat$mean_aquaculture_production_brackish,dat$mean_aquaculture_production_marine,dat$mean_aquaculture_production_freshwater),na.rm=TRUE)

head(select(dat,mean_aquaculture_production,mean_aquaculture_production_brackish,mean_aquaculture_production_freshwater,mean_aquaculture_production_marine))

dat_model<- dat %>%
  mutate(
    unit_exports=mean_exports_USD1000/mean_exports_tonnes,
    unit_imports=mean_imports_USD1000/mean_imports_tonnes,
    sat_model_est_wealth_log = log(sat_model_est_wealth+1),
    eez_total_log = log(eez_total+1),
    log_pop = log(mean_population),
    inland_water_max_log = log(inland_water_max+1),
    mean_capture_production_log = log(mean_capture_production+1),
    mean_aquaculture_production_log = log(mean_aquaculture_production+1),
    unit_exports_log = log(unit_exports+1),
    unit_imports_log = log(unit_imports+1),
    affordability_response = fish_affordability,
    catch_quality_response = mean_catch_nutrition_quality,
    consumption_response=fish_supply_daily_g_protein_percap,
    export_response = mean_exports_USD1000/mean_gdp*mean_population,
    reliance_response=percent_animal_protein_fish,
    total_livelihood_response = direct_w_esitimated_ssf + indirect_w_esitimated_ssf,
    total_production_response = mean_total_production / direct_w_esitimated_ssf,
    women_livelihood_response = direct_w_esitimated_ssf * female_particip_ssf+ 1e-6
  ) %>%
  select(
    country_name_en,
    sat_model_est_wealth_log,mean_educ,gender_equality,language_diversity,
    cultural_hegemony,working_percent_un,mean_voice_account,
    mean_capture_production_log,mean_aquaculture_production_log,
    unit_exports_log,unit_imports_log,
    pp_eez_weighted,eez_total,inland_water_max,LAT_ZONE,fish_relative_caloric_price, 
    # mean_aquaculture_production_freshwater,mean_aquaculture_production_marine,mean_aquaculture_production_brackish,
    # mean_gdp,mean_population,
    # mean_exports_USD1000,mean_exports_tonnes,mean_imports_USD1000,mean_imports_tonnes,
    affordability_response,catch_quality_response,consumption_response,export_response,reliance_response,
    total_livelihood_response,total_production_response, women_livelihood_response
  ) 

write.csv(
  dat_model,
  file.path(work_dir,'data','data_clean','bayesian_model_variables.csv'),
  row.names = FALSE
)


dat_transform <- dat_model %>%
  select(
    country_name_en,
    sat_model_est_wealth_log,mean_educ,gender_equality,language_diversity,
    cultural_hegemony,working_percent_un,mean_voice_account,
    mean_capture_production_log,mean_aquaculture_production_log,
    unit_exports_log,unit_imports_log,
    pp_eez_weighted,eez_total,inland_water_max,fish_relative_caloric_price
    # mean_aquaculture_production_freshwater,mean_aquaculture_production_marine,mean_aquaculture_production_brackish,
    # mean_gdp,mean_population,
    # mean_exports_USD1000,mean_exports_tonnes,mean_imports_USD1000,mean_imports_tonnes,
    # affordability_response,consumption_response,export_response,reliance_response,
    # total_livelihood_response,total_production_response, women_livelihood_response
  ) 




dat_cor <- dat_transform %>%
  select(-country_name_en) %>%
  rename(
    'Wealth\n(log)'=sat_model_est_wealth_log,
    'Education'=mean_educ,
    'Gender\nequality'=gender_equality,
    'Language\ndiversity'=language_diversity,
    'Cultural\nhegemony'=cultural_hegemony,
    'Working\npopulation'=working_percent_un,
    'Voice &\naccountability'=mean_voice_account,
    'Capture\n(log)'=mean_capture_production_log,
    'Aquaculture\n(log)'=mean_aquaculture_production_log,
    'Exports\n(log)'=unit_exports_log,
    'Imports\n(log)'=unit_imports_log,
    'Primary\nproductivity'=pp_eez_weighted,
    'EEZ'=eez_total,
    'Max\nfreshwater\nrunoff'=inland_water_max,
    'Fish relative\ncaloric price'=fish_relative_caloric_price
  ) %>%
  cor(use='pairwise')

  pal_blue_yellow_coral_emdash <- c(
    colorRampPalette(c('#004B87','#FAE053'))(50),
    colorRampPalette(c('#FAE053','#FF808B'))(50)
  )
  
  corrplot.mixed(dat_cor,
                 tl.cex=.45,tl.col='gray10',
                 lower='number',
                 upper='color',
                 order = 'AOE',
                 lower.col = pal_blue_yellow_coral_emdash,
                 upper.col = pal_blue_yellow_coral_emdash,
                 number.cex=0.9
  )
  
