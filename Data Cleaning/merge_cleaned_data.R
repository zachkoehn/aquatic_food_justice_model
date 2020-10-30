#######################
# Merge national-level data
# Zach Koehn
# zkoehn@stanford.edu
#######################

# load dataset and packages
library(tidyverse)
library(countrycode)


work_dir <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model"

setwd(file.path(work_dir,"data","data_clean"))

library(readr)  
library(dplyr)

# loads all files
files<- list.files(pattern="*.csv")
files <- files[files!="all_national_indicators.csv"] # remove pre-existing dataset so we aren't merging the same information on infinite repeat :) 
df_list <- lapply(files, read_csv) 
names(df_list) <- files

oecd_categories <- read.csv(
  file.path(work_dir,"data","data_raw","oecd_country_grouping_categories.csv"),
  header=TRUE,
  nrows=193
  ) 



oecd_categories <- oecd_categories %>%
  select(X...ISO.Code,Geographic.access..5.) %>%
  rename(
    iso3c=X...ISO.Code,
    geographic_access=Geographic.access..5.
    ) %>%
  mutate(
    geographic_access=str_trim(geographic_access,"both")) %>%
  filter(geographic_access=="Landlocked")
landlocked_countries <- oecd_categories$iso3c

# substitute 0 for NA values in all produciton data (no need to do this for total)
df_list[["production_by_isccaap_faostat_mean_2006-2016.csv"]] <- df_list[["production_by_isccaap_faostat_mean_2006-2016.csv"]] %>%
  mutate(
    across(mean_prod_carps_etc:mean_pro_horeshoe_crabs_etc, ~replace_na(.x,0))
    )
df_list[["production_by_source_faostat_mean_2006-2016.csv"]] <- df_list[["production_by_source_faostat_mean_2006-2016.csv"]] %>%
  mutate(
    across(mean_aquaculture_production_freshwater:mean_aquaculture_production_brackish, ~replace_na(.x,0))
    )

  # and now do the same thing for the SAU based variables that do not have landlocked countries


# now collapse the list into a single data frame in order to export to CSV
df_merged <- df_list %>%
  reduce(full_join, by = c("iso3n","iso3c")) %>% #merges all by the iso codes
  do(.[!duplicated(names(.))]) %>%
  filter(
    is.na(iso3n)==FALSE,
    iso3c!="NA"
    ) %>%
  dplyr::select(., #selects within piped data
         -starts_with("country"), #removes country variable (duplicated in csvs)
         -starts_with("year"),#removes year category variable (duplicated in csvs)
         -starts_with("geog"),#removes geog variable 
         -starts_with("unit"),#removes unit variable (duplicated in csvs)
         -Code #removes a code value that is from the voice and accountability (just a duplicated iso3c)
         
  ) %>% 
  mutate(
    country_name_en=countrycode(iso3n,"iso3n","country.name.en"),
    un_subregion_name=countrycode(iso3c,"iso3c","un.regionsub.name"),
    un_subregion_code=countrycode(iso3c,"iso3c","un.regionsub.code")
    ) %>%
  distinct() %>%
  mutate( # a few of the indicators reported their proportions as 57% instead of 0.57... so transforming those
    mean_educ=mean_educ*.01,
    mean_pov_prop=mean_pov_prop*.01,
    mean_women_parl_perc=mean_women_parl_perc*.01
    ) %>%
  rename(
    mean_voice_account=mean.voice.and.accountability #and change variable name to underscore from period
    ) %>%
  dplyr::select(country_name_en,iso3c,iso3n,everything())


# ok, now we need to replace  NA's for landlocked countries coming from SAU data with 0s (because SAU doesn't include any information on landlocked countries
df_merged$eez_total[df_merged$iso3c %in% landlocked_countries] <- 0
df_merged$ifa_total[df_merged$iso3c %in% landlocked_countries] <- 0
df_merged$pp_eez_weighted[df_merged$iso3c %in% landlocked_countries] <- 0 

write.csv(df_merged,
          file.path(work_dir,"data","data_clean","all_national_indicators.csv"),
          row.names=FALSE
          )

write.csv(df_merged,
          file.path(work_dir,"all_national_indicators.csv"),
          row.names=FALSE
          )

