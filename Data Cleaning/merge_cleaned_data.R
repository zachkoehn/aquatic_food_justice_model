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
files<- list.files(pattern="*.csv")
files <- files[files!="all_national_indicators.csv"] # remove pre-existing dataset so we aren't merging the same information on infinite repeat :) 
df_list <- lapply(files, read_csv) 


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
  mutate(country_name_en=countrycode(iso3n,"iso3n","country.name.en")) %>%
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

df_merged[df_merged$iso3c=="CHN",]


write.csv(df_merged,
          file.path(work_dir,"data","data_clean","all_national_indicators.csv"),
          row.names=FALSE
          )

write.csv(df_merged,
          file.path(work_dir,"all_national_indicators.csv"),
          row.names=FALSE
          )

