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
  reduce(full_join, by = c("iso3n","iso3c")) %>% #merges all by 
  do(.[!duplicated(names(.))]) %>%
  filter(is.na(iso3n)==FALSE) %>%
  select(., #selects within piped data
         -starts_with("country"), #removes country variable (duplicated in csvs)
         -starts_with("year"),#removes year category variable (duplicated in csvs)
         -starts_with("geog"),#removes geog variable 
         -starts_with("unit"),#removes unit variable (duplicated in csvs)
         -Code #removes a code value that is from the voice and accountability (just a duplicated iso3c)
         
  ) %>%
  distinct() 


write.csv(df_merged,
          file.path(work_dir,"data","data_clean","all_national_indicators.csv"),
          row.names=FALSE
          )


