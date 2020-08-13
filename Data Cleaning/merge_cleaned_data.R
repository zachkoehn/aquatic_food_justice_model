#######################
# Merge national-level data
# Zach Koehn
# zkoehn@stanford.edu
#######################

# load dataset and packages
library(tidyverse)
library(countrycode)


work_dir <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model"

setwd(file.path(work_dir))




setwd(file.path(work_dir,"data","data_clean"))
your_data_frame <- do.call(rbind, lapply(file.path(work_dir,"data","data_clean",file_names), read.csv, header = FALSE))

files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

library(readr)
library(dplyr)
files = list.files(pattern="*.csv")

df_list = lapply(files, read_csv) 


df_merged <- df_list %>%
  reduce(full_join, by = c("iso3n","iso3c")) %>% #merges all by 
  do(.[!duplicated(names(.))]) %>%
  filter(is.na(iso3n)==FALSE) %>%
  select(., #selects within piped data
         -starts_with("country"), #removes country variable (duplicated in csvs)
         -starts_with("year"),#removes year category variable (duplicated in csvs)
         -starts_with("geog"),#removes geog variable 
         -starts_with("unit"),#removes unit variable (duplicated in csvs)
         -X1, #removes a column that was read in as a column
         -Code #removes a code value that is from the voice and accountability (just a duplicated iso3c)
         
  ) %>%
  distinct() 




