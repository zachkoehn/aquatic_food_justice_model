#######################
# Cleaning Women participating in fisheries data (Harper et al 2020)
# Zach Koehn
# zkoehn@stanford.edu
#######################

# load dataset and packages
library(tidyverse)
library(here)
library(countrycode)

# data from Table S2 in https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0228912#sec010
directory <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model"
setwd(
  file.path(
    directory
  )
)
dat_raw <- read.csv(
  file.path(
    "data",
    "data_raw",
    "world_bank_gender_politics",
    "data.csv"
  ),
  header=TRUE
)

dat_clean <- dat_raw %>%
  filter(
    Indicator == "Women in parliament",
    Subindicator.Type =="Index"
  ) %>% 
  select(Country.Name,Country.ISO3,X2018)
  


dat_final <- dat_clean %>%
  mutate(
    iso3n=countrycode(Country.ISO3,"iso3c","iso3n"), # assign iso3n from iso3c
    year_range="snapshot_2018"
  ) %>%
  rename(
    country_name_en=Country.Name,
    women_parl_index=X2018,
    iso3c=Country.ISO3
         ) %>%
  select(country_name_en,iso3c,iso3n,women_parl_index,year_range)  #only select variable 

# check to see how many countries are missing
# dat_final[is.na(dat_final$iso3n)==TRUE,]


write.csv(
  dat_final,
  file.path(
    "data",
    "data_clean",
    "women_parliament_snapshot2018.csv"
  ),
  row.names = FALSE
)
