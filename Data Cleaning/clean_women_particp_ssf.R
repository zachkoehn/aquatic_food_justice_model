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
    "harper_women_fishing",
    "harper_et_al_table_S1_female_ssf_participation.csv"
  ),
  header=TRUE
)
wo
names(dat_raw) <- c("geography","female_particip_ssf","count_femal_particip_ssf")

dat_final <- dat_raw %>%
  mutate(
    iso3c=countrycode(geography,"country.name","iso3c"), # assign iso3c from country name
    iso3n=countrycode(iso3c,"iso3c","iso3n"), # assign iso3n from iso3c
    year_range="2006-2016" #add the year_range category
  ) %>%
  select(geography,iso3c,iso3n,female_particip_ssf,count_femal_particip_ssf,year_range) %>% #only select variable 
  rename(country_name_en=geography)

# check to see how many countries are missing
# dat_final[is.na(dat_final$iso3n)==TRUE,]

dat_final <- dat_final[complete.cases(dat_final),]

write.csv(
  dat_final,
  file.path(
    "data",
    "data_clean",
    "women_particip_ssf_2006-2016.csv"
  ),
  row.names = FALSE
)
