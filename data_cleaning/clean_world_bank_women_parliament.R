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
    "world_bank_women_parliament",
    "API_SG.GEN.PARL.ZS_DS2_en_csv_v2_1217669.csv"
  ),
  skip = 4,
  header=TRUE
)


dat_clean <- dat_raw %>% 
  select(-c(Indicator.Name,Indicator.Code)) %>%
  gather(key="year",value="women_parl_perc_annual",-Country.Name,-Country.Code) %>% #gathers all GDP-year combinations into two columns (year and gdp_annual)
  mutate(year=as.numeric(str_replace_all(year,"X",""))) %>% # cleans string to remove leading X....
  filter(year > 2005 & year < 2017 ) %>% # subset annual sequence we are currently using 2006:2016
  group_by(Country.Name,Country.Code) %>% # group by country
  summarize(mean_women_parl_perc=mean(women_parl_perc_annual,na.rm=TRUE)) # aggregate by mean

# now add necessary country information using {countrycode}

dat_final <- dat_clean %>%
  mutate(
    iso3c=countrycode(Country.Name,"country.name","iso3c"), # assign iso3c from country name
    iso3n=countrycode(iso3c,"iso3c","iso3n"), # assign iso3n from iso3c
    year_range="2006-2016" #add the year_range category
  ) %>%
  rename(country_name_en=Country.Name) %>% #rename to align with other datasets
  select(country_name_en,iso3c,iso3n,mean_women_parl_perc,year_range) #only select variable 

# and write the csv 
write.csv(
  dat_final,
  file.path(
    "data",
    "data_clean",
    "women_parl_2006-2016.csv"
  ),
  row.names = FALSE
)
