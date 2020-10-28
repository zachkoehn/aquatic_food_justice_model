#######################
# Cleaning World Bank poverty rate data
# Zach Koehn
# zkoehn@stanford.edu
#######################


# load dataset and packages
library(tidyverse)
library(here)
library(countrycode)

directory <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model"

# data from https://data.worldbank.org/indicator/SI.POV.NAHC
dat_raw <- read.csv(
  file.path(
    directory,
    "data",
    "data_raw",
    "world_bank_poverty",
    "API_SI.POV.NAHC_DS2_en_csv_v2_1345136.csv"
  ),
  skip = 4,
  header=TRUE
)

# now aggregate annual pov values by year and take the mean across 2006:2016

dat_clean <- dat_raw %>% 
  dplyr::select(-c(Indicator.Name,Indicator.Code)) %>%
  gather(key="year",value="povprop_annual",-Country.Name,-Country.Code) %>% #gathers all pov-year combinations into two columns (year and gdp_annual)
  mutate(year=as.numeric(str_replace_all(year,"X",""))) %>% # cleans string to remove leading X....
  filter(year > 2005 & year < 2017 ) %>% # subset annual sequence we are currently using 2006:2016
  group_by(Country.Name,Country.Code) %>% # group by country
  summarize(mean_pov_prop=mean(povprop_annual,na.rm=TRUE)) # aggregate by mean

# now add necessary country information using {countrycode}

dat_final <- dat_clean %>%
  mutate(
    iso3c=countrycode(Country.Name,"country.name","iso3c"), # assign iso3c from country name
    iso3n=countrycode(iso3c,"iso3c","iso3n"), # assign iso3n from iso3c
    year_range="2006-2016" #add the year_range category
  ) %>%
  filter(is.na(iso3n) ==FALSE) %>%
  rename(country_name_en=Country.Name) %>% #rename to align with other datasets
  dplyr::select(country_name_en,iso3c,iso3n,mean_pov_prop,year_range) #only select variable 


write.csv(
  dat_final,
  here(
    "data",
    "data_clean",
    "poverty_mean_2006-2016.csv"
  ),
  row.names = FALSE
)



