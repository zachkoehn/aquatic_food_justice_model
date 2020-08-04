#######################
# Cleaning World Bank poverty rate data
# Zach Koehn
# zkoehn@stanford.edu
#######################


# load dataset and packages
library(tidyverse)
library(here)
library(countrycode)

# data from https://data.worldbank.org/indicator/SI.POV.DDAY?view=map
dat_raw <- read.csv(
  here(
    "data",
    "data_raw",
    "world_bank_poverty",
    "API_SI.POV.DDAY_DS2_en_csv_v2_1217581.csv"
  ),
  skip = 4,
  header=TRUE
)

# now aggregate annual pov values by year and take the mean across 2006:2016

dat_clean <- dat_raw %>% 
  select(-c(Indicator.Name,Indicator.Code)) %>%
  gather(key="year",value="povprop_annual",-Country.Name,-Country.Code) %>% #gathers all pov-year combinations into two columns (year and gdp_annual)
  mutate(year=as.numeric(str_replace_all(year,"X",""))) %>% # cleans string to remove leading X....
  filter(year > 2005) %>% # subset annual sequence we are currently using 2006:2016
  group_by(Country.Name,Country.Code) %>% # group by country
  summarize(mean_pov_prop=mean(povprop_annual,na.rm=TRUE)) # aggregate by mean

# now add necessary country information using {countrycode}

dat_final <- dat_clean %>%
  mutate(
    iso3c=countrycode(Country.Name,"country.name","iso3c"), # assign iso3c from country name
    iso3n=countrycode(iso3c,"iso3c","iso3n"), # assign iso3n from iso3c
    year_range="2006-2016" #add the year_range category
  ) %>%
  rename(country_name_en=Country.Name) %>% #rename to align with other datasets
  select(country_name_en,iso3c,iso3n,mean_pov_prop,year_range) #only select variable 

dat_final
# and write the csv 
write.csv(
  dat_final,
  here(
    "data",
    "data_clean",
    "poverty_mean_2006-2016.csv"
  ),
  row.names = FALSE
)



