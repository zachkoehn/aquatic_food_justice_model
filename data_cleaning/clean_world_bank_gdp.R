#######################
# Cleaning world bank GDP per capita (PPP)
# Zach Koehn
# zkoehn@stanford.edu
#######################


# load dataset and packages
library(tidyverse)
library(here)
library(countrycode)

wk_dir <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model"
# data from https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD?view=map
dat_raw <- read.csv(
  file.path(
    wk_dir,
    "data",
    "data_raw",
    "world_bank_gdp",
    "API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_1217517.csv"
  ),
  skip = 4,
  header=TRUE
)

# now aggregate annual GDP values by year and take the mean across 2006:2016

dat_clean <- dat_raw %>% 
  select(-c(Indicator.Name,Indicator.Code)) %>%
  gather(key="year",value="gdp_annual",-Country.Name,-Country.Code) %>% #gathers all GDP-year combinations into two columns (year and gdp_annual)
  mutate(year=as.numeric(str_replace_all(year,"X",""))) %>% # cleans string to remove leading X....
  filter(year > 2005 & year < 2017 ) %>% # subset annual sequence we are currently using 2006:2016
  group_by(Country.Name,Country.Code) %>% # group by country
  summarize(mean_gdp=mean(gdp_annual,na.rm=TRUE)) # aggregate by mean

# now add necessary country information using {countrycode}

dat_final <- dat_clean %>%
  mutate(
    iso3c=countrycode(Country.Name,"country.name","iso3c"), # assign iso3c from country name
    iso3n=countrycode(iso3c,"iso3c","iso3n"), # assign iso3n from iso3c
    year_range="2006-2016" #add the year_range category
    ) %>%
  rename(country_name_en=Country.Name) %>% #rename to align with other datasets
  select(country_name_en,iso3c,iso3n,mean_gdp,year_range) #only select variable 

# and write the csv 
write.csv(
  dat_final,
  here(
    "data",
    "data_clean",
    "gdp_mean_2006-2016.csv"
  ),
  row.names = FALSE
  )


# time series for Liz

dat_clean_ts <- dat_raw %>% 
  select(-c(Indicator.Name,Indicator.Code)) %>%
  gather(key="year",value="gdp_annual",-Country.Name,-Country.Code) %>% #gathers all GDP-year combinations into two columns (year and gdp_annual)
  mutate(year=as.numeric(str_replace_all(year,"X",""))) %>% # cleans string to remove leading X....
  filter(year > 2007 ) 
# now add necessary country information using {countrycode}

dat_final_ts <- dat_clean_ts %>%
  rename(country_name_en=Country.Name)  #rename to align with other datasets
  

# and write the csv 
write.csv(
  dat_final_ts,
  here(
    "data",
    "data_clean",
    "gdp_mean_annual_2006-2016.csv"
  ),
  row.names = FALSE
  )
