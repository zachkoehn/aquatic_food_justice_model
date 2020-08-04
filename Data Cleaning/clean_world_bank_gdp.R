#######################
# Cleaning
# Zach Koehn
# zkoehn@stanford.edu
#######################


# load dataset and packages
library(tidyverse)
library(here)

dat_raw <- read.csv(
  here(
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
  filter(year > 2005) %>% # subset annual sequence we are currently using 2006:2016
  group_by(Country.Name,Country.Code) %>% # group by country
  summarize(mean_gdp=mean(gdp_annual,na.rm=TRUE)) # aggregate by mean

# now add necessary country information using 


write.csv("data","data_clean")
