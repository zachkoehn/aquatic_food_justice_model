#######################
# Cleaning OECD SIGI Gender discirmination
# Zach Koehn
# zkoehn@stanford.edu
#######################


# load dataset and packages
library(tidyverse)
library(countrycode)

directory <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model"

# data from https://stats.oecd.org/
dat_raw_2014 <- read.csv(
  file.path(
    directory,
    "data",
    "data_raw",
    "oecd_sigi",
    "SIGI2014_22102020224146444.csv"
  ),
  # skip = 4,
  header=TRUE
)

dat_raw_2019 <- read.csv(
  file.path(
    directory,
    "data",
    "data_raw",
    "oecd_sigi",
    "SIGI2019_22102020212655411.csv"
  ),
  # skip = 4,
  header=TRUE
)



# now aggregate annual pov values by year and take the mean across 2006:2016

dat_clean_2014 <- dat_raw_2014 %>% 
  dplyr::select(LOCATION,VARIABLE,Value) %>%
  rename(
    variable=VARIABLE,
    iso3c=LOCATION,
    value=Value
    ) %>%
  filter(
    variable == "SIGI"
    ) %>%
  mutate(
    # iso3c=countrycode(Country.Name,"country.name","iso3c"), # assign iso3c from country name
    country_name_en=countrycode(iso3c,"iso3c","country.name"), # assign country name from iso3c
    iso3n=countrycode(iso3c,"iso3c","iso3n"), # assign iso3n from iso3c
    variable=str_replace_all(variable,"SIGI","SIGI_2014")
  ) %>%
  dplyr::select(country_name_en,iso3c,iso3n,variable,value)


dat_clean_2019 <- dat_raw_2019 %>% 
  dplyr::select(LOCATION,VAR,Value) %>%
  rename(
    variable=VAR,
    iso3c=LOCATION,
    value=Value
    ) %>%
  filter(
    variable == "SIGI_2"
    ) %>%
  mutate(
    # iso3c=countrycode(Country.Name,"country.name","iso3c"), # assign iso3c from country name
    country_name_en=countrycode(iso3c,"iso3c","country.name"), # assign country name from iso3c
    iso3n=countrycode(iso3c,"iso3c","iso3n"), # assign iso3n from iso3c
    variable=str_replace_all(variable,"SIGI_2","SIGI_2019"),
    value=value*.01
  ) %>%
  dplyr::select(country_name_en,iso3c,iso3n,variable,value)


dat_clean_both_years <- bind_rows(dat_clean_2014,dat_clean_2019) %>%
  distinct()

dat_final <- dat_clean_both_years %>%
  group_by(country_name_en,iso3c,iso3n) %>%
  summarize(mean_sigi=mean(value)) %>%
  mutate(
    year_range="2014_2019"
    )



write.csv(
  dat_final,
  file.path(
    directory,
    "data",
    "data_clean",
    "sigi_mean_2014-2019.csv"
  ),
  row.names = FALSE
)



