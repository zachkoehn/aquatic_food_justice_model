#######################
# Cleaning UNESCO educational attainment data
# Zach Koehn
# zkoehn@stanford.edu
#######################


# load dataset and packages
library(tidyverse)
library(here)
library(countrycode)
library(readxl)

directory <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model"


# data from http://data.uis.unesco.org/#
dat_raw <- read_excel(
  file.path(
    directory,
    "data",
    "data_raw",
    "un_wpp",
    "WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx"
  ),
  sheet="ESTIMATES"
) %>%
  as.data.frame()

column_names <- dat_raw[12,]
dat_raw <- dat_raw[-c(1:12),]
names(dat_raw) <- column_names


dat_clean <- dat_raw %>%
  rename(
    year='Reference date (as of 1 July)',
    country='Region, subregion, country or area *'
  ) %>%
  filter(
    Type=="Country/Area",
    year == "2015"
  ) %>% mutate(
    across('0-4':'100+',as.numeric)
  ) %>%
  mutate(
    total_pop=select(.,'0-4':'100+') %>% rowSums(),
    working_pop=select(.,'15-19':'60-64') %>% rowSums(),
    working_percent_un = working_pop/total_pop,
    iso3c=countrycode(country,'country.name.en','iso3c'),
    iso3n=countrycode(iso3c,"iso3c","iso3n") # assign iso3n from iso3c
  ) %>%
  filter(
    !is.na(iso3c)
  )

# # quick check (before selecting ONLY the var we care about)
# dat_clean %>%
#   filter(country=="United States of America") %>%
#   select('15-19':'60-64') %>%
#   sum()
# 
# dat_clean %>%
#   filter(country=="United States of America") %>%
#   select(working_pop,total_pop,working_percent_un)



dat_final <- dat_clean %>%
  select(country,iso3c,iso3n,working_percent_un)


# and write the csv 
write.csv(
  dat_final,
  here(
    "data",
    "data_clean",
    "working_pop_percent_un_2015.csv"
  ),
  row.names = FALSE
)
