#######################
# Cleaning Catch quality data from Eva
# Zach Koehn
# zkoehn@stanford.edu
#######################


# load dataset and packages
library(tidyverse)
library(countrycode)


work_dir <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model"

# data from https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD?view=map
dat_raw <- read.csv(
  file.path(
    work_dir,
    "data",
    "data_raw",
    "nutrition_score_Fish_Invert.csv"
  ),
  header=TRUE
)

# now aggregate annual GDP values by year and take the mean across 2006:2016

dat_clean <- dat_raw %>% 
  rename(
    country_name_en=country_name,
    iso3c=country_code,
    mean_catch_nutrition_quality=mean_nutrition_score
    ) %>%
  mutate(iso3n= countrycode(iso3c,"iso3c","iso3n")) %>%
  select(country_name_en,iso3c,iso3n,mean_catch_nutrition_quality)
# now add necessary country information using {countrycode}

# and write the csv 
write.csv(
  dat_clean,
  file.path(
    work_dir,
    "data",
    "data_clean",
    "catch_quality_2010-2014.csv"
  ),
  row.names = FALSE
  )



