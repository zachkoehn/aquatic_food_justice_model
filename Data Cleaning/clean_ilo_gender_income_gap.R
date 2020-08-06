#######################
# Cleaning ILO gender income gap data
# Zach Koehn
# zkoehn@stanford.edu
#######################

# load dataset and packages
library(tidyverse)
library(here)
library(countrycode)

# data from https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD?view=map
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
