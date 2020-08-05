# Kelvin Gorospe
# kdgorospe@gmail.com

# Data comes from Sea Around Us
rm(list=ls())
library(tidyverse)
library(countrycode)
# note: Change outdir to the filepath where you want outputs to go
outdir <- "Outputs"
# for Mac:
#datadir <- "/Volumes/jgephart/FishStatR/Data/CommoditiesAndTrade/FishStatJFiles"
# for Windows:
datadir <- "K:/Blue Foods/Data/SAU"

sau_dat <- read.csv(file = file.path(datadir, "SAU_EEZ_IFA_PP.csv"))

sau_clean <- sau_dat %>%
  rename(eez = EEZ..km2.,
         ifa = IFA..km2.,
         pp = PP..mgCm.2day.1.) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")) #%>%
# FIX IT - use FAO export data to get list of countries (this will be the "standard")
# Extract parentheses into a separate column; if able to match iso3c to this column then use THIS as the iso3c
# If unable to match iso3c to the parentheses column then try matching the original column (without parentheses)
# For ambiguous cases (when both match), refer to FAO export country list