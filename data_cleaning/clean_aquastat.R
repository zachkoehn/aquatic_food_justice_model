# Aquastat data cleaning

library(tidyverse)
library(countrycode)

# First load function for rebuilding from FishStat ZIP file:
rm(list=ls())

# note: Change outdir to the filepath where you want outputs to go
outdir <- "Outputs"
# for MacOS
datadir <- "/Volumes/jgephart/Blue Foods/Data/AQUASTAT"

df <- read.csv(file.path(datadir, "aquastat.csv"))

df$iso3c <- countrycode(df$Area, origin = "country.name", destination = "iso3c")
df$iso3n <- countrycode(df$Area, origin = "country.name", destination = "iso3n")
df$year.range <- "2017"

df <- df %>%
  select(country = Area, year.range, iso3c, iso3n, total_renewable_water = Value)

write.csv(df, "Outputs/aquastat_total_renewable_water.csv", row.names = FALSE)
