# Kelvin Gorospe
# kdgorospe@gmail.com
# Download World Bank national population stats

rm(list=ls())
library(WDI)
library(tidyverse)
library(countrycode)

outdir <- "Outputs"


new_wb_cache <- WDIcache() # Download an updated list of available WDI indicators from the WB website
str(new_wb_cache)
as.data.frame(new_wb_cache$series) %>% filter(indicator == "SP.POP.TOTL") # get information on all indicators

# get population data
population_raw <- WDI(indicator=c("total_population" = 'SP.POP.TOTL'), country="all",start=1960, end=2019, extra = TRUE) # extra = TRUE to get iso3c c
population <- population_raw %>%
  mutate(iso3n = countrycode(population$iso3c, origin = "iso3c", destination = "iso3n")) %>%
  select(country, iso3n, iso3c, year, total_population) %>%
  mutate(country = str_replace_all(country, ",", "")) # remove commas in countrynames before writing to csv

write.csv(population, file.path(outdir, "population_worldbank_all_years.csv"), row.names = FALSE, quote = FALSE)

# get mean for 2006-2016
population_mean <- population %>%
  filter(year < 2017 & year > 2005) %>% # subset 2006 to 2016
  group_by(country, iso3n, iso3c) %>%
  summarise(mean_population = mean(total_population, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(year_range = '2006-2016') # add metadata column
  
  

write.csv(population_mean, file.path(outdir, "population_worldbank_mean_2006-2016.csv"), row.names = FALSE, quote = FALSE)
