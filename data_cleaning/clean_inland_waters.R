# Kelvin Gorospe
# kdgorospe@gmail.com
# Clean inland waters data obtained from Etienne Fluet-Chouinard (efluet@stanford.edu)

# NOTE: Kosovo is claimed by Serbia, need to check other datasets to be sure this is consistent

library(tidyverse)
library(readxl)
library(countrycode)

datadir <- "C:/Users/kdgor/Downloads"
outdir <- "Outputs"

water_dat <- read_excel(path = file.path(datadir, "water_area_per_country.xlsx"), 
                        col_names = c("Country", "GLC2000", "GLWD", "GRoWI", "MA_min", "MA_max", "LT_max", "FAO_water"),
                        skip = 6) %>%
  mutate(unit = "1000_km2") %>%
  # Clean country names
  mutate(Country = case_when(Country == "Centra African Republic" ~ "Central African Republic",
                             Country == "Kosovo" ~ "Serbia",
                             TRUE ~ Country)) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"),
         iso3n = countrycode(Country, origin = "country.name", destination = "iso3n")) %>%
  select(Country, iso3n, iso3c, unit, MA_min, MA_max) %>%
  group_by(Country, iso3n, iso3c, unit) %>%
  summarise(inland_water_min = sum(MA_min),
            inland_water_max = sum(MA_max)) %>%
  ungroup()


write.csv(water_dat, file.path(outdir, "inland_waters_min_and_max.csv"), row.names = FALSE, quote = FALSE)
  
