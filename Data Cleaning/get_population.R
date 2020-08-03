# kdgorospe@gmail.com
# Download World Bank national population stats

library(WDI)
library(tidyverse)
library(countrycode)
new_wb_cache <- WDIcache() # Download an updated list of available WDI indicators from the WB website
str(new_wb_cache)
as.data.frame(new_wb_cache$series) %>% filter(indicator == "SP.POP.TOTL") # get information on all indicators

# get population data
population <- WDI(indicator=c("total_population" = 'SP.POP.TOTL'), country="all",start=1960, end=2019, extra = TRUE) # extra = TRUE to get iso3c c
population <- population %>%
  select(country, iso3c, year, total_population) %>%
  mutate(iso3n = countrycode(population$iso3c, origin = "iso3c", destination = "iso3n")) %>%
  mutate(country = str_replace(country, ",", "")) # remove commas in countrynames before writing to csv

write.csv(population, "population_worldbank_all_years.csv", row.names = FALSE, quote = FALSE)

# get mean for 2006-2016
population_mean <- population %>%
  group_by(country, iso2c, iso3c) %>%
  summarise(mean_population = mean(total_population, na.rm = TRUE))
write.csv(population_mean, "population_worldbank_mean_2006-2016.csv", row.names = FALSE, quote = FALSE)