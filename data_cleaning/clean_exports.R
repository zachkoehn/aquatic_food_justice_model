# Kelvin Gorospe
# kdgorospe@gmail.com
# Get net exports (in tonnes and USD) from FAO Stat

# First load function for rebuilding from FishStat ZIP file:
rm(list=ls())
library(tidyverse)
# note: Change outdir to the filepath where you want outputs to go
outdir <- "Outputs"
# for MacOS
datadir <- "/Volumes/jgephart/FishStatR/Data/CommoditiesAndTrade/FishStatJFiles"
# for Windows:
#datadir <- "K:/FishStatR/Data/CommoditiesAndTrade/FishStatJFiles"

# First load function for rebuilding from FishStat ZIP file:

clean_fish<-function(path_to_file, dataset="value_column"){ # dataset parameter is the column name that will be assigned to values_to in pivot_longer
  # Consider removing check_pgk_deps because this will be different for different functions:
  # check_pkg_deps() # packages are automatically loaded, but will show error message if one of the needs to be installed
  require(dplyr)
  require(tidyr)
  require(stringr)
  fish_file<-read.csv(path_to_file)
  #Remove last three rows: two rows of summed tonnage and abundance + citation
  fish_file<-fish_file %>%
    dplyr::filter(!str_detect((fish_file)[,1], pattern = "Total")) %>%
    dplyr::filter(!str_detect(.[,1], pattern = "www"))
  
  #Year column issues: "...", "-", "0 0", and blanks
  #Find year columns (i.e., start_with X) and mutate those columns by replacing "..." with NAs
  fish_clean_years<-fish_file %>%
    mutate_at(vars(starts_with("X")), ~na_if(., "...")) %>%
    #Same, but now replace "-" with NAs
    mutate_at(vars(starts_with("X")), ~na_if(., "-")) %>%
    #Same, but now replace "0 0" with NAs
    mutate_at(vars(starts_with("X")), ~na_if(., "0 0")) %>%
    #Same, but now remove "F"s
    mutate_at(vars(starts_with("X")), ~str_replace_all(., "F", "")) %>%
    #Same, but not trim white space
    mutate_at(vars(starts_with("X")), ~str_trim(.)) %>%
    #Same, but now convert to numeric
    mutate_at(vars(starts_with("X")), ~as.numeric(.))
  
  tmp_fish<-fish_clean_years %>%
    pivot_longer(cols = starts_with("X"),
                 names_to = "YEAR",
                 names_prefix = "X",
                 names_ptypes = list(Year=integer()),
                 values_to = dataset,
                 values_drop_na = TRUE) #this last option drops years with NAs
}


trade_dat <- clean_fish(path_to_file = file.path(datadir, "FAO Fisheries commodities production and trade-VALUE.csv"), dataset = "trade_USD1000")

# Output total imports and exports per country
trade_dat_total <- trade_dat %>%
  rename(iso3n = Country..Country..2,
         iso3c = Country..Country..5,
         year = YEAR,
         unit = Unit,
         country_name_en = Country..Country., 
         trade_flow = Trade.flow..Trade.flow.) %>%
  filter(Commodity..ISSCAAP.group. %in% c("Red seaweeds", "Green seaweeds", "Brown seaweeds")==FALSE) %>%
  filter(Commodity..ISSCAAP.division. != "PLANTAE AQUATICAE") %>% 
  filter(unit == "USD 1000") %>%
  mutate(year = as.numeric(year)) %>%
  filter(year < 2017 & year > 2005) %>% # subset 2006 to 2016
  filter(trade_flow %in% c("Exports", "Imports")) %>% # Remove "re-exports"
  group_by(country_name_en, iso3n, iso3c, year, unit, trade_flow) %>%
  summarise(total_trade_by_year = sum(trade_USD1000, na.rm = TRUE)) %>% # Calculate total export and imports for each country for each year
  ungroup() %>%
  group_by(country_name_en, iso3n, iso3c, unit, trade_flow) %>% 
  summarise(mean_trade_USD1000 = mean(total_trade_by_year, na.rm = TRUE)) %>% # now collapse year, calculate mean
  ungroup() %>%
  mutate_all(~str_replace_all(., ",", "")) %>% # remove commas before writing to csv
  mutate(year_range = '2006-2016') %>% # add metadata column
  pivot_wider(names_from = trade_flow, values_from = mean_trade_USD1000) %>%
  rename(mean_imports_USD1000 = Imports,
         mean_exports_USD1000 = Exports)

  
# get list of unique countries for future reference:
trade_dat_country_list <- trade_dat_total %>%
  select(country_name_en, iso3n, iso3c) %>%
  unique() %>%
  arrange(country_name_en)

# OUTPUTS:
write.csv(trade_dat_country_list, file.path(outdir, "trade_dat_country_list.csv"), quote = FALSE, row.names = FALSE)

write.csv(trade_dat_total, file.path(outdir, "distribution_trade_in_1000USD_faostat_mean_2006-2016.csv"), row.names = FALSE, quote = FALSE)

# REPEAT with trade quantities dataset:
trade_quantity <- clean_fish(path_to_file = file.path(datadir, "FAO Fisheries commodities production and trade-QUANTITY.csv"), dataset = "trade_tonnes")

trade_quantity_total <- trade_quantity %>%
  rename(iso3n = Country..Country..2,
         iso3c = Country..Country..5,
         year = YEAR,
         unit = Unit,
         country_name_en = Country..Country., 
         trade_flow = Trade.flow..Trade.flow.) %>%
  filter(Commodity..ISSCAAP.group. %in% c("Red seaweeds", "Green seaweeds", "Brown seaweeds")==FALSE) %>%
  filter(Commodity..ISSCAAP.division. != "PLANTAE AQUATICAE") %>% 
  mutate(unit = str_remove(unit, " \x96")) %>%
  filter(unit == "Tonnes net product weight") %>%
  mutate(year = as.numeric(year)) %>%
  filter(year < 2017 & year > 2005) %>% # subset 2006 to 2016
  filter(trade_flow %in% c("Exports", "Imports")) %>%
  group_by(country_name_en, iso3n, iso3c, year, unit, trade_flow) %>%
  summarise(total_trade_by_year = sum(trade_tonnes, na.rm = TRUE)) %>% # Calculate total export for each country for each year
  ungroup() %>%
  group_by(country_name_en, iso3n, iso3c, unit, trade_flow) %>%
  summarise(mean_trade_tonnes = mean(total_trade_by_year, na.rm = TRUE)) %>% # now collapse year, calculate mean
  ungroup() %>%
  mutate_all(~str_replace_all(., ",", "")) %>% # remove commas before writing to csv
  mutate(year_range = '2006-2016') %>% # add metadata column
  pivot_wider(names_from = trade_flow, values_from = mean_trade_tonnes) %>%
  rename(mean_imports_tonnes = Imports,
         mean_exports_tonnes = Exports)

write.csv(trade_quantity_total, file.path(outdir, "distribution_trade_in_tonnes_faostat_mean_2006-2016.csv"), row.names = FALSE, quote = FALSE)
