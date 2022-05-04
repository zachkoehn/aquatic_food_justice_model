# First load function for rebuilding from FishStat ZIP file:
rm(list=ls())
library(tidyverse)
# note: Change outdir to the filepath where you want outputs to go
outdir <- "Outputs"
# for MacOS
datadir <- "/Volumes/jgephart/FishStatR/Data/CommoditiesAndTrade/FishStatJFiles"


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


trade_dat <- clean_fish(path_to_file = file.path(datadir, "FAO Fisheries commodities production and trade-VALUE.csv"), dataset = "net_exports_USD1000")

# Output total exports per country
trade_dat_total <- trade_dat %>%
  rename(iso3n = Country..Country..2,
         iso3c = Country..Country..5,
         year = YEAR,
         unit = Unit,
         country_name_en = Country..Country.) %>%
  filter(Commodity..ISSCAAP.group. %in% c("Red seaweeds", "Green seaweeds", "Brown seaweeds")==FALSE) %>%
  filter(Commodity..ISSCAAP.division. != "PLANTAE AQUATICAE") %>% 
  filter(unit == "USD 1000") %>%
  mutate(year = as.numeric(year)) %>%
  group_by(country_name_en, iso3n, iso3c, year, unit) %>%
  summarise(total_exports_by_year = sum(net_exports_USD1000, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate_all(~str_replace_all(., ",", "")) # remove commas before writing to csv

# OUTPUTS:
write.csv(trade_dat_total, file.path(outdir, "distribution_exports_in_USD_ts.csv"), row.names = FALSE, quote = FALSE)

# REPEAT with trade quantities dataset:\
trade_quantity <- clean_fish(path_to_file = file.path(datadir, "FAO Fisheries commodities production and trade-QUANTITY.csv"), dataset = "net_exports_tonnes")

trade_quantity_total <- trade_quantity %>%
  rename(iso3n = Country..Country..2,
         iso3c = Country..Country..5,
         year = YEAR,
         unit = Unit,
         country_name_en = Country..Country.) %>%
  filter(Commodity..ISSCAAP.group. %in% c("Red seaweeds", "Green seaweeds", "Brown seaweeds")==FALSE) %>%
  filter(Commodity..ISSCAAP.division. != "PLANTAE AQUATICAE") %>% 
  mutate(unit = str_remove(unit, " \x96")) %>%
  filter(unit == "Tonnes net product weight") %>%
  mutate(year = as.numeric(year)) %>%
  group_by(country_name_en, iso3n, iso3c, year, unit) %>%
  summarise(total_exports_by_year = sum(net_exports_tonnes, na.rm = TRUE)) %>% # Calculate total export for each country for each year
  ungroup() %>%
  mutate_all(~str_replace_all(., ",", "")) # remove commas before writing to csv

write.csv(trade_quantity_total, file.path(outdir, "distribution_exports_in_tonnes_ts.csv"), row.names = FALSE, quote = FALSE)
