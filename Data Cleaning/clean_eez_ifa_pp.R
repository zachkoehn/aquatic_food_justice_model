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
country_list <- read.csv(file = file.path(outdir, "trade_dat_country_list.csv"))

sau_clean <- sau_dat %>%
  rename(eez = EEZ..km2.,
         ifa = IFA..km2.,
         pp = PP..mgCm.2day.1.) %>%
  mutate(sau_name_full = Country) %>%
  # Extract parentheses into a separate column;
  separate(Country, c("sau_name", "sau_parenth"), sep = " \\(", fill = "right") %>%
  mutate(sau_parenth = str_remove(sau_parenth, "\\)")) %>%
  mutate(sau_name = str_trim(sau_name, side = "both")) %>%
  # use countrycodes to attempt matching with both sau_parenth and sau_name
  mutate(iso3c_sau_parenth = countrycode(sau_parenth, origin = "country.name", destination = "iso3c")) %>%
  mutate(iso3c_sau_name = countrycode(sau_name, origin = "country.name", destination = "iso3c"))
  
# Inspect cases where countrycodes was able to match BOTH sau_parenth and sau_name
test_double_matches <- sau_clean %>%
  filter(is.na(iso3c_sau_parenth)==FALSE & is.na(iso3c_sau_name)==FALSE) %>%
  select(sau_name_full, iso3c_sau_parenth, iso3c_sau_name) %>%
  # sau_name, NOT in parentheses, are "potential territories" of the country in parentheses
  # create a column "iso3c_final" to indicate 
  # if the "territory" name is ALSO found in trade data country_list, use the iso3c match for the territory
  # if the "territory" name is NOT found in trade data country_list, use the iso3c match for the name in parentheses
  mutate(sau_parenth_test = iso3c_sau_parenth %in% country_list$iso3c,
         sau_name_test = iso3c_sau_name %in% country_list$iso3c) %>%
  mutate(iso3c_final = case_when(sau_name_test == TRUE ~ iso3c_sau_name,
                                 sau_name_test == FALSE ~ iso3c_sau_parenth,
                                 TRUE ~ "no_match"))

# Inspect cases where countrycodes had NO MATCHES for both sau_parenth and sau_name
test_no_matches <- sau_clean %>%
  filter(is.na(iso3c_sau_parenth)==TRUE & is.na(iso3c_sau_name)==TRUE)
# NONE

# Inspect cases where only sau_parenth had a match, confirm that these should be the iso3c_final
test_parenth_matches <- sau_clean %>%
  filter(is.na(iso3c_sau_parenth)==FALSE & is.na(iso3c_sau_name)==TRUE) %>%
  select(sau_name_full, iso3c_sau_parenth, iso3c_sau_name) %>%
  # confirm all matches are also part of trade data country_list
  mutate(sau_parenth_test = iso3c_sau_parenth %in% country_list$iso3c) %>%
  mutate(iso3c_final = iso3c_sau_parenth)
  
# Inspect cases where only sau_name (non-parentheses) had a match, confirm that these should be the iso3c_final
test_non_parenth_matches <- sau_clean %>%
  filter(is.na(iso3c_sau_parenth)==TRUE & is.na(iso3c_sau_name)==FALSE) %>%
  select(sau_name_full, iso3c_sau_parenth, iso3c_sau_name) %>%
  mutate(sau_name_test = iso3c_sau_name %in% country_list$iso3c) %>%
  # American Samoa, French Guiana, and Nauru are not part of the trade data country_list - adjust these manually
  mutate(iso3c_final = case_when(sau_name_test == TRUE ~ iso3c_sau_name,
                                 sau_name_full == "American Samoa" ~ "USA", # assign to USA
                                 sau_name_full == "French Guiana" ~ "FRA", # assign to France
                                 sau_name_full == "Nauru" ~ "NRU", # assign to Nauru, even though they appear to have no exports (not in trade data)
                                 TRUE ~ "no_match"))

# Add iso3c_final back to sau_clean
final_matches <- rbind(test_double_matches[c("sau_name_full", "iso3c_final")],
                       test_parenth_matches[c("sau_name_full", "iso3c_final")],
                       test_non_parenth_matches[c("sau_name_full", "iso3c_final")])

sau_clean_final <- sau_clean %>%
  left_join(final_matches, by = "sau_name_full") %>%
  rename(iso3c = iso3c_final) %>%
  # Create an official country name column and iso3n column
  mutate(country_name = countrycode(iso3c, origin = "iso3c", destination = "country.name"),
         iso3n = countrycode(iso3c, origin = "iso3c", destination = "iso3n")) %>%
  # Remove commas (messes up convert to numeric)
  mutate_at(vars(c(eez, ifa, pp)), ~str_remove_all(., pattern = ",")) %>% 
  mutate(eez = as.numeric(eez),
         ifa = as.numeric(ifa),
         pp = as.numeric(pp)) %>%
  select(sau_name_full, country_name, iso3c, iso3n, eez, ifa, pp)

write.csv(sau_clean_final, file = file.path(outdir, "sau_eez_ifa_pp_individual.csv"), quote = FALSE, row.names = FALSE)


sau_aggregated <- sau_clean_final %>%
  group_by(country_name, iso3c, iso3n) %>%
  #summarise(eez_proportion_of_total = eez / eez_total)
  summarise(eez_total = sum(eez),
            ifa_total = sum(ifa),
            pp_eez_weighted = sum(pp * (eez/eez_total)))

write.csv(sau_aggregated, file = file.path(outdir, "sau_eez_ifa_pp_aggregated.csv"), quote = FALSE, row.names = FALSE)
