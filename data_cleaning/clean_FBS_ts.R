# Clean food balance sheet data to provide consumption and consumption as percent of animal protein

library(tidyverse)
library(countrycode)

# First load function for rebuilding from FishStat ZIP file:
rm(list=ls())

# note: Change outdir to the filepath where you want outputs to go
outdir <- "Outputs"
# for MacOS
datadir <- "/Volumes/jgephart/FishStatR/Data/FoodBalanceSheets"

df_new <- read.csv(file.path(datadir, "FoodBalanceSheets_E_All_Data", "FoodBalanceSheets_E_All_Data.csv"))
df_hist <- read.csv(file.path(datadir, "FoodBalanceSheetsHistoric_E_All_Data", "FoodBalanceSheetsHistoric_E_All_Data.csv"))

# Filter to countries and protein supply
df_new <- df_new %>%
  filter(Element == "Protein supply quantity (g/capita/day)", Area.Code < 1000) %>%
  filter(Item %in% c("Fish, Seafood", "Aquatic Products, Other", "Aquatic Plants", "Animal Products")) %>%
  select(-ends_with("F")) %>% 
  pivot_longer(Y2014:Y2017, names_to = "year") %>%
  mutate(year = as.numeric(gsub("Y", "", year)))

df_hist <- df_hist %>%
  filter(Element == "Protein supply quantity (g/capita/day)", Area.Code < 1000) %>%
  filter(Item %in% c("Fish, Seafood", "Aquatic Products, Other", "Aquatic Plants", "Animal Products")) %>%
  select(-ends_with("F")) %>% 
  pivot_longer(Y1961:Y2013, names_to = "year") %>%
  mutate(year = as.numeric(gsub("Y", "", year)))

df <- bind_rows(df_new, df_hist) %>%
  pivot_wider(names_from = Item, values_from = value, names_repair = "universal") %>%
  mutate(iso3c = countrycode(Area, origin = "country.name", destination = "iso3c"),
         iso3n = countrycode(Area, origin = "country.name", destination = "iso3n")) %>%
  filter(!(Area == "China")) # Only include China, mainland because territories report separate data
# Clean codes
df$iso3c[df$Area == "Eswatini"] <- "SWZ"
df$iso3n[df$Area == "Eswatini"] <- "748"

# Group to calculate reliance
df <- df %>%
  group_by(year, Area, iso3c, iso3n) %>%
  summarise(fish_supply_daily_g_protein_percap = sum(c(Fish..Seafood, Aquatic.Products..Other, -1*Aquatic.Plants), na.rm = TRUE),
            percent_animal_protein_fish = 100*(fish_supply_daily_g_protein_percap/sum(fish_supply_daily_g_protein_percap + Animal.Products, na.rm = TRUE)))


df <- df %>%
  select(country = Area, iso3c, iso3n, year, fish_supply_daily_g_protein_percap, percent_animal_protein_fish)

write.csv(df, "Outputs/FBS_seafood_consumption_reliance_ts.csv", row.names = FALSE)

