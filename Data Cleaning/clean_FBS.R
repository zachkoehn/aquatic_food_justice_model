# Clean food balance sheet data to provide consumption and consumption as percent of animal protein

library(tidyverse)
library(countrycode)

# First load function for rebuilding from FishStat ZIP file:
rm(list=ls())

# note: Change outdir to the filepath where you want outputs to go
outdir <- "Outputs"
# for MacOS
datadir <- "/Volumes/jgephart/FishStatR/Data/FoodBalanceSheets/FoodBalanceSheets_E_All_Data"

df <- read.csv(file.path(datadir, "FoodBalanceSheets_E_All_Data.csv"))

# Filter to countries and protein supply
df <- df %>%
  filter(Element == "Protein supply quantity (g/capita/day)", Area.Code < 1000) 

# Calculate 2014-2016 average fish protein supply
df <- df %>%
  filter(Item %in% c("Fish, Seafood", "Aquatic Products, Other", "Aquatic Plants", "Animal Products")) %>%
  group_by(Area, Item) %>%
  summarise(supply = sum(Y2014, Y2015, Y2016)/3) %>% # Calculate average supply
  pivot_wider(names_from = Item, values_from = supply, names_repair = "universal") %>%
  mutate(fish_supply_daily_g_protein_percap = sum(c(Fish..Seafood, Aquatic.Products..Other, -1*Aquatic.Plants), na.rm = TRUE)) %>%
  mutate(percent_animal_protein_fish = 100*(fish_supply_daily_g_protein_percap/sum(fish_supply_daily_g_protein_percap + Animal.Products, na.rm = TRUE)))

# Format for full analysis
df$iso3c <- countrycode(df$Area, origin = "country.name", destination = "iso3c")
df$iso3c[df$Area == "Eswatini"] <- "SWZ"
df$iso3n <- countrycode(df$Area, origin = "country.name", destination = "iso3n")
df$iso3n[df$Area == "Eswatini"] <- "748"
df$year.range <- "2014-2016"

df <- df %>%
  select(country = Area, year.range, iso3c, iso3n, fish_supply_daily_g_protein_percap, percent_animal_protein_fish) %>%
  filter(!(Area == "China")) # Only include China, mainland because territories report separate data

write.csv(df, "Outputs/FBS_seafood_consumption_reliance.csv", row.names = FALSE)

  