# Gini time series

#____________________________________________________________________________________________________#
# Load pacakges
#____________________________________________________________________________________________________#
library(tidyverse)
library(ggplot2)
library(countrycode)
library(ineq)
library(REAT)

#____________________________________________________________________________________________________#
# Load data
#____________________________________________________________________________________________________#
prod <- read.csv("Outputs/production_by_source_faostat_ts.csv")
exports_usd <- read.csv("Outputs/distribution_exports_in_USD_ts.csv")
exports_t <- read.csv("Outputs/distribution_exports_in_tonnes_ts.csv")
supply <- read.csv("Outputs/FBS_seafood_consumption_reliance_ts.csv")
pop <- read.csv("FAOSTAT_population_data_8-21-2020.csv")

#____________________________________________________________________________________________________#
# Filter population and join to df
#____________________________________________________________________________________________________#
pop$iso3c <- countrycode(pop$Area, origin = "country.name", destination = "iso3c")
# FIX IT: Check unmatched countries with country list in df
# Replace NA's with zeros in appropriate columns (FIX IT: need to check this carefully--> currently just doing everywhere)
pop <- pop %>%
  select(iso3c, year = Year, pop = Value)

prod <- prod %>%
  left_join(pop, by = c("iso3c", "year"))
prod[is.na(prod)] <- 0

exports_usd$year <- as.numeric(as.character(exports_usd$year))
exports_usd <- exports_usd %>%
  left_join(pop, by = c("iso3c", "year"))
exports_usd[is.na(exports_usd)] <- 0

exports_t$year <- as.numeric(as.character(exports_t$year))
exports_t <- exports_t %>%
  left_join(pop, by = c("iso3c", "year"))
exports_t[is.na(exports_t)] <- 0

supply <- supply %>%
  left_join(pop, by = c("iso3c", "year"))
supply[is.na(supply)] <- 0

# Convert consumption to total
supply$total_supply_gprotein <- supply$fish_supply_daily_g_protein_percap*supply$pop

#____________________________________________________________________________________________________#
# Calculate Gini for each year
#____________________________________________________________________________________________________#
gini_capture <- prod %>% group_by(year) %>%
  summarise(gini = gini(mean_capture_production, weighting=pop)) %>%
  mutate(variable = "Capture", stage = "Production")

gini_fw_aqua <- prod %>% group_by(year) %>%
  summarise(gini = gini(mean_aquaculture_production_freshwater, weighting=pop))%>%
  mutate(variable = "Freshwater aquaculture", stage = "Production")

gini_brackish_aqua <- prod %>% group_by(year) %>%
  summarise(gini = gini(mean_aquaculture_production_brackish, weighting=pop))%>%
  mutate(variable = "Brackish aquaculture", stage = "Production")

gini_marine_aqua <- prod %>% group_by(year) %>%
  summarise(gini = gini(mean_aquaculture_production_marine, weighting=pop))%>%
  mutate(variable = "Marine aquaculture", stage = "Production")

gini_exports_t <- exports_t %>% group_by(year) %>%
  summarise(gini = gini(total_exports_by_year, weighting=pop)) %>%
  mutate(variable = "Exports (t)", stage = "Distribution")

gini_exports_usd <- exports_usd %>% group_by(year) %>%
  summarise(gini = gini(total_exports_by_year, weighting=pop)) %>%
  mutate(variable = "Exports (USD)", stage = "Distribution")

gini_protein_supply <- supply %>% group_by(year) %>%
  summarise(gini = gini(total_supply_gprotein, weighting=pop)) %>%
  mutate(variable = "Supply (g protein)", stage = "Consumption")

gini_all <- gini_capture %>%
  bind_rows(gini_fw_aqua) %>%
  bind_rows(gini_brackish_aqua) %>%
  bind_rows(gini_marine_aqua) %>%
  bind_rows(gini_exports_t) %>%
  bind_rows(gini_exports_usd) %>%
  bind_rows(gini_protein_supply)

#____________________________________________________________________________________________________#
# Plot across years
#____________________________________________________________________________________________________#
ggplot(gini_all, aes(x = year, y = gini, color = variable)) +
  geom_line() +
  theme_minimal()

ggplot(gini_all %>% filter(variable != "Supply (g protein)"), aes(x = year, y = gini, color = variable)) +
  geom_line() +
  theme_minimal()

ggplot(gini_all %>% filter(variable == "Supply (g protein)"), aes(x = year, y = gini, color = variable)) +
  geom_line() +
  theme_minimal()
