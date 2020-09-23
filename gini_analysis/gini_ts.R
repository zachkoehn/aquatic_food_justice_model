# Gini time series

#____________________________________________________________________________________________________#
# Load pacakges
#____________________________________________________________________________________________________#
library(tidyverse)
library(ggplot2)
library(countrycode)
library(REAT)

#____________________________________________________________________________________________________#
# Load data
#____________________________________________________________________________________________________#
prod <- read.csv("Outputs/production_by_source_faostat_ts.csv")
exports_usd <- read.csv("Outputs/distribution_exports_in_USD_ts.csv")
exports_t <- read.csv("Outputs/distribution_exports_in_tonnes_ts.csv")
supply <- read.csv("Outputs/FBS_seafood_consumption_reliance_ts.csv")
pop <- read.csv("population_worldbank_all_years.csv")

#____________________________________________________________________________________________________#
# Filter population and join to df
#____________________________________________________________________________________________________#
# Replace NA's with zeros in appropriate columns (FIX IT: need to check this carefully--> currently just doing everywhere)
pop <- pop %>%
  select(iso3c, year, pop = total_population)

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

# Convert consumption to total
supply$total_supply_gprotein <- supply$fish_supply_daily_g_protein_percap*supply$pop

# Check for missing country data
unique(prod$iso3c[!(prod$iso3c %in% pop$iso3c)])
unique(exports_t$iso3c[!(exports_t$iso3c %in% pop$iso3c)])
unique(exports_usd$iso3c[!(exports_usd$iso3c %in% pop$iso3c)])
unique(supply$iso3c[!(supply$iso3c %in% pop$iso3c)])
#____________________________________________________________________________________________________#
# Calculate Gini for each year
#____________________________________________________________________________________________________#
gini_capture <- prod %>% 
  filter(pop > 0) %>%
  mutate(value_per_cap = mean_capture_production/pop) %>%
  group_by(year) %>%
  summarise(gini = gini(value_per_cap, weighting=pop)) %>%
  mutate(variable = "Capture (t)", stage = "Production")

gini_fw_aqua <- prod %>% 
  filter(pop > 0) %>%
  mutate(value_per_cap = mean_aquaculture_production_freshwater/pop) %>%
  group_by(year) %>%
  summarise(gini = gini(value_per_cap, weighting=pop))%>%
  mutate(variable = "Freshwater aquaculture", stage = "Production")

gini_brackish_aqua <- prod %>% 
  filter(pop > 0) %>%
  mutate(value_per_cap = mean_aquaculture_production_brackish/pop) %>%
  group_by(year) %>%
  summarise(gini = gini(value_per_cap, weighting=pop))%>%
  mutate(variable = "Brackish aquaculture", stage = "Production")

gini_marine_aqua <- prod %>% 
  filter(pop > 0) %>%
  mutate(value_per_cap = mean_aquaculture_production_marine/pop) %>%
  group_by(year) %>%
  summarise(gini = gini(value_per_cap, weighting=pop))%>%
  mutate(variable = "Marine aquaculture", stage = "Production")

gini_aqua <- prod %>%
  filter(pop > 0) %>%
  mutate(aqua_total = mean_aquaculture_production_marine + mean_aquaculture_production_brackish + mean_aquaculture_production_freshwater) %>%
  mutate(value_per_cap = aqua_total/pop) %>%
  group_by(year) %>%
  summarise(gini = gini(value_per_cap, weighting=pop))%>%
  mutate(variable = "Aquaculture (t)", stage = "Production")

gini_exports_t <- exports_t %>% 
  filter(pop > 0) %>%
  mutate(value_per_cap = total_exports_by_year/pop) %>%
  group_by(year) %>%
  summarise(gini = gini(value_per_cap, weighting=pop)) %>%
  mutate(variable = "Exports (t)", stage = "Distribution")

gini_exports_usd <- exports_usd %>% 
  filter(pop > 0) %>%
  mutate(value_per_cap = total_exports_by_year/pop) %>%
  group_by(year) %>%
  summarise(gini = gini(value_per_cap, weighting=pop)) %>%
  mutate(variable = "Exports (USD)", stage = "Distribution")

gini_protein_supply <- supply %>% 
  filter(pop > 0) %>%
  group_by(year) %>%
  summarise(gini = gini(fish_supply_daily_g_protein_percap, weighting=pop)) %>%
  mutate(variable = "Supply (g protein)", stage = "Consumption")

gini_all <- gini_capture %>%
  bind_rows(gini_fw_aqua) %>%
  bind_rows(gini_brackish_aqua) %>%
  bind_rows(gini_marine_aqua) %>%
  bind_rows(gini_aqua) %>%
  bind_rows(gini_exports_t) %>%
  bind_rows(gini_exports_usd) %>%
  bind_rows(gini_protein_supply)

#____________________________________________________________________________________________________#
# Plot across years
#____________________________________________________________________________________________________#
ggplot(gini_all %>% filter(variable %in% c("Capture (t)", "Aquaculture (t)", "Exports (t)", "Supply (g protein)")), 
       aes(x = year, y = gini, color = variable)) +
  geom_line(size = 1.25) +
  labs(x = "", y = "Gini coefficient") +
  theme_minimal()

#____________________________________________________________________________________________________#
# Plot Lorenz curves 
#____________________________________________________________________________________________________#
plot.lc <- function(plot.df, plot.col, plot.year){
  colnames(plot.df)[colnames(plot.df) == plot.col] <- "value"
  
  plot.df <- plot.df %>%
    filter(pop > 0, year == plot.year) %>%
    select(iso3c, value, pop) %>% 
    mutate(value_percap = value/pop) 
  
  par(mfrow=c(2,1))
  hist(plot.df$value_percap, breaks = 25, main = paste(plot.col, "\n Histogram"), 
       xlab = "Benefit per capita")
  gini(plot.df$value_percap, weighting=plot.df$pop, lc = TRUE, 
       lctitle = paste("Lorenz Curve, Gini =", round(gini(plot.df$value_percap, weighting=plot.df$pop), 2)),
       lcx = "% of population", lcy = "% of benefit",
       le.col = "black", lc.col = "black",
       lsize = 1, ltype = "solid",
       bg.col = "white", bgrid = FALSE)
}


# Prodcution
plot.year <- 2000
plot.lc(plot.df = prod, plot.col = "mean_capture_production", plot.year = plot.year)
plot.lc(plot.df = prod, plot.col = "mean_aquaculture_production_freshwater", plot.year = plot.year)
plot.lc(plot.df = prod, plot.col = "mean_aquaculture_production_brackish", plot.year = plot.year)
plot.lc(plot.df = prod, plot.col = "mean_aquaculture_production_marine", plot.year = plot.year)

# Distribution
plot.lc(plot.df = exports_t, plot.col = "total_exports_by_year", plot.year = plot.year)
plot.lc(plot.df = exports_usd, plot.col = "total_exports_by_year", plot.year = plot.year)

# Consumption
plot.lc(plot.df = supply, plot.col = "total_supply_gprotein", plot.year = plot.year)

# Test of the function
income <- c(10, 20)
groups <- c(1, 3)
income_indiv <- c(10, 20, 20, 20)
gini(income, weighting = groups)
gini(income_indiv)
