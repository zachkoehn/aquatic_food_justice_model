# Gini plots for all variables

#____________________________________________________________________________________________________#
# Load pacakges
#____________________________________________________________________________________________________#
library(tidyverse)
library(countrycode)
library(reldist)
library(ggplot2)
library(gglorenz)
library(gridExtra)
library(ggthemes)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


#____________________________________________________________________________________________________#
# Load data
#____________________________________________________________________________________________________#
df <- read.csv("all_national_indicators.csv")

df <- df %>%
  mutate(mean_aquaculture = sum(mean_aquaculture_production_freshwater, 
                                mean_aquaculture_production_marine, 
                                mean_aquaculture_production_brackish, na.rm = TRUE),
         women_livelihoods = direct_w_esitimated_ssf*female_particip_ssf) %>%
  # Mutate all variables to per capita, as necessary
  mutate(
    mean_exports_USD1000_percap = mean_exports_USD1000/mean_population,
    mean_exports_tonnes_percap = mean_exports_tonnes/mean_population,
    direct_w_esitimated_ssf_percap = direct_w_esitimated_ssf/mean_population, 
    indirect_w_esitimated_ssf_percap = indirect_w_esitimated_ssf/mean_population,
    women_livelihoods_percap = women_livelihoods/mean_population,
    mean_total_production_percap = mean_total_production/mean_population, 
    mean_aquaculture_percap = mean_aquaculture/mean_population,
    mean_capture_production_percap = mean_capture_production/mean_population,
    mean_total_production_perworker = mean_total_production/direct_w_esitimated_ssf
  )

# Bind data with world data for mapping
map.world <- ne_countries(scale = "medium", returnclass = "sf")
map.world <- left_join(map.world, df, by = c("iso_a3" = "iso3c"))

#____________________________________________________________________________________________________#
# Create function for map with embedded hist and Gini
#____________________________________________________________________________________________________#

plot.dist <- function(dat.col, variable.title){
  # Calculate Gini
  df.world <- as.data.frame(map.world)
  df.world <- data.frame(gini.col = c(df.world[[dat.col]]), 
                         pop = df.world$mean_population)
  df.world <- df.world %>%
    drop_na()
  # Can add population weighting, but https://doi.org/10.1080/17421772.2017.1343491 argues against
  gini <- gini(x = df.world$gini.col, weights = df.world$pop) 
  gini(x = df.world$gini.col) 
  
  # Create histogram
  g_hist <- ggplot(df.world, aes(x = gini.col)) +
    geom_histogram() +
    theme_clean() + 
    labs(x = paste(variable.title))
  
  # Create unweighted Lorenz
  g_lorenz <- ggplot(df.world, aes(gini.col)) +
    stat_lorenz() +
    annotate_ineq(df.world$gini.col) +
    theme_clean() + 
    labs(x = paste(variable.title), y = "")
  
  # Create weighted Lorenz
  # df.world.weighted <- as.data.frame(lapply(df.world, rep, floor(df.world$pop/10000)))
  # g_lorenz <- ggplot(df.world.weighted, aes(gini.col)) +
  #   stat_lorenz() +
  #   annotate_ineq(df.world.weighted$gini.col)
  
  # Create map
  g_map <- ggplot(data = map.world) +
    geom_sf(aes_string(fill = dat.col)) +
    scale_fill_viridis_c(option = "plasma") +
    labs(fill = paste(variable.title)) +
    theme_map() 
    
  grid.arrange(grobs = list(g_hist, g_lorenz, g_map),
               layout_matrix = rbind(c(1, 1, 2, 2),
                                     c(3, 3, 3, 3),
                                     c(3, 3, 3, 3)))
}

png("Outputs/mean_total_production_percap.png")
plot.dist(dat.col = "mean_total_production_percap", variable.title = "Per cap production (t)")
dev.off()

png("Outputs/mean_total_production_perworker.png")
plot.dist(dat.col = "mean_total_production_perworker", variable.title = "Per worker production (t)")
dev.off()

png("Outputs/mean_capture_production_percap.png")
plot.dist(dat.col = "mean_capture_production_percap", variable.title = "Per cap capture production (t)")
dev.off()

png("Outputs/mean_aquaculture_percap.png")
plot.dist(dat.col = "mean_aquaculture_percap", variable.title = "Per cap aquaculture production (t)")
dev.off()

png("Outputs/direct_w_esitimated_ssf_percap.png")
plot.dist(dat.col = "direct_w_esitimated_ssf_percap", variable.title = "Per cap livelihoods")
dev.off()

png("Outputs/women_livelihoods_percap.png")
plot.dist(dat.col = "women_livelihoods_percap", variable.title = "Per cap women's livelihoods")
dev.off()

png("Outputs/indirect_w_esitimated_ssf_percap.png")
plot.dist(dat.col = "indirect_w_esitimated_ssf_percap", variable.title = "Per cap indirect livelihoods")
dev.off()

png("Outputs/fish_affordability.png")
plot.dist(dat.col = "fish_affordability", variable.title = "Relative affordability")
dev.off()

png("Outputs/mean_exports_tonnes_percap.png")
plot.dist(dat.col = "mean_exports_tonnes_percap", variable.title = "Per cap exports (t)")
dev.off()

png("Outputs/mean_exports_USD1000_percap.png")
plot.dist(dat.col = "mean_exports_USD1000_percap", variable.title = "Per cap exports (1000 USD)")
dev.off()

png("Outputs/mean_catch_nutrition_quality.png")
plot.dist(dat.col = "mean_catch_nutrition_quality", variable.title = "Mean catch quality")
dev.off()

png("Outputs/fish_supply_daily_g_protein_percap.png")
plot.dist(dat.col = "fish_supply_daily_g_protein_percap", variable.title = "Per cap supply (g protein)")
dev.off()

png("Outputs/mean_gdp.png")
plot.dist(dat.col = "mean_gdp", variable.title = "GDP")
dev.off()


