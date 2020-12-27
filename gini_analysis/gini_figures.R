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
    log_mean_exports_USD1000_percap = log(mean_exports_USD1000_percap),
    
    mean_exports_tonnes_percap = mean_exports_tonnes/mean_population,
    log_mean_exports_tonnes_percap = log(mean_exports_tonnes_percap),
    
    direct_w_esitimated_ssf_percap = direct_w_esitimated_ssf/mean_population, 
    log_direct_w_esitimated_ssf_percap = log(direct_w_esitimated_ssf_percap),
    
    indirect_w_esitimated_ssf_percap = indirect_w_esitimated_ssf/mean_population,
    log_indirect_w_esitimated_ssf_percap = log(indirect_w_esitimated_ssf_percap),
    
    women_livelihoods_percap = women_livelihoods/mean_population,
    log_women_livelihoods_percap = log(women_livelihoods_percap),
    
    mean_total_production_percap = mean_total_production/mean_population, 
    log_mean_total_production_percap = log(mean_total_production_percap), 
    
    mean_aquaculture_percap = mean_aquaculture/mean_population,
    log_mean_aquaculture_percap = log(mean_aquaculture_percap),
    
    mean_capture_production_percap = mean_capture_production/mean_population,
    log_mean_capture_production_percap = log(mean_capture_production_percap),
    
    mean_total_production_perworker = 
      mean_total_production/(direct_w_esitimated_ssf+indirect_w_esitimated_ssf_percap),
    log_mean_total_production_perworker = log(mean_total_production_perworker)
  )

# Bind data with world data for mapping
map.world <- ne_countries(scale = "medium", returnclass = "sf")
map.world <- left_join(map.world, df, by = c("iso_a3" = "iso3c"))

#____________________________________________________________________________________________________#
# Create function for map with embedded hist and Gini
#____________________________________________________________________________________________________#

plot.dist <- function(dat.col, variable.title, log.dat.col = NULL, log.variable.title = NULL){
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
    geom_histogram(bins = 50) +
    theme_clean(base_size = 10) + 
    labs(x = paste(variable.title), y = "No. of countries")
  
  # Create unweighted Lorenz
  g_lorenz <- ggplot(df.world, aes(gini.col)) +
    stat_lorenz() +
    annotate_ineq(df.world$gini.col, x = .15, size = 2.5) +
    geom_abline(linetype = "dashed") +
    theme_clean(base_size = 10) + 
    labs(x = paste(variable.title), y = "Proportion of benefit")

  map.dat.col <- ifelse(is.null(log.dat.col) == TRUE, paste(dat.col), paste(log.dat.col))
  map.lab <- ifelse(is.null(log.variable.title) == TRUE, paste(variable.title), paste(log.variable.title))
  
  # Create map
  g_map <- ggplot(data = map.world) +
    geom_sf(aes_string(fill = map.dat.col)) +
    scale_fill_viridis_c(option = "plasma") +
    labs(fill = paste(map.lab)) +
    theme(axis.line = element_blank(), axis.text = element_blank(), 
          axis.ticks = element_blank(), axis.title = element_blank(), 
          panel.background = element_blank(), panel.border = element_blank(), 
          panel.grid = element_blank(), panel.spacing = unit(0, 
                                                             "lines"), plot.background = element_blank(), 
          legend.justification = c(0, 0), legend.position = "bottom")
    
  grid.arrange(grobs = list(g_hist, g_lorenz, g_map),
               layout_matrix = rbind(c(1, 3, 3, 3),
                                     c(2, 3, 3, 3)))
}

png("Outputs/mean_total_production_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "mean_total_production_percap", variable.title = "Per cap production (t)",
          log.dat.col = "log_mean_total_production_percap", 
          log.variable.title = "Per cap production (log(t))")
dev.off()

png("Outputs/mean_total_production_perworker.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "mean_total_production_perworker", variable.title = "Per worker production (t)",
          log.dat.col = "log_mean_total_production_perworker", 
          log.variable.title = "Per worker production (log(t))")
dev.off()

png("Outputs/mean_capture_production_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "mean_capture_production_percap", variable.title = "Per cap capture production (t)",
          log.dat.col = "log_mean_capture_production_percap", 
          log.variable.title = "Per cap capture production (log(t))")
dev.off()

png("Outputs/mean_aquaculture_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "mean_aquaculture_percap", variable.title = "Per cap aquaculture production (t)",
          log.dat.col = "log_mean_aquaculture_percap", 
          log.variable.title = "Per cap aquaculture production (log(t))")
dev.off()

png("Outputs/direct_w_esitimated_ssf_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "direct_w_esitimated_ssf_percap", variable.title = "Per cap direct livelihoods",
          log.dat.col = "log_direct_w_esitimated_ssf_percap", 
          log.variable.title = "Per cap direct livelihoods (log(workers/cap))")
dev.off()

png("Outputs/women_livelihoods_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "women_livelihoods_percap", variable.title = "Per cap women's livelihoods",
          log.dat.col = "log_women_livelihoods_percap", 
          log.variable.title = "Per cap women's livelihoods (log(workers/cap))")
dev.off()

png("Outputs/indirect_w_esitimated_ssf_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "indirect_w_esitimated_ssf_percap", variable.title = "Per cap indirect livelihoods",
          log.dat.col = "log_indirect_w_esitimated_ssf_percap", 
          log.variable.title = "Per cap indirect livelihoods (log(workers/cap))")
dev.off()

png("Outputs/fish_affordability.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "fish_affordability", variable.title = "Relative affordability")
dev.off()

png("Outputs/mean_exports_tonnes_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "mean_exports_tonnes_percap", variable.title = "Per cap exports (t)",
          log.dat.col = "log_mean_exports_tonnes_percap", 
          log.variable.title = "Per cap exports (log(t))")
dev.off()

png("Outputs/mean_exports_USD1000_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "mean_exports_USD1000_percap", variable.title = "Per cap exports (1000 USD)",
          log.dat.col = "log_mean_exports_USD1000_percap", 
          log.variable.title = "Per cap exports (log(1000 USD))")
dev.off()

png("Outputs/mean_catch_nutrition_quality.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "mean_catch_nutrition_quality", variable.title = "Mean catch quality")
dev.off()

png("Outputs/fish_supply_daily_g_protein_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "fish_supply_daily_g_protein_percap", variable.title = "Per cap supply (g protein)")
dev.off()

png("Outputs/mean_gdp.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "mean_gdp", variable.title = "GDP")
dev.off()


