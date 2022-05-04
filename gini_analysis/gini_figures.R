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
library(cowplot)
library(colortools)
library(ggpubr)
library(ggtextures)
library(magick)

#____________________________________________________________________________________________________#
# Load data
#____________________________________________________________________________________________________#
df <- read.csv("all_national_indicators.csv")

df <- df %>%
  mutate(mean_aquaculture = sum(mean_aquaculture_production_freshwater, 
                                mean_aquaculture_production_marine, 
                                mean_aquaculture_production_brackish, na.rm = TRUE),
         women_livelihoods = direct_w_esitimated_ssf*female_particip_ssf) %>%
  # Remove when all_indicators is updated
  mutate(direct_w_esitimated_ssf = 1000*direct_w_esitimated_ssf,
         indirect_w_esitimated_ssf = 1000*indirect_w_esitimated_ssf) %>%
  # Mutate all variables to per capita, as necessary
  mutate(
    mean_exports_USD1000_percap = mean_exports_USD1000/mean_population,
    log_mean_exports_USD1000_percap = log(mean_exports_USD1000_percap),
    
    mean_exports_tonnes_percap = mean_exports_tonnes/mean_population,
    log_mean_exports_tonnes_percap = log(mean_exports_tonnes_percap),
    
    mean_imports_tonnes_percap = mean_imports_tonnes/mean_population,
    log_mean_imports_tonnes_percap = log(mean_imports_tonnes_percap),
    
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
    log_mean_total_production_perworker = log(mean_total_production_perworker),
    
    log_fish_supply_daily_g_protein_percap = log(fish_supply_daily_g_protein_percap)
  )

# Bind data with world data for mapping
map.world <- ne_countries(scale = "medium", returnclass = "sf")
map.world <- left_join(map.world, df, by = c("iso_a3" = "iso3c"))

#____________________________________________________________________________________________________#
# Create function for map with embedded hist and Gini
#____________________________________________________________________________________________________#

base_size <- 10
base_family <- "Helvetica Neue"

plot.dist <- function(dat.col, variable.title, log.dat.col = NULL, log.variable.title = NULL, main.title = ""){
  # Calculate Gini
  df.world <- as.data.frame(map.world)
  df.world <- data.frame(gini.col = c(df.world[[dat.col]]), 
                         pop = df.world$mean_population)
  df.world <- df.world %>%
    drop_na()
  # Compare weighted gini
  gini <- gini(x = df.world$gini.col, weights = df.world$pop) 
  gini(x = df.world$gini.col) 
  
  # Create histogram
  g_hist <- ggplot(df.world, aes(x = gini.col)) +
    geom_histogram(bins = 50) +
    labs(x = paste(variable.title), y = "No. of countries") + 
    theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"), 
            axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"), 
            axis.text = element_text(size = ceiling(base_size*0.7), colour = "black"),
            axis.title = element_text(size = ceiling(base_size*0.8)), 
            panel.grid.minor = element_blank(), 
            panel.grid.major.y = element_blank(), 
            panel.grid.major.x = element_blank(), 
            panel.background = element_blank(), panel.border = element_blank(), 
            strip.background = element_rect(linetype = 0), strip.text = element_text(), 
            strip.text.x = element_text(vjust = 0.5), strip.text.y = element_text(angle = -90), 
            legend.text = element_text(size = ceiling(base_size*0.9), family = "sans"), 
            legend.title = element_blank(), 
            legend.key = element_rect(fill = "white", colour = NA), 
            legend.position="bottom",
            plot.title = element_text(size = ceiling(base_size*1.1), face = "bold"), 
            plot.subtitle = element_text(size = ceiling(base_size*1.05)))
  
  # Create unweighted Lorenz
  g_lorenz <- ggplot(df.world, aes(gini.col)) +
    stat_lorenz() +
    annotate_ineq(df.world$gini.col, x = .15, size = 2.5) +
    geom_abline(linetype = "dashed") +
    labs(x = paste(variable.title), y = "Proportion of benefit") + 
    theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"), 
          axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"), 
          axis.text = element_text(size = ceiling(base_size*0.7), colour = "black"),
          axis.title = element_text(size = ceiling(base_size*0.8)), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.major.x = element_blank(), 
          panel.background = element_blank(), panel.border = element_blank(), 
          strip.background = element_rect(linetype = 0), strip.text = element_text(), 
          strip.text.x = element_text(vjust = 0.5), strip.text.y = element_text(angle = -90), 
          legend.text = element_text(size = ceiling(base_size*0.9), family = "sans"), 
          legend.title = element_blank(), 
          legend.key = element_rect(fill = "white", colour = NA), 
          legend.position="bottom",
          plot.title = element_text(size = ceiling(base_size*1.1), face = "bold"), 
          plot.subtitle = element_text(size = ceiling(base_size*1.05)))

  map.dat.col <- ifelse(is.null(log.dat.col) == TRUE, paste(dat.col), paste(log.dat.col))
  map.lab <- ifelse(is.null(log.variable.title) == TRUE, paste(variable.title), paste(log.variable.title))
  
  # Create map
  g_map <- ggplot(data = map.world) +
    geom_sf(aes_string(fill = map.dat.col), size = 0.1) +
    scale_fill_gradient(low = "white", high = "#70468C", na.value = "#A69569") +
    #scale_fill_gradientn(colours = c("#FFD947", "#FFE78B", "#FFF3C4", "#FFFBEC", "#F3F5F6", "#C3CAD3", "#758699", "#364F6B")) +
    labs(fill = paste(map.lab)) +
    coord_sf(ylim = c(-50, 90), datum = NA) +
    theme(axis.line = element_blank(), axis.text = element_blank(), 
          axis.ticks = element_blank(), axis.title = element_blank(), 
          panel.background = element_blank(), panel.border = element_blank(), 
          panel.grid = element_blank(), panel.spacing = unit(0, 
                                                             "lines"), plot.background = element_blank(), 
          legend.justification = c(0, 0), legend.position = "bottom")
    
  # grid.arrange(grobs = list(g_hist, g_lorenz, g_map),
  #              layout_matrix = rbind(c(1, 3, 3, 3),
  #                                    c(2, 3, 3, 3)))
  
  # # Layout for with histogram
  # ggdraw() +
  #   draw_plot(
  #     g_map, x=0, y=0.5, hjust = 0, vjust=0.5, height = .8, width=.8
  #   ) +
  #   draw_plot(
  #     g_hist, x=.68, y = .65, height = .28, width = .28, hjust = 0, vjust=0.5
  #   ) +
  #   draw_plot(
  #     g_lorenz, x=.68, y = .35, height = .28, width = .28, hjust = 0, vjust=0.5
  #   ) + 
  #   geom_text(data = data.frame(x = 0.05, y = .86, label = paste(main.title)),
  #             aes(x, y, label = label),
  #             hjust = 0, vjust = 0, angle = 0, size = .5*base_size, fontface="bold",
  #             color = "black",
  #             inherit.aes = FALSE,
  #             family= base_family)
  
  # Layout without histogram
  ggdraw() +
    draw_plot(
      g_map, x=0, y=0.5, hjust = 0, vjust=0.5, height = 0.9, width = 1
    ) +
    draw_plot(
      g_lorenz, x=0.05, y = 0.45, height = .4, width = .25, hjust = 0, vjust=0.55
    ) + 
    geom_text(data = data.frame(x = 0.05, y = .86, label = paste(main.title)),
              aes(x, y, label = label),
              hjust = 0, vjust = 0, angle = 0, size = .5*base_size, fontface="bold",
              color = "black",
              inherit.aes = FALSE,
              family= base_family)

  }

png("Outputs/mean_total_production_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "mean_total_production_percap", variable.title = "Production (t/capita)",
          log.dat.col = "log_mean_total_production_percap", 
          log.variable.title = "log(t/capita)", main.title = "Production per capita")
dev.off()

png("Outputs/mean_total_production_perworker.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "mean_total_production_perworker", variable.title = "Production (t/worker)",
          log.dat.col = "log_mean_total_production_perworker", 
          log.variable.title = "log(t/worker)", main.title = "a")
dev.off()

png("Outputs/mean_capture_production_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "mean_capture_production_percap", variable.title = "Per cap capture production (t)",
          log.dat.col = "log_mean_capture_production_percap", 
          log.variable.title = "Per cap capture production (log(t))", main.title = "Capture production per capita")
dev.off()

png("Outputs/mean_aquaculture_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "mean_aquaculture_percap", variable.title = "Per cap aquaculture production (t)",
          log.dat.col = "log_mean_aquaculture_percap", 
          log.variable.title = "Per cap aquaculture production (log(t))", main.title = "Aquaculture production per capita")
dev.off()

png("Outputs/direct_w_esitimated_ssf_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "direct_w_esitimated_ssf_percap", variable.title = "Per cap direct livelihoods",
          log.dat.col = "log_direct_w_esitimated_ssf_percap", 
          log.variable.title = "Per cap direct livelihoods (log(workers/cap))", main.title = "Direct livelihoods per capita")
dev.off()

png("Outputs/women_livelihoods_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "women_livelihoods_percap", variable.title = "Per cap women's livelihoods",
          log.dat.col = "log_women_livelihoods_percap", 
          log.variable.title = "Per cap women's livelihoods (log(workers/cap))", main.title = "Women's livelihoods per capita")
dev.off()

png("Outputs/indirect_w_esitimated_ssf_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "indirect_w_esitimated_ssf_percap", variable.title = "Per cap indirect livelihoods",
          log.dat.col = "log_indirect_w_esitimated_ssf_percap", 
          log.variable.title = "Per cap indirect livelihoods (log(workers/cap))", main.title = "Indirect livelihoods per capita")
dev.off()

png("Outputs/fish_affordability.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "fish_affordability", variable.title = "Relative affordability", main.title = "Relative affordability")
dev.off()

png("Outputs/mean_exports_tonnes_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "mean_exports_tonnes_percap", variable.title = "Per cap exports (t)",
          log.dat.col = "log_mean_exports_tonnes_percap", 
          log.variable.title = "Per cap exports (log(t))", main.title = "b")
dev.off()

png("Outputs/mean_exports_USD1000_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "mean_exports_USD1000_percap", variable.title = "Per cap exports (1000 USD)",
          log.dat.col = "log_mean_exports_USD1000_percap", 
          log.variable.title = "Per cap exports (log(1000 USD))", main.title = "Exports per capita ($)")
dev.off()

png("Outputs/mean_catch_nutrition_quality.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "mean_catch_nutrition_quality", variable.title = "Mean catch quality", main.title = "Catch quality")
dev.off()

png("Outputs/fish_supply_daily_g_protein_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "fish_supply_daily_g_protein_percap", variable.title = "Per cap supply (g protein)", main.title = "c")
dev.off()

png("Outputs/log_fish_supply_daily_g_protein_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "fish_supply_daily_g_protein_percap", variable.title = "Per cap supply (g protein)",
          log.dat.col = "log_fish_supply_daily_g_protein_percap", 
          log.variable.title = "Per cap supply (log(g protein))", main.title = "c")
dev.off()

png("Outputs/mean_gdp.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist(dat.col = "mean_gdp", variable.title = "GDP", main.title = "GDP")
dev.off()


#____________________________________________________________________________________________________#
# Create function for map with embedded Gini with log scale
#____________________________________________________________________________________________________#

base_size <- 10
base_family <- "Helvetica Neue"

plot.dist.log <- function(dat.col, variable.title, log.dat.col = NULL, log.variable.title = NULL, 
                          main.title = "", ramp_color = "darkblue"){
  # Calculate Gini
  df.world <- as.data.frame(map.world)
  df.world <- data.frame(gini.col = c(df.world[[dat.col]]), 
                         pop = df.world$mean_population)
  df.world <- df.world %>%
    drop_na()
  gini <- gini(x = df.world$gini.col, weights = df.world$pop) 
  gini_annotate <- gini(x = df.world$gini.col) 
  
  # Create histogram
  g_hist <- ggplot(df.world, aes(x = gini.col)) +
    geom_histogram(bins = 50) +
    labs(x = paste(variable.title), y = "No. of countries") + 
    theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"), 
          axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"), 
          axis.text = element_text(size = ceiling(base_size*0.7), colour = "black"),
          axis.title = element_text(size = ceiling(base_size*0.8)), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.major.x = element_blank(), 
          panel.background = element_blank(), panel.border = element_blank(), 
          strip.background = element_rect(linetype = 0), strip.text = element_text(), 
          strip.text.x = element_text(vjust = 0.5), strip.text.y = element_text(angle = -90), 
          legend.text = element_text(size = ceiling(base_size*0.9), family = "sans"), 
          legend.title = element_blank(), 
          legend.key = element_rect(fill = "white", colour = NA), 
          legend.position="bottom",
          plot.title = element_text(size = ceiling(base_size*1.1), face = "bold"), 
          plot.subtitle = element_text(size = ceiling(base_size*1.05)))
  
  # Create unweighted Lorenz
  g_lorenz <- ggplot(df.world, aes(gini.col)) +
    stat_lorenz() +
    #annotate_ineq(df.world$gini.col, x = .15, size = 2.5) + # this version of annotate drops the 0 in "0.90"
    annotate("text", label = paste("Gini: ", substr(as.character(gini_annotate), 1, 4), sep = ""), x = .15, y = 0.95, size = 2.5) + # set the number of digits with substr
    geom_abline(linetype = "dashed") +
    labs(x = paste(variable.title), y = "Proportion of benefit") + 
    theme(axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"), 
          axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"), 
          axis.text = element_text(size = ceiling(base_size*0.7), colour = "black"),
          axis.title = element_text(size = ceiling(base_size*0.8)), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.major.x = element_blank(), 
          panel.background = element_blank(), panel.border = element_blank(), 
          strip.background = element_rect(linetype = 0), strip.text = element_text(), 
          strip.text.x = element_text(vjust = 0.5), strip.text.y = element_text(angle = -90), 
          legend.text = element_text(size = ceiling(base_size*0.9), family = "sans"), 
          legend.title = element_blank(), 
          legend.key = element_rect(fill = "white", colour = NA), 
          legend.position="bottom",
          plot.title = element_text(size = ceiling(base_size*1.1), face = "bold"), 
          plot.subtitle = element_text(size = ceiling(base_size*1.05)))
  
  map.dat.col <- ifelse(is.null(log.dat.col) == TRUE, paste(dat.col), paste(log.dat.col))
  map.lab <- ifelse(is.null(log.variable.title) == TRUE, paste(variable.title), paste(log.variable.title))
  
  # Create map
  g_map <- ggplot(data = map.world) +
    geom_sf(aes_string(fill = map.dat.col), size = 0.1) +
    scale_fill_gradient(low = "white", high = ramp_color, na.value = "#E4DDCF", trans = "log10") +
    #scale_fill_gradientn(colours = c("#FFD947", "#FFE78B", "#FFF3C4", "#FFFBEC", "#F3F5F6", "#C3CAD3", "#758699", "#364F6B"),
    #                     trans = "log10") +
    labs(fill = paste(map.lab)) +
    coord_sf(ylim = c(-50, 90), datum = NA) +
    guides(fill = guide_colourbar(barwidth = 10, barheight = 0.5)) +
    theme(axis.line = element_blank(), axis.text = element_blank(), 
          axis.ticks = element_blank(), axis.title = element_blank(), 
          panel.background = element_blank(), panel.border = element_blank(), 
          panel.grid = element_blank(), panel.spacing = unit(0, 
                                                             "lines"), plot.background = element_blank(), 
          legend.justification = c(0, 0), legend.position = "bottom")
  
  # Layout without histogram
  ggdraw() +
    draw_plot(
      g_map, x=0.05, y=0.5, hjust = 0, vjust=0.5,
    ) +
    draw_plot(
      g_lorenz, x=0.05, y = 0.45, height = .4, width = .25, hjust = 0, vjust=0.55
    ) + 
    geom_text(data = data.frame(x = 0.05, y = .86, label = paste(main.title)),
              aes(x, y, label = label),
              hjust = 0, vjust = 0, angle = 0, size = .5*base_size, fontface="bold",
              color = "black",
              inherit.aes = FALSE,
              family= base_family)
  
}


png("Outputs/Fig_1.png", width = 7, height = 10, units = 'in', res = 300)
a <- plot.dist.log(dat.col = "mean_total_production_perworker", variable.title = "Production (t/worker)",
          main.title = "", ramp_color = "#00BCC6")
b <- plot.dist.log(dat.col = "mean_exports_USD1000_percap", variable.title = "Per cap exports (1000 USD)",
                   main.title = "", ramp_color = "#70468C")
c <- plot.dist.log(dat.col = "fish_supply_daily_g_protein_percap", variable.title = "Protein supply (g/cap)",
               main.title = "", ramp_color = "#16CC52")
ggarrange(a, b, c, labels = c("a", "b", "c"), ncol = 1)
dev.off()

pdf("Outputs/Fig_1.pdf")
a <- plot.dist.log(dat.col = "mean_total_production_perworker", variable.title = "Production (t/worker)",
                   main.title = "", ramp_color = "#00BCC6")
b <- plot.dist.log(dat.col = "mean_exports_USD1000_percap", variable.title = "Per cap exports (1000 USD)",
                   main.title = "", ramp_color = "#70468C")
c <- plot.dist.log(dat.col = "fish_supply_daily_g_protein_percap", variable.title = "Protein supply (g/cap)",
                   main.title = "", ramp_color = "#16CC52")
ggarrange(a, b, c, labels = c("a", "b", "c"), ncol = 1)
dev.off()

png("Outputs/SIFig_inequality_maps.png", width = 7, height = 10, units = 'in', res = 300)
a <- plot.dist.log(dat.col = "mean_total_production_percap", variable.title = "Production (t/capita)",
               main.title = "")
b <- plot.dist.log(dat.col = "direct_w_esitimated_ssf_percap", variable.title = "Per cap direct livelihoods",
                   main.title = "")
c <- plot.dist.log(dat.col = "indirect_w_esitimated_ssf_percap", variable.title = "Per cap indirect livelihoods",
               main.title = "")
d <- plot.dist.log(dat.col = "women_livelihoods_percap", variable.title = "Per cap women's livelihoods",
               main.title = "")
e <- plot.dist.log(dat.col = "mean_exports_tonnes_percap", variable.title = "Exports (t/cap)",
                   main.title = "")
ggarrange(a, b, c, d, e, labels = c("a", "b", "c", "d", "e"), ncol = 1)
dev.off()

pdf("Outputs/SIFig_inequality_maps.pdf")
a <- plot.dist.log(dat.col = "mean_total_production_percap", variable.title = "Production (t/capita)",
                   main.title = "")
b <- plot.dist.log(dat.col = "direct_w_esitimated_ssf_percap", variable.title = "Per cap direct livelihoods",
                   main.title = "")
c <- plot.dist.log(dat.col = "indirect_w_esitimated_ssf_percap", variable.title = "Per cap indirect livelihoods",
                   main.title = "")
d <- plot.dist.log(dat.col = "women_livelihoods_percap", variable.title = "Per cap women's livelihoods",
                   main.title = "")
e <- plot.dist.log(dat.col = "mean_exports_tonnes_percap", variable.title = "Exports (t/cap)",
                   main.title = "")
ggarrange(a, b, c, d, e, labels = c("a", "b", "c", "d", "e"), ncol = 1)
dev.off()

# Extra plots
png("Outputs/mean_capture_production_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist.log(dat.col = "mean_capture_production_percap", variable.title = "Per cap capture production (t)",
          main.title = "Capture production per capita")
dev.off()

png("Outputs/mean_aquaculture_percap.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist.log(dat.col = "mean_aquaculture_percap", variable.title = "Per cap aquaculture production (t)",
          main.title = "Aquaculture production per capita")
dev.off()

png("Outputs/fish_affordability.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist.log(dat.col = "fish_affordability", variable.title = "Relative affordability", 
          main.title = "Relative affordability")
dev.off()

png("Outputs/mean_catch_nutrition_quality.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist.log(dat.col = "mean_catch_nutrition_quality", variable.title = "Mean catch quality", main.title = "Catch quality")
dev.off()

png("Outputs/mean_gdp.png", width = 8, height = 3.5, units = 'in', res = 300)
plot.dist.log(dat.col = "mean_gdp", variable.title = "GDP", main.title = "GDP")
dev.off()


#____________________________________________________________________________________________________#
# Summary stats
#____________________________________________________________________________________________________#
df_export_USD1000 <- df %>% 
  select(country_name_en, mean_exports_USD1000) %>%
  filter(!is.na(mean_exports_USD1000)) %>%
  arrange(desc(mean_exports_USD1000)) %>%
  mutate(cum_sum = cumsum(mean_exports_USD1000)) %>%
  mutate(cum_per = 100*cum_sum/sum(mean_exports_USD1000))

df_export_tonnes <- df %>% 
  select(country_name_en, mean_exports_tonnes) %>%
  filter(!is.na(mean_exports_tonnes)) %>%
  arrange(desc(mean_exports_tonnes)) %>%
  mutate(cum_sum = cumsum(mean_exports_tonnes)) %>%
  mutate(cum_per = 100*cum_sum/sum(mean_exports_tonnes))

df %>% 
  select(country_name_en, mean_total_production_perworker) %>%
  arrange(desc(mean_total_production_perworker))


df %>% 
  select(country_name_en, mean_total_production_percap) %>%
  arrange(desc(mean_total_production_percap))


#____________________________________________________________________________________________________#
# Table comparison of weighted and unweighted gini
#____________________________________________________________________________________________________#

df.world <- as.data.frame(map.world)
df.world <- df.world %>%
  select(mean_population, mean_total_production_perworker, mean_exports_USD1000_percap, 
         fish_supply_daily_g_protein_percap, mean_total_production_percap, 
         direct_w_esitimated_ssf_percap, indirect_w_esitimated_ssf_percap, 
         women_livelihoods_percap, mean_exports_tonnes_percap) 

gini.table <- data.frame(variable = c("mean_total_production_perworker", "mean_exports_USD1000_percap", 
                                      "fish_supply_daily_g_protein_percap", "mean_total_production_percap", 
                                      "direct_w_esitimated_ssf_percap", "indirect_w_esitimated_ssf_percap", 
                                      "women_livelihoods_percap", "mean_exports_tonnes_percap"), 
                         unweighted.gini = numeric(length = 8), 
                         weighted.gini = numeric(length = 8))

for(i in 1:nrow(gini.table)){
  loop.df <- df.world %>%
    select(var_col = paste(gini.table$variable[i]), mean_population) %>%
    drop_na()
  
  gini.table$unweighted.gini[i] <- gini(x = loop.df$var_col) 
  
  gini.table$weighted.gini[i] <- gini(x = loop.df$var_col,
                                      weights = loop.df$mean_population) 
}

write.csv(gini.table, "gini_comparison.csv")
