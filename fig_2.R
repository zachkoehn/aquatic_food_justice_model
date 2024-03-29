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
library(here)

#____________________________________________________________________________________________________#
# Load data
#____________________________________________________________________________________________________#

df <- read.csv(here("all_national_indicators.csv"))

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

# dot-based solution for smaller island countries, based on script from Juliano Palacios Abrantes 
map.islands <- map.world %>% 
  filter(subregion %in% c("Micronesia","Polynesia")) %>%
  st_cast("POINT") # transform country polygons into points

#____________________________________________________________________________________________________#
# Create function for map with embedded Gini with log scale
#____________________________________________________________________________________________________#

# custom presets
base_size <- 10
base_family <- "Helvetica Neue"

# plot to create log-scale map gini
plot.dist.log <- function(dat.col, variable.title, log.dat.col = NULL, log.variable.title = NULL, main.title = "",color_high,color_low){
  
  # dat.col = "mean_total_production_perworker"; variable.title = "Production (t/worker)";
  # main.title = "";log.dat.col = NULL; log.variable.title = NULL;
  # color_low="#cbeef0";color_high="#1e666a"
  # Calculate Gini
  df.world <- as.data.frame(map.world)
  df.world <- data.frame(gini.col = c(df.world[[dat.col]]), 
                         pop = df.world$mean_population)
  df.world <- df.world %>%
    drop_na()
  # Can add population weighting, but https://doi.org/10.1080/17421772.2017.1343491 argues against
  gini <- gini(x = df.world$gini.col, weights = df.world$pop) 
  gini_annotate <- gini(x = df.world$gini.col) 
  
  # Create histogram
  g_hist <- ggplot(df.world, aes(x = gini.col)) +
    geom_histogram(bins = 50) +
    labs(x = paste(variable.title), y = "No. of countries") + 
    theme(
      axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
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
      plot.subtitle = element_text(size = ceiling(base_size*1.05))
    )
  
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
  
  PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
  map.world.reproj <- st_transform(map.world,PROJ) %>%
    filter(!subregion %in% c("Micronesia","Polynesia"))
  map.islands.reproj<- st_transform(map.islands,PROJ)
  poly_points <- rbind(map.world.reproj,map.islands.reproj) %>%
    filter(continent !="Antarctica")
  
  
  # Create map
  g_map <- ggplot(data = poly_points) +
    geom_sf(aes_string(fill = map.dat.col,color=map.dat.col), size = .1) +
    scale_fill_gradient(low = color_low, high = color_high,trans="log10",na.value = "#dfd9c9") +
    scale_color_gradient(low = color_low, high = color_high,trans="log10",na.value = "#dfd9c9") +
    labs(fill = paste(map.lab),color=paste(map.lab)) +
    # coord_sf(ylim = c(-50, 90), datum = NA) +
    guides(
      fill = guide_colourbar(barwidth = 10, barheight = 0.5),
      color = guide_colourbar(barwidth = 10, barheight = 0.5)) +
    theme(
      axis.line = element_blank(), axis.text = element_blank(), 
      axis.ticks = element_blank(), axis.title = element_blank(), 
      panel.background = element_blank(),
      # panel.background = element_rect(fill = "black"),
      panel.border = element_blank(), 
      panel.grid = element_blank(), panel.spacing = unit(0, "lines"), plot.background = element_blank(), 
      legend.justification = c(0.5, 0), legend.position = "bottom",
      legend.title= element_text(size = ceiling(base_size*0.8)),
      legend.text = element_text(size = ceiling(base_size*0.7))
    )
  
  # Layout without histogram
  ggdraw() +
    draw_plot(
      g_lorenz, x=0.01, y = .25, height = .4, width = .25, hjust = 0, vjust=0.55
    ) + 
    draw_plot(
      g_map, x=0.05, y=0.5, hjust = 0, vjust=0.5,
    ) +
    geom_text(data = data.frame(x = 0.05, y = .86, label = paste(main.title)),
              aes(x, y, label = label),
              hjust = 0, vjust = 0, angle = 0, size = .5*base_size, fontface="bold",
              color = "black",
              inherit.aes = FALSE,
              family= base_family)
  
}

# png(here("gini_analysis/outputs/Fig_2_redo.png"), width = 7, height = 10, units = 'in', res = 300)
# a <- plot.dist.log(dat.col = "mean_total_production_perworker", variable.title = "Production (t/worker)",
#                    main.title = "",color_low="#cbeef0",color_high="#1e666a")
# b <- plot.dist.log(dat.col = "mean_exports_USD1000_percap", variable.title = "Per cap exports (1000 USD)",
#                    main.title = "",color_low="#ded0e8",color_high="#70468C")
# c <- plot.dist.log(dat.col = "fish_supply_daily_g_protein_percap", variable.title = "Protein supply (g/cap)",
#                    main.title = "",color_low="#a7e7be",color_high="#248446")
# ggarrange(a, b, c, labels = c("a", "b", "c"), ncol = 1)
# dev.off()


a <- plot.dist.log(dat.col = "mean_total_production_perworker", variable.title = "Production (t/worker)",
                   main.title = "",color_low="#cbeef0",color_high="#1e666a")
b <- plot.dist.log(dat.col = "mean_exports_USD1000_percap", variable.title = "Per cap exports (1000 USD)",
                   main.title = "",color_low="#ded0e8",color_high="#70468C")
c <- plot.dist.log(dat.col = "fish_supply_daily_g_protein_percap", variable.title = "Protein supply (g/cap)",
                   main.title = "",color_low="#a7e7be",color_high="#248446")
figure_2 <- ggarrange(a, b, c, labels = c("a", "b", "c"), ncol = 1)

pdf(here("outputs/figure_2_nature_food.pdf"), width = 7, height = 10)
figure_2
dev.off()

  #____________________________________________________________________________________________________#
  # Summary stats
  #____________________________________________________________________________________________________#
  df_export <- df %>% 
    select(country_name_en, mean_exports_USD1000) %>%
    filter(!is.na(mean_exports_USD1000)) %>%
    arrange(desc(mean_exports_USD1000)) %>%
    mutate(cum_sum = cumsum(mean_exports_USD1000)) %>%
    mutate(cum_per = 100*cum_sum/sum(mean_exports_USD1000))
  
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
  