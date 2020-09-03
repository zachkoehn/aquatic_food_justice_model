# Title: Data exploration 
# Author: JAG
# Date: 21-Aug-20
# Notes
# Zach - something weird is happening with China (it appears twice) and even when adjusting for that,
# it seems like the percent of population there is too high
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
df <- read.csv("all_national_indicators.csv")
df <- df %>%
  filter(!is.na(iso3c)) # FIX IT: Ask Zach about NAs in ISO column
pop <- read.csv("FAOSTAT_population_data_8-21-2020.csv")

#____________________________________________________________________________________________________#
# Filter population and join to df
#____________________________________________________________________________________________________#
pop$iso3c <- countrycode(pop$Area, origin = "country.name", destination = "iso3c")
# FIX IT: Check unmatched countries with country list in df

pop_06_16 <- pop %>%
  filter(Year %in% c(2006:2016)) %>%
  group_by(iso3c) %>% 
  summarise(pop_06_16 = 1000*sum(Value)/length(c(2006:2016)))

pop_14_16 <- pop %>%
  filter(Year %in% c(2014:2016)) %>%
  group_by(iso3c) %>% 
  summarise(pop_14_16 = 1000*sum(Value)/length(c(2014:2016)))

df <- df %>%
  select(iso3c, fish_supply_daily_g_protein_percap, mean_exports_USD1000, mean_exports_tonnes, 
         mean_capture_production, mean_aquaculture_production_freshwater, mean_aquaculture_production_brackish,
         mean_aquaculture_production_marine) %>%
  left_join(pop_06_16, by = "iso3c") %>%
  left_join(pop_14_16, by = "iso3c") 

# Replace NA's with zeros in appropriate columns (need to check this carefully--> currently just doing everywhere)
df[is.na(df)] <- 0

# Convert consumption to total
df$total_supply_gprotein <- df$fish_supply_daily_g_protein_percap*df$pop_14_16

#____________________________________________________________________________________________________#
# Plot Lorenz curves 
#____________________________________________________________________________________________________#
plot.lc <- function(plot.df, plot.col, pop.col){
  plot.df <- df
  colnames(plot.df)[colnames(plot.df) == plot.col] <- "value"
  colnames(plot.df)[colnames(plot.df) == pop.col] <- "pop"
  
  plot.df <- plot.df %>%
    select(iso3c, value, pop) %>% 
    group_by(iso3c) %>% 
    summarise(value = mean(value), pop = mean(pop)) %>%
    mutate(value_percap = value/pop) %>%
    filter(pop >0)
  
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
plot.lc(plot.df = df, plot.col = "mean_capture_production", pop.col = "pop_06_16")
plot.lc(plot.df = df, plot.col = "mean_aquaculture_production_freshwater", pop.col = "pop_06_16")
plot.lc(plot.df = df, plot.col = "mean_aquaculture_production_brackish", pop.col = "pop_06_16")
plot.lc(plot.df = df, plot.col = "mean_aquaculture_production_marine", pop.col = "pop_06_16")

# Distribution
plot.lc(plot.df = df, plot.col = "mean_exports_USD1000", pop.col = "pop_06_16")
plot.lc(plot.df = df, plot.col = "mean_exports_tonnes", pop.col = "pop_06_16")

# Consumption
plot.lc(plot.df = df, plot.col = "total_supply_gprotein", pop.col = "pop_14_16")



