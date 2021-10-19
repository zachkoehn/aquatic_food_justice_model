# Livelihoods data cleaning
# Raw data from Teh and Sumaila SI Table 1

library(tidyverse)
library(countrycode)

# First load function for rebuilding from FishStat ZIP file:
rm(list=ls())

# note: Change outdir to the filepath where you want outputs to go
outdir <- "Outputs"
# for MacOS
datadir <- "/Volumes/jgephart/BFA Justice/Data/Livelihoods Teh and Sumaila"

df <- read.csv(file.path(datadir, "Teh_Sumaila_2013_SITable1.csv"))

df$iso3c <- countrycode(df$Country, origin = "country.name", destination = "iso3c")
df$iso3n <- countrycode(df$Country, origin = "country.name", destination = "iso3n")
df$year.range <- "2003"

df <- df %>%
  select(country = Country, year.range, iso3c, iso3n, direct_w_esitimated_ssf, indirect_w_esitimated_ssf) %>%
  mutate(direct_w_esitimated_ssf = 1000*direct_w_esitimated_ssf, 
         indirect_w_esitimated_ssf = 1000*indirect_w_esitimated_ssf)

write.csv(df, "Outputs/livelihoods_direct_indirect.csv", row.names = FALSE)
