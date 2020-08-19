#######################
# Cleaning fish affordability data
# from Heady & Alderman
# Zach Koehn
# zkoehn@stanford.edu
#######################

# load dataset and packages
library(tidyverse)
library(here)
library(countrycode)


rm(list=ls())
directory <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model"
setwd(directory)


# data from Heady & Alderman 2019 https://doi.org/10.1093/jn/nxz158
dat_raw <- read.csv(
  file.path(directory,
    "data",
    "data_raw",
    "heady_alderman_rcp",
    "Relative caloric price_fish_food_systems_dash_fr_Heady_and_Alderman.csv"
  ),
  header=TRUE
)



