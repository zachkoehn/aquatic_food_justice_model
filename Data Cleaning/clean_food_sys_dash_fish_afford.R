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


dat_clean <- dat_raw %>%
	select(AreaID,AreaName,DataValue) %>%
	rename(
		country_code=AreaID,
		country_name_en=AreaName,
		fish_relative_caloric_price=DataValue
		) %>%
	mutate(
	    iso3c=countrycode(country_name_en,"country.name","iso3c"), # assign iso3c from country name
	    iso3n=countrycode(iso3c,"iso3c","iso3n"), # assign iso3n from iso3c
	    year_range="snapshot" #add the year_range category
		) %>%
	select(country_name_en,iso3c,iso3n,fish_relative_caloric_price,year_range)

write.csv(dat_clean,
	 file.path(directory,
	    "data",
	    "data_clean",
	    "fish_relative_affordability_snapshot.csv"
  	),
  	row.names=FALSE
	)