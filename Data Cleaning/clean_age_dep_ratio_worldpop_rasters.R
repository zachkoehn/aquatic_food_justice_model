#######################
# Cleaning national age dependency ratios extracted from Worldpop
# from Worldpop dataset
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


# data from worldpop https://www.worldpop.org/project/categories?id=8
dat_raw <- read.csv(
  file.path(directory,
    "data",
    "data_raw",
    "worldpop",
    "age_pop_rasters",
    "national_age_dep_satellite_stats.csv"
  ),
  header=TRUE
)


dat_clean <- dat_raw %>%
	select(iso_a2,age_2006_mean,age_2016_mean) %>%
	rowwise() %>%
	mutate(
	    iso3c=countrycode(iso_a2,"iso2c","iso3c"),
	    country_name_en=countrycode(iso3c,"iso3c","country.name"), # assign iso3c from country name
	    iso3n=countrycode(iso3c,"iso3c","iso3n"), # assign iso3n from iso3c
	    year_range="2016and2006_mean", #add the year_range category
	    age_dep_ratio_sat_mean=mean(age_2016_mean,age_2006_mean,na.rm=TRUE)
		) %>%
	select(country_name_en,iso3c,iso3n,age_dep_ratio_sat_mean,year_range)

write.csv(dat_clean,
	 file.path(directory,
	    "data",
	    "data_clean",
	    "age_dependency_ratio_worldpop_satellite.csv"
  	),
  	row.names=FALSE
	)