#######################
# Extracting and cleaning harmonized nighttime lights data
# Zach Koehn
# zkoehn@stanford.edu
#######################



library(tidyverse)
library(pbapply)
library(countrycode)
library(raster)
library(sf)
library(spData)
library(exactextractr)
library(viridis)
library(readxl)
library(rnaturalearth)
library(rnaturalearthhires)


# read in data

# data from Table S2 in https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0228912#sec010
directory <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model"

rasterOptions(progress = 'text',timer=TRUE) #allows for progress timer... helps multitask :) 



ihme_second_women_15to49 <- read.csv(
  file.path(
    directory,
    "data",
    "data_raw",
    "ihme",
    "educational_attainment",
    "Data [CSV]",
    "IHME_LMIC_EDU_2000_2017_SECONDARYPROP_15_49_FEMALE_AD1_Y2019M12D24.CSV"
    )
  )


sub_gis <- ne_states() #subnational polygons



ihme_second_men_15to49 <- read.csv(
  file.path(
    directory,
    "data",
    "data_raw",
    "ihme",
    "educational_attainment",
    "Data [CSV]",
    "IHME_LMIC_EDU_2000_2017_SECONDARYPROP_15_49_MALE_AD1_Y2019M12D24.CSV"
    )
  )

rast_ihme_second_men_15to49 <- raster(
  file.path(
    directory,
    "data",
    "data_raw",
    "ihme",
    "educational_attainment",
    "IHME_LMIC_EDU_2000_2017_SECONDARYPROP_1549_MALE_MEAN_Y2019M12D24.TIF"
    )
  )

rast_ihme_second_women_15to49 <- raster(
  file.path(
    directory,
    "data",
    "data_raw",
    "ihme",
    "educational_attainment",
    "IHME_LMIC_EDU_2000_2017_SECONDARYPROP_1549_FEMALE_MEAN_Y2019M12D24.TIF"
    )
  )




sub_nat_boundaries <- ne_states()
sub_nat_boundaries <- st_as_sf(sub_nat_boundaries)

extract_subnational_stats <- function(rast_layer,i) {
  # i=7
  # rast_layer=rast
  subnational_a2_code <- sub_nat_boundaries$iso_3166_2[i]
  subnational_poly <- st_geometry(sub_nat_boundaries[i,])
  subnational_extract <- exact_extract(
    rast_layer,subnational_poly,
    c("mean")
  )
  subnational_stats <- cbind(subnational_a2_code,subnational_extract)
  return(subnational_stats)
}
beginCluster()
education_ratio <- rast_ihme_second_women_15to49/rast_ihme_second_men_15to49
endCluster()


beginCluster()
education_ratio_subnational_stats <- t(
  pbsapply(1:dim(sub_nat_boundaries)[1],function(c) extract_subnational_stats(rast_layer=education_ratio,i=c))
  )
endCluster()


education_ratio_subnational_stats_df <- data.frame(matrix(unlist(education_ratio_subnational_stats), nrow=dim(sub_nat_boundaries)[1], byrow=F),stringsAsFactors=FALSE)
names(education_ratio_subnational_stats_df) <- c("iso_3166_2","gender_secondary_education_ratio_mean")


beginCluster()
women_education_subnational_stats <- t(
  pbsapply(1:dim(sub_nat_boundaries)[1],function(c) extract_subnational_stats(rast_layer=rast_ihme_second_women_15to49,i=c))
  )
endCluster()


women_education_subnational_stats_df <- data.frame(matrix(unlist(women_education_subnational_stats), nrow=dim(sub_nat_boundaries)[1], byrow=F),stringsAsFactors=FALSE)
names(women_education_subnational_stats_df) <- c("iso_3166_2","women_secondary_education_mean")



write.csv(
	education_ratio_subnational_stats_df,
	file.path(
		directory,
		"data",
		"data_clean",
		"clean_subnational",
		"education_ratio_subnational_mean_2000_2017.csv"
		),
	row.names=FALSE
	)




write.csv(
	women_education_subnational_stats_df,
	file.path(
		directory,
		"data",
		"data_clean",
		"clean_subnational",
		"women_education_subnational_mean_2000_2017.csv"
		),
	row.names=FALSE
	)


















