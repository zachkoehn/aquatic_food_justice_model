#######################
# Extracting and cleaning harmonized nighttime lights data
# Zach Koehn
# zkoehn@stanford.edu
#######################

# load dataset and packages
library(tidyverse)
library(pbapply)
library(countrycode)
library(raster)
library(sf)
library(spData)
library(exactextractr)
# data from Table S2 in https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0228912#sec010
directory <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model"
setwd(
  file.path(
    directory,
    "data",
    "data_raw",
    "li_et_all_nighttime_lights",
    "harmonized_1992-2018"
  )
)
library(readr)
library(dplyr)
files<- list.files(pattern="*.tif")
files <- files[10:26] # remove the one non-harmonized file
dat_list <- lapply(files, raster) 
names(dat_list) <- files

rast <- dat_list[["Harmonized_DN_NTL_2013_calDMSP.tif"]] 

extract_lumens_country_stats <- function(rast_layer,i) {
  # i=7
  # rast_layer=rast
  country_a2_code <- world$iso_a2[i]
  country_poly <- st_geometry(world[i,])
  country_extract <- exact_extract(
    rast_layer,country_poly,
    c("mean","count","variance","coefficient_of_variation")
  )
  country_stats <- cbind(country_a2_code,country_extract)
  return(country_stats)
}


lumens_country_stats <- t(pbsapply(1:dim(world)[1],function(c) extract_lumens_country_stats(rast_layer=rast,i=c)))
lumens_country_stats_df <- data.frame(matrix(unlist(lumens_country_stats), nrow=dim(world)[1], byrow=F),stringsAsFactors=FALSE)
names(lumens_country_stats_df) <- c("iso_a2","mean","count","variance","coefficient_of_variation")

lumens_world <- merge(world,lumens_country_stats_df,by="iso_a2")



lumens_world %>%
  mutate(
    across(mean:coefficient_of_variation,as.numeric),
    lumens_perarea = mean/area_km2
    ) %>%
  ggplot() +
  geom_sf(aes(fill = mean)) +
    scale_fill_viridis_c() +
    coord_sf() +
    theme_bw()


library(RColorBrewer)
plot(
  rast,
  col=grey.colors(n = 100, 0, 1)
  ,
  ylim=c(0,55),
  xlim=c(50,130)
  )
plot(st_geometry(lumens_world),border="darkorchid1",lwd=0.3,add=TRUE)


library(ipumsr)




