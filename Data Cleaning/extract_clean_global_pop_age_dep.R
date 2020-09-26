library(tidyverse)
library(readr)
library(raster)
library(spDataLarge)
library(sf)
library(pbapply)
library(exactextractr)
setwd("/Users/zachkoehn/Documents/worldpop/age_pop_rasters")


# load dataset and packages
library(tidyverse)
library(countrycode)
rasterOptions(progress = 'text',timer=TRUE) #allows for progress timer... helps multitask :) 


work_dir <- "/Users/zachkoehn/Documents/worldpop/age_pop_rasters"

setwd(file.path(work_dir,"rast_2006"))

files_2006 <- list.files()
rast_2006_list <- lapply(files_2006, raster) 
names(rast_2006_list) <- files_2006


setwd(file.path(work_dir,"rast_2016"))

files_2016 <- list.files()
rast_2016_list <- lapply(files_2016, raster) 
names(rast_2016_list) <- files_2016

setwd(file.path(work_dir))




beginCluster()
global_2006_underworking <- sum(
	rast_2006_list[["global_f_0_2006_1km.tif"]],
	rast_2006_list[["global_f_1_2006_1km.tif"]],
	rast_2006_list[["global_f_5_2006_1km.tif"]],
	rast_2006_list[["global_f_10_2006_1km.tif"]],
	rast_2006_list[["global_m_0_2006_1km.tif"]],
	rast_2006_list[["global_m_1_2006_1km.tif"]],
	rast_2006_list[["global_m_5_2006_1km.tif"]],
	rast_2006_list[["global_m_10_2006_1km.tif"]]
	)

global_2006_overworking <- sum(
	rast_2006_list[["global_f_65_2006_1km.tif"]],
	rast_2006_list[["global_f_70_2006_1km.tif"]],
	rast_2006_list[["global_f_75_2006_1km.tif"]],
	rast_2006_list[["global_f_80_2006_1km.tif"]],
	rast_2006_list[["global_m_65_2006_1km.tif"]],
	rast_2006_list[["global_m_70_2006_1km.tif"]],
	rast_2006_list[["global_m_75_2006_1km.tif"]],
	rast_2006_list[["global_m_80_2006_1km.tif"]]
	)

global_2006_working <- sum(
	rast_2006_list[["global_f_15_2006_1km.tif"]],
	rast_2006_list[["global_f_20_2006_1km.tif"]],
	rast_2006_list[["global_f_25_2006_1km.tif"]],
	rast_2006_list[["global_f_30_2006_1km.tif"]],
	rast_2006_list[["global_f_35_2006_1km.tif"]],
	rast_2006_list[["global_f_40_2006_1km.tif"]],
	rast_2006_list[["global_f_45_2006_1km.tif"]],
	rast_2006_list[["global_f_50_2006_1km.tif"]],
	rast_2006_list[["global_f_55_2006_1km.tif"]],
	rast_2006_list[["global_f_60_2006_1km.tif"]],
	rast_2006_list[["global_m_15_2006_1km.tif"]],
	rast_2006_list[["global_m_20_2006_1km.tif"]],
	rast_2006_list[["global_m_25_2006_1km.tif"]],
	rast_2006_list[["global_m_30_2006_1km.tif"]],
	rast_2006_list[["global_m_35_2006_1km.tif"]],
	rast_2006_list[["global_m_40_2006_1km.tif"]],
	rast_2006_list[["global_m_45_2006_1km.tif"]],
	rast_2006_list[["global_m_50_2006_1km.tif"]],
	rast_2006_list[["global_m_55_2006_1km.tif"]],
	rast_2006_list[["global_m_60_2006_1km.tif"]]
	)

global_2006 <- stack(global_2006_underworking,global_2006_overworking,global_2006_working)
names(global_2006) <- c("underworking","overworking","working")

global_2006$age_dep <- (global_2006$underworking+global_2006$overworking)/global_2006$working


global_2016_underworking <- sum(
	rast_2016_list[["global_f_0_2016_1km.tif"]],
	rast_2016_list[["global_f_1_2016_1km.tif"]],
	rast_2016_list[["global_f_5_2016_1km.tif"]],
	rast_2016_list[["global_f_10_2016_1km.tif"]],
	rast_2016_list[["global_m_0_2016_1km.tif"]],
	rast_2016_list[["global_m_1_2016_1km.tif"]],
	rast_2016_list[["global_m_5_2016_1km.tif"]],
	rast_2016_list[["global_m_10_2016_1km.tif"]]
	)

global_2016_overworking <- sum(
	rast_2016_list[["global_f_65_2016_1km.tif"]],
	rast_2016_list[["global_f_70_2016_1km.tif"]],
	rast_2016_list[["global_f_75_2016_1km.tif"]],
	rast_2016_list[["global_f_80_2016_1km.tif"]],
	rast_2016_list[["global_m_65_2016_1km.tif"]],
	rast_2016_list[["global_m_70_2016_1km.tif"]],
	rast_2016_list[["global_m_75_2016_1km.tif"]],
	rast_2016_list[["global_m_80_2016_1km.tif"]]
	)

global_2016_working <- sum(
	rast_2016_list[["global_f_15_2016_1km.tif"]],
	rast_2016_list[["global_f_20_2016_1km.tif"]],
	rast_2016_list[["global_f_25_2016_1km.tif"]],
	rast_2016_list[["global_f_30_2016_1km.tif"]],
	rast_2016_list[["global_f_35_2016_1km.tif"]],
	rast_2016_list[["global_f_40_2016_1km.tif"]],
	rast_2016_list[["global_f_45_2016_1km.tif"]],
	rast_2016_list[["global_f_50_2016_1km.tif"]],
	rast_2016_list[["global_f_55_2016_1km.tif"]],
	rast_2016_list[["global_f_60_2016_1km.tif"]],
	rast_2016_list[["global_m_15_2016_1km.tif"]],
	rast_2016_list[["global_m_20_2016_1km.tif"]],
	rast_2016_list[["global_m_25_2016_1km.tif"]],
	rast_2016_list[["global_m_30_2016_1km.tif"]],
	rast_2016_list[["global_m_35_2016_1km.tif"]],
	rast_2016_list[["global_m_40_2016_1km.tif"]],
	rast_2016_list[["global_m_45_2016_1km.tif"]],
	rast_2016_list[["global_m_50_2016_1km.tif"]],
	rast_2016_list[["global_m_55_2016_1km.tif"]],
	rast_2016_list[["global_m_60_2016_1km.tif"]]
	)

global_2016 <- stack(global_2016_underworking,global_2016_overworking,global_2016_working)
names(global_2016) <- c("underworking","overworking","working")

global_2016$age_dep <- (global_2016$underworking+global_2016$overworking)/global_2016$working




endCluster()



extract_country_stats <- function(rast_layer,i) {
  # i=7
  # rast_layer=rast
  country_a2_code <- world$iso_a2[i]
  country_poly <- st_geometry(world[i,])
  country_extract <- exact_extract(
    rast_layer,country_poly,
    c("sum","mean","count","variance","coefficient_of_variation")
  )
  country_stats <- cbind(country_a2_code,country_extract)
  return(country_stats)
}




age_2006_country_stats <- t(pbsapply(1:dim(world)[1],function(c) extract_country_stats(rast_layer=global_2006$age_dep,i=c)))
age_2006_country_stats_df <- data.frame(matrix(unlist(age_2006_country_stats), nrow=dim(world)[1], byrow=F),stringsAsFactors=FALSE)
names(age_2006_country_stats_df) <- c("iso_a2","age_2006_sum","age_2006_mean","age_2006_n","age_2006_var","age_2006_cv")



age_2016_country_stats <- t(pbsapply(1:dim(world)[1],function(c) extract_country_stats(rast_layer=global_2016$age_dep,i=c)))
age_2016_country_stats_df <- data.frame(matrix(unlist(age_2016_country_stats), nrow=dim(world)[1], byrow=F),stringsAsFactors=FALSE)
names(age_2016_country_stats_df) <- c("iso_a2","age_2016_sum","age_2016_mean","age_2016_n","age_2016_var","age_2016_cv")

age_06_16_country_stats_df <- merge(age_2006_country_stats_df,age_2016_country_stats_df,by="iso_a2")


age_06_16_country_stats_df <- age_06_16_country_stats_df %>%
	mutate(
		across(age_2006_sum:age_2016_var,as.numeric),
		age_dep_year_mean=((age_2006_mean+age_2016_mean)/2)*100
		)


age_geo <- merge(world,age_06_16_country_stats_df,by="iso_a2")


library(ggpubr)

  age_geo %>%
  ggplot(
    aes(
      # x=gdp_percap_prop,
      # y=pop_perlight_prop
      )
    ) +
  geom_sf(aes(fill=age_dep_year_mean))



library(rnaturalearth);library(rnaturalearthhires)

sub_nat_boundaries <- ne_states()
sub_nat_boundaries <- st_as_sf(sub_nat_boundaries)
# sp::plot(sub_nat_boundaries)

extract_subnational_stats <- function(rast_layer,i) {
  # i=7
  # rast_layer=rast
  subnational_a2_code <- sub_nat_boundaries$iso_3166_2[i]
  subnational_poly <- st_geometry(sub_nat_boundaries[i,])
  subnational_extract <- exact_extract(
    rast_layer,subnational_poly,
    c("sum","mean","count","variance","coefficient_of_variation")
  )
  subnational_stats <- cbind(subnational_a2_code,subnational_extract)
  return(subnational_stats)
}


age_2006_country_stats <- t(pbsapply(1:dim(world)[1],function(c) extract_subnational_stats(rast_layer=global_2006$age_dep,i=c)))
age_2006_country_stats_df <- data.frame(matrix(unlist(age_2006_country_stats), nrow=dim(world)[1], byrow=F),stringsAsFactors=FALSE)
names(age_2006_country_stats_df) <- c("iso_a2","age_2006_sum","age_2006_mean","age_2006_n","age_2006_var","age_2006_cv")



age_2016_country_stats <- t(pbsapply(1:dim(world)[1],function(c) extract_subnational_stats(rast_layer=global_2016$age_dep,i=c)))
age_2016_country_stats_df <- data.frame(matrix(unlist(age_2016_country_stats), nrow=dim(world)[1], byrow=F),stringsAsFactors=FALSE)
names(age_2016_country_stats_df) <- c("iso_a2","age_2016_sum","age_2016_mean","age_2016_n","age_2016_var","age_2016_cv")


