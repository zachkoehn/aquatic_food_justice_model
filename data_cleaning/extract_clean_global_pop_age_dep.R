# ===============================================================================
# Extracting age data from the World Pop global age population mosaics
# 
# Note this was originally to calculate age dependency ratios (young plus elderly/working age pop)
#
# Name: J. Zachary (Zach) Koehn
# Email: zkoehn@gmail.com
# For: BFA Justice
# Date started: 08/17/2017
# Revised: 09/14/2020
# ===============================================================================


library(tidyverse)
library(readr)
library(raster)
library(spDataLarge)
library(sf)
library(pbapply)
library(exactextractr)
library(viridis)
library(countrycode)
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

# global_2006$age_dep <- (global_2006$underworking+global_2006$overworking)/global_2006$working
global_2006$working_percent_pop <- global_2006$working/(global_2006$working+global_2006$underworking+global_2006$overworking)


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

global_2016$working_percent_pop <- global_2016$working/(global_2016$working+global_2016$underworking+global_2016$overworking)




endCluster()

stackSave(global_2016,file.path(work_dir,"global_2016"))
writeRaster(global_2006,file.path(work_dir,"global_2006.grd"), format="raster")
writeRaster(global_2016$working_percent_pop,file.path(work_dir,"global_2016_working_percent_pop.tif"), format="raster")
writeRaster(
  global_2016$working_percent_pop,
  file.path(work_dir,"proportion_working_age_2016_wgs84_longlat.tif"),
  format="GTiff",overwrite=TRUE
)

global_2006 <- raster(file.path(work_dir,"global_2006.grd"), format="raster")

global_2016 <- stack(file.path(work_dir,"global_2016"), format="raster")

library(rnaturalearth)
world <- ne_countries()

extract_country_stats <- function(rast_layer,i) {
  # i=7
  # rast_layer=global_2006$working_percent_pop
  country_a2_code <- world$iso_a2[i]
  country_poly <- st_as_sf(world[i,])
  country_extract <- exact_extract(
    rast_layer,country_poly,
    c("sum","mean","count","coefficient_of_variation")
  )
  country_stats <- cbind(country_a2_code,country_extract)
  return(country_stats)
}




age_2006_country_stats <- t(pbsapply(1:dim(world)[1],function(c) extract_country_stats(rast_layer=global_2006$working_percent_pop,i=c)))
age_2006_country_stats_df <- data.frame(matrix(unlist(age_2006_country_stats), nrow=dim(world)[1], byrow=F),stringsAsFactors=FALSE)
names(age_2006_country_stats_df) <- c("iso_a2","age_2006_sum","age_2006_mean","age_2006_n","age_2006_cv")



age_2016_country_stats <- t(pbsapply(1:dim(world)[1],function(c) extract_country_stats(rast_layer=global_2016$working_percent_pop,i=c)))
age_2016_country_stats_df <- data.frame(matrix(unlist(age_2016_country_stats), nrow=dim(world)[1], byrow=F),stringsAsFactors=FALSE)
names(age_2016_country_stats_df) <- c("iso_a2","age_2016_sum","age_2016_mean","age_2016_n","age_2016_cv")

age_06_16_country_stats_df <- merge(age_2006_country_stats_df,age_2016_country_stats_df,by="iso_a2")


age_06_16_country_stats_df <- age_06_16_country_stats_df %>%
	mutate(
		across(age_2006_sum:age_2016_cv,as.numeric),
		working_perc_mean=((age_2006_mean+age_2016_mean)/2)*100
		)
write.csv(age_06_16_country_stats_df,file.path(work_dir,"national_age_dep_satellite_stats.csv"),row.names=FALSE)

age_nat_geo <- merge(world,age_06_16_country_stats_df,by="iso_a2")


library(ggpubr)



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



age_2006_subnational_stats <- t(pbsapply(1:dim(sub_nat_boundaries)[1],function(c) extract_subnational_stats(rast_layer=global_2006$age_dep,i=c)))
age_2006_subnational_stats_df <- data.frame(matrix(unlist(age_2006_subnational_stats), nrow=dim(sub_nat_boundaries)[1], byrow=F),stringsAsFactors=FALSE)
names(age_2006_subnational_stats_df) <- c("iso_3166_2","age_2006_sum","age_2006_mean","age_2006_n","age_2006_var","age_2006_cv")



age_2016_subnational_stats <- t(pbsapply(1:dim(sub_nat_boundaries)[1],function(c) extract_subnational_stats(rast_layer=global_2016$age_dep,i=c)))
age_2016_subnational_stats_df <- data.frame(matrix(unlist(age_2016_subnational_stats), nrow=dim(sub_nat_boundaries)[1], byrow=F),stringsAsFactors=FALSE)
names(age_2016_subnational_stats_df) <- c("iso_3166_2","age_2016_sum","age_2016_mean","age_2016_n","age_2016_var","age_2016_cv")


age_06_16_subnational_stats_df <- merge(age_2006_subnational_stats_df,age_2016_subnational_stats_df,by="iso_3166_2")


age_06_16_subnational_stats_df <- age_06_16_subnational_stats_df %>%
	mutate(
		across(age_2006_sum:age_2016_cv,as.numeric),
		age_dep_year_mean=((age_2006_mean+age_2016_mean)/2)*100
		)


work_dir

write.csv(age_06_16_subnational_stats_df,file.path(work_dir,"subnational_age_dep_satellite_stats.csv"),row.names=FALSE)

geo_subnat <- merge(sub_nat_boundaries,age_06_16_subnational_stats_df,by="iso_3166_2")
geo_subnat$age_dep_year_mean


all_dat <- read.csv("/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model/data/data_clean/all_national_indicators.csv",header=TRUE)

age_validate_dat <- all_dat %>%
	dplyr::select(iso3c,age.dep.ratio) %>%
	mutate(iso_a2=countrycode(iso3c,"iso3c","iso2c"))


nat_validate <- merge(age_06_16_country_stats_df,age_validate_dat,by="iso_a2")

nat_validate %>%
	ggplot() +
	geom_point(aes(x=age.dep.ratio,y=age_dep_year_mean))+
	ggtitle("Observed vs Raster estimate (Adjusted r2=0.87)")+
	xlab("Age Dep. calculated from World Population Prospects") +
	ylab("Age Dep. calculated from Worldpop rasters")


summary(lm(data=nat_validate,age.dep.ratio~age_dep_year_mean))


# pretty plots :) 

plot(global_2016$age_dep,col=plasma(100))
  
 age_nat_geo %>%
  ggplot(
    aes(
      # x=gdp_percap_prop,
      # y=pop_perlight_prop
      )
    ) +
  geom_sf(aes(fill=age_dep_year_mean)) +
  scale_fill_viridis(option="plasma")+

  ggtitle("National age dependency ratios")





  geo_subnat %>%
    st_as_sf() %>%
    ggplot(
      # aes(
      #   x=mean_subnat_mpi,
      #   y=sat_poverty_est
      #   )
      ) +
    # geom_point() +
    geom_sf(aes(
      fill=age_dep_year_mean
      ),
    lwd=0.1
    ) + 
    scale_fill_viridis(option="plasma")+
    ggtitle("Sub-national age dependency ratios")



