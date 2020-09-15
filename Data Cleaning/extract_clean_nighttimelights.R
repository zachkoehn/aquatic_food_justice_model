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
library(viridis)
library(readxl)
library(rnaturalearth)
library(rnaturalearthhires)

# data from Table S2 in https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0228912#sec010
directory <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model"

rasterOptions(progress = 'text',timer=TRUE) #allows for progress timer... helps multitask :) 


rast_pop_2016 <- raster(
  file.path(
    directory,
    "data",
    "data_raw",
    "landscan",
    "lspop2016",
    "w001001.adf"
    )
  )
# now load all the satellite TIF files (fastest way requires setting a working directory)
setwd(
  file.path(
    directory,
    "data",
    "data_raw",
    "li_et_all_nighttime_lights",
    "harmonized_1992-2018"
  )
)

# now load poverty data used validate satellite-based estimates from https://data.worldbank.org/indicator/SI.POV.DDAY?view=map
# data from https://data.worldbank.org/indicator/SI.POV.NAHC
pov_dat_raw <- read.csv(
  file.path(
    directory,
    "data",
    "data_raw",
    "world_bank_poverty",
    "API_SI.POV.NAHC_DS2_en_csv_v2_1345136.csv"
  ),
  skip = 4,
  header=TRUE
)



library(readr)
library(dplyr)
files<- list.files(pattern="*.tif")
files <- files[10:26] # remove the one non-harmonized file
dat_list <- lapply(files, raster) 
names(dat_list) <- files

rast_lights_2016 <- dat_list[["Harmonized_DN_NTL_2016_simVIIRS.tif"]] 
beginCluster()

crop_lights_2016 <- crop(rast_lights_2016,
  extent(
    -180, #xmin
    180, #xmax
    extent(rast_pop_2016)[3], #ymin
    extent(rast_pop_2016)[4] #ymax
    )
  )

crop_pop_2016 <- crop(rast_pop_2016,
  extent(
    -180, #xmin
    180, #xmax
    extent(rast_pop_2016)[3], #ymin
    extent(rast_pop_2016)[4] #ymax
    )
  )

resample_lights_2016 <- raster::resample(
  crop_lights_2016,crop_pop_2016
  )
endCluster()

crs(rast_pop_2016)



resample_lights_2016[resample_lights_2016<0]=0
beginCluster()

MyFun <- function(resample_lights_2016, crop_pop_2016) {
  new_rast <- ifelse(  resample_lights_2016 == 0, crop_pop_2016, crop_pop_2016/resample_lights_2016)
  return(new_rast)
}

rast_pop_per_light <- overlay(resample_lights_2016, crop_pop_2016, fun = MyFun,forcefun=TRUE)
names(rast_pop_per_light) <- "pop_per_light"

rast_pop_per_light$pop_per_light_log <- log (rast_pop_per_light$pop_per_light+0.01)
endCluster()
plot(rast)

names(rast_pop_per_light)[1] <- "pop_per_light"
par(mfrow=c(1,2))
plot(
  rast_pop_per_light$pop_per_light,
  ylim=c(0,20),
  xlim=c(95,115),
  col=inferno(256)
  )
plot(
  rast_pop_per_light$pop_per_light_log,
  # ylim=c(36.886,38.087),
  # xlim=c(-123.006,-121.677),
  col=viridis(100)
  )

plot(
  rast_pop_per_light,
  ylim=c(36.886,38.087),
  xlim=c(-123.006,-121.677),
  col=inferno(256)
  )

# writeRaster(rast_pop_per_light$pop_per_light, 
#   filename=file.path(
#     directory,
#     "data",
#     "temp",
#     "pop_per_light_2016.asc"
#     ), 
#   format = "ascii", datatype='INT4S', overwrite=TRUE)


extract_lumens_country_stats <- function(rast_layer,i) {
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


pop_per_light_country_stats <- t(pbsapply(1:dim(world)[1],function(c) extract_lumens_country_stats(rast_layer=rast_pop_per_light$pop_per_light,i=c)))
lumens_country_stats_df <- data.frame(matrix(unlist(pop_per_light_country_stats), nrow=dim(world)[1], byrow=F),stringsAsFactors=FALSE)
names(lumens_country_stats_df) <- c("iso_a2","sum","mean","count","variance","coefficient_of_variation")

lumens_world <- merge(world,lumens_country_stats_df,by="iso_a2")

library(ggpubr)
lumens_world <- lumens_world %>%
  mutate(
    across(sum:coefficient_of_variation,as.numeric),
    est_pov = sum/pop
    ) 
  lumens_world %>%
  ggplot(
    aes(
      # x=gdp_percap_prop,
      # y=pop_perlight_prop
      )
    ) +
  geom_sf(aes(fill=est_pov))


  # validate 

  # data from https://data.worldbank.org/indicator/SI.POV.DDAY?view=map
mpi_dat_raw <- read_excel(
  file.path(
    directory,
    "data",
    "data_raw",
    "mpi",
    "Table-5-Subnational-Results-MPI-2020.xlsx"
  ),
  sheet="5.1 MPI Region"
)

mpi_dat_raw <- mpi_dat_raw[9:dim(mpi_dat_raw)[1],1:9]
names(mpi_dat_raw) <- c("iso3n","iso3c","country_name_en","region","data_source","data_year","subnational_region","mpi_national","mpi_subnational")

summary(as.factor(mpi_dat_raw$data_year))

mpi_dat_country_clean <- mpi_dat_raw %>%
  mutate(
    across(mpi_national:mpi_subnational,as.numeric),
    iso_a2=countrycode(iso3c,"iso3c","iso2c") # assign iso3n from iso3c
    ) %>%
  group_by(iso_a2) %>%
  summarize(mean_country_mpi=mean(mpi_national,na.rm=TRUE)) %>%# aggregate by mean
  dplyr::select(iso_a2,mean_country_mpi)
summary(mpi_dat_country_clean)

mpi_dat_country_clean[order(mpi_dat_country_clean$iso3c),]


dat_validate <- merge(lumens_world,mpi_dat_country_clean,by=c("iso_a2"),all.x=TRUE) %>%
  rename(
    mpi=mean_country_mpi,
    sat_model_est_pov=est_pov
    )
  

dat_validate %>%
  ggplot() +
  geom_point(aes(x=mean_country_mpi,y=est_pov)) 

library(patchwork)
mpi_nat_map <- dat_validate %>%
  ggplot(
    aes(
      # x=gdp_percap_prop,
      # y=pop_perlight_prop
      )
    ) +
  geom_sf(aes(fill=mpi))

sat_nat_map <- dat_validate %>%
  ggplot(
    aes(
      # x=gdp_percap_prop,
      # y=pop_perlight_prop
      )
    ) +
  geom_sf(aes(fill=sat_model_est_pov))

mpi_nat_map / sat_nat_map

library(ipumsr)

dat_difs <- dat_validate %>%
  mutate(
    diffs = est_pov-un_pov
    ) %>%
  dplyr::select(country_name_en,diffs,un_pov,est_pov) %>%
  mutate(
    across(diffs:est_pov,as.numeric)
    )


dat_difs_df <- as.data.frame(cbind(dat_difs$country_name_en,as.numeric(dat_difs$diffs),as.numeric(dat_difs$un_pov),as.numeric(dat_difs$est_pov)))
dat_difs_df[,2:4] <- apply(dat_difs_df[,2:4],2,function(x) as.numeric(x))
dat_difs_df[order(dat_difs_df[,2]),]
cor(dat_validate$un_pov,dat_validate$est_pov,use="complete.obs")
plot(lm(data=dat_validate,un_pov~est_pov))


summary(lm(data=dat_validate,est_pov~mean_country_mpi))


# 

library(rnaturalearth);library(rnaturalearthhires)

sub_nat_boundaries <- ne_states()

# sp::plot(sub_nat_boundaries)

extract_lumens_subnational_stats <- function(rast_layer,i) {
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


pop_per_light_country_stats <- t(pbsapply(1:dim(world)[1],function(c) extract_lumens_country_stats(rast_layer=rast_pop_per_light$pop_per_light,i=c)))
lumens_country_stats_df <- data.frame(matrix(unlist(pop_per_light_country_stats), nrow=dim(world)[1], byrow=F),stringsAsFactors=FALSE)
names(lumens_country_stats_df) <- c("iso_a2","sum","mean","count","variance","coefficient_of_variation")



