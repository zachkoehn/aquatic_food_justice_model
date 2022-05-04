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
# par(mfrow=c(1,2))
# plot(
#   rast_pop_per_light$pop_per_light,
#   ylim=c(0,20),
#   xlim=c(95,115),
#   col=inferno(256)
#   )
# plot(
#   rast_pop_per_light$pop_per_light_log,
#   # ylim=c(36.886,38.087),
#   # xlim=c(-123.006,-121.677),
#   col=viridis(100)
#   )

# plot(
#   rast_pop_per_light,
#   ylim=c(36.886,38.087),
#   xlim=c(-123.006,-121.677),
#   col=inferno(256)
#   )

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

mpi_dat_raw <- mpi_dat_raw[9:dim(mpi_dat_raw)[1],c(1,2,3,4,5,6,7,8,9,18)]
names(mpi_dat_raw) <- c("iso3n","iso3c","country_name_en","region","data_source","data_year","subnational_region","mpi_national","mpi_subnational","subnational_pop")

summary(as.factor(mpi_dat_raw$data_year))

mpi_dat_country_clean <- mpi_dat_raw %>%
  mutate(
    across(mpi_national:subnational_pop,as.numeric),
    iso_a2=countrycode(iso3c,"iso3c","iso2c") # assign iso3n from iso3c
    ) %>%
  group_by(iso_a2) %>%
  summarize(mean_country_mpi=mean(mpi_national,na.rm=TRUE)) %>%# aggregate by mean
  dplyr::select(iso_a2,mean_country_mpi)



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


dat_difs_df <- as.data.frame(cbind(dat_difs$country_name_en,as.numeric(dat_difs$diffs),as.numeric(dat_difs$un_pov),as.numeric(dat_difs$est_pov)))
dat_difs_df[,2:4] <- apply(dat_difs_df[,2:4],2,function(x) as.numeric(x))
dat_difs_df[order(dat_difs_df[,2]),]
cor(dat_validate$un_pov,dat_validate$est_pov,use="complete.obs")
plot(lm(data=dat_validate,un_pov~est_pov))


summary(lm(data=dat_validate,sat_model_est_pov~mpi))

nat_sat_poverty_clean <- dat_validate %>%
  mutate( 
    iso3c=countrycode(iso_a2,"iso2c","iso3c"),
    iso3n=countrycode(iso3c,"iso3c","iso3n"),
    country_name_en=countrycode(iso3c,"iso3c","country.name")
    )%>%
    dplyr::select(country_name_en, iso3c,iso3n,sat_model_est_pov) %>%
    drop_na()
st_geometry(nat_sat_poverty_clean) <- NULL

write.csv(nat_sat_poverty_clean,
  file.path(
    directory,
    "data",
    "data_clean",
    "satellite_poverty_national_ests.csv"
    ),
  row.names=FALSE
  )


library(rnaturalearth);library(rnaturalearthhires)

sub_nat_boundaries <- ne_states()
sub_nat_boundaries <- st_as_sf(sub_nat_boundaries)
sp::plot(sub_nat_boundaries[1])

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



pop_per_light_subnational_stats <- t(
  pbsapply(1:dim(sub_nat_boundaries)[1],function(c) extract_subnational_stats(rast_layer=rast_pop_per_light$pop_per_light,i=c))
  )
pop_lights_subnational_stats_df <- data.frame(matrix(unlist(pop_per_light_subnational_stats), nrow=dim(sub_nat_boundaries)[1], byrow=F),stringsAsFactors=FALSE)
names(pop_lights_subnational_stats_df) <- c("iso_3166_2","pop_lights_sum","pop_lights_mean",
  "pop_lights_count","pop_lights_variance","pop_lights_coefficient_of_variation")

pop_subnational_stats <- t(
  pbsapply(1:dim(sub_nat_boundaries)[1],function(c) extract_subnational_stats(rast_layer=crop_pop_2016,i=c))
  )
pop_subnational_stats_df <- data.frame(matrix(unlist(pop_per_light_subnational_stats), nrow=dim(sub_nat_boundaries)[1], byrow=F),stringsAsFactors=FALSE)
names(pop_subnational_stats_df) <- c("iso_3166_2","pop_sum","pop_mean",
  "pop_count","pop_variance","pop_coefficient_of_variation")

geo_subnat %>%
  ggplot() +
  geom_sf(aes(fill=pop_lights_sum))



dat_subnat <- merge(pop_subnational_stats_df,pop_lights_subnational_stats_df,by="iso_3166_2")
write.csv(dat_subnat,file.path(directory,"data","temp","subnational_poverty_satellite_est.csv"))
write.csv(dat_subnat,file.path(directory,"data","temp","subnational_poverty_satellite_est.csv"))


geo_subnat <- merge(sub_nat_boundaries,dat_subnat,by="iso_3166_2")




mpi_dat_subnat_clean <- mpi_dat_raw %>%
  mutate(
    across(mpi_national:subnational_pop,as.numeric),
    iso_a2=countrycode(iso3c,"iso3c","iso2c") # assign iso3n from iso3c
    ) %>%
  group_by(subnational_region) %>%
  summarize( # aggregate poverty index and population by mean (if there are multiple observations)
    mean_subnat_mpi=mean(mpi_subnational,na.rm=TRUE),
    mean_subnat_pop=mean(subnational_pop,na.rm=TRUE)

    ) %>%
  dplyr::select(subnational_region,mean_subnat_mpi,mean_subnat_pop)


mpi_geo_merge <- merge(geo_subnat,mpi_dat_subnat_clean,all.y=TRUE,by.y="subnational_region",by.x="name")



library(ggpubr)

summary(as.numeric(mpi_geo_merge$pop_lights_sum))
summary(as.numeric(mpi_geo_merge$mean_subnat_pop))

mpi_geo_merge <- mpi_geo_merge %>%
  mutate(
    sat_poverty_est = as.numeric(pop_lights_sum)/as.numeric(mean_subnat_pop)
    ) 

summary(lm(data=mpi_geo_merge,mean_subnat_mpi~sat_poverty_est))

plot(mpi_geo_merge$mean_subnat_mpi,mpi_geo_merge$sat_poverty_est,pch=16,cex=0.4,xlim=c(0,1),ylim=c(0,1))


plot(sfmpi_geo_merge["pop_lights_sum"])
  mpi_geo_merge %>%
    st_as_sf() %>%
    ggplot(
      # aes(
      #   x=mean_subnat_mpi,
      #   y=sat_poverty_est
      #   )
      ) +
    # geom_point() +
    geom_sf(aes(
      fill=mean_subnat_mpi
      )
    ) + 
    scale_fill_viridis()


