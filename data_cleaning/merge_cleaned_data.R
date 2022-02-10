#######################
# Merge national-level data
# Zach Koehn
# zkoehn@stanford.edu
#######################

# load dataset and packages
library(tidyverse)
library(countrycode)


work_dir <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model"

setwd(file.path(work_dir,"data","data_clean"))

library(readr)  
library(dplyr)

# loads all files
files<- list.files(pattern="*.csv")
files <- files[files!="all_national_indicators.csv"] # remove pre-existing dataset so we aren't merging the same information on infinite repeat :) 
files <- files[files!="meow_climatic_zone.csv"] # remove pre-existing dataset so we aren't merging the same information on infinite repeat :) 
df_list <- lapply(files, read_csv) 
names(df_list) <- files

oecd_categories <- read.csv(
  file.path(work_dir,"data","data_raw","oecd_country_grouping_categories.csv"),
  header=TRUE,
  nrows=193
  ) 



oecd_categories <- oecd_categories %>%
  select(X...ISO.Code,Geographic.access..5.) %>%
  rename(
    iso3c=X...ISO.Code,
    geographic_access=Geographic.access..5.
    ) %>%
  mutate(
    geographic_access=str_trim(geographic_access,"both")) %>%
  filter(geographic_access=="Landlocked")
landlocked_countries <- oecd_categories$iso3c

# vector of territories to exclude from the model
territories <-c(
  "VIR", #Virgin Islands United States
  "UMI", #Minor outlying islands United States
  "PRI", #Puerto Rico United States
  "MNP", #N Mariana islands United States
  "GUM", #Guam United States
  "ASM", #American Samoa United States
  "VGB", #Virgin islands UK
  "TCA", #Turks and Caicos UK
  "SGS", #South Georgia and South Sandwich Islands UK
  "SHN", # Saint Helena, Ascension and Tristan da Cunha UK
  "PCN", #Pitcairn UK
  "MSR", #Montserrat UK
  "GIB", #Gibraltar UK
  "FLK", #Falkland Islands UK
  "CYM", #Cayman Islands UK
  "IOT", #British Indian Ocean Territory UK
  "BMU", #Bermuda UK
  "AIA", #Anguilla UK
  "SJM", #Svalbard and Jan Mayen Norway
  "BVT",#Bouvet Island Norway
  "TKL", #Tokelau New Zealand
  "NIU", #Niue New Zealand
  "COK", #Cook Islands New Zealand
  "SXM", #Sint Maarten Netherlands
  "CUW", #Curacao Netherlands
  "BES", #Bonaire, Sint Eustatius and Saba Netherlands
  "ABW", #Aruba Netherlands
  "WLF", #Wallis and Fortuna France
  "SPM", #Saint Pierre and Miquelon France
  "MAF", #Saint Martin France
  "BLM", #Saint Barthelemy France
  "REU", #Reunion France
  "NCL", #New Caledonia France
  "MYT", #Mayotte France
  "MTQ", #Martinique France
  "GLP", #Guadeloupe France
  "ATF", #French Southern Territories
  "PYF", #French Polynesia France
  "GUF", #French Guiana France
  "ALA", #Aland Finland
  "GRL", #Greenland Denmark
  "FRO", #Faroe Islands Denmark
  "MAC", #Macao China
  "HKG", #Hong Kong China
  "JEY", #Jersey UK
  "IMN", #Isle of Man UK
  "GGY", #Guernsey UK
  "NFK", #Norfolk Island Australia
  "HMD", #Heard and McDonald Islands Australia
  "CCK", #Cocos Islands Australia
  "CXR", #Christmas island Australia
  "VAT",  #Vatican City
  "ESH",
  "ATA"   #Antarctica
  )

lapply(df_list,function(x)names(x))
# substitute 0 for NA values in all produciton data (no need to do this for total)
df_list[["production_by_isccaap_faostat_mean_2006-2016.csv"]] <- df_list[["production_by_isccaap_faostat_mean_2006-2016.csv"]] %>%
  mutate(
    across(mean_prod_carps_etc:mean_pro_horeshoe_crabs_etc, ~replace_na(.x,0))
    ) %>%
  select(-unit)
df_list[["production_by_source_faostat_mean_2006-2016.csv"]] <- df_list[["production_by_source_faostat_mean_2006-2016.csv"]] %>%
  mutate(
    across(mean_aquaculture_production_freshwater:mean_aquaculture_production_brackish, ~replace_na(.x,0))
    ) %>%
  select(-unit)
df_list[["production_total_faostat_mean_2006-2016.csv"]] <- df_list[["production_total_faostat_mean_2006-2016.csv"]] %>%
  select(-unit)

df_list[["aquastat_total_renewable_water.csv"]] <- df_list[["aquastat_total_renewable_water.csv"]] %>%  # name needs to be changed from year.range to year_range, also double 
    rename(year_range=year.range) %>%
    mutate(year_range=as.character(year_range))


# name needs to be changed from year.range to year_range
df_list[["FBS_seafood_consumption_reliance.csv"]] <- df_list[["FBS_seafood_consumption_reliance.csv"]] %>%  # name needs to be changed from year.range to year_range, also double 
    rename(year_range=year.range) %>%
    mutate(year_range=as.character(year_range))

df_list[["voice_and_accountability_2008_2018.csv"]] <- df_list[["voice_and_accountability_2008_2018.csv"]] %>%  # name needs to be changed from year.range to year_range, also double 
    rename(
      year_range=year.range
      ) %>%
    mutate(year_range=as.character(year_range)) %>%
  select(-Code)

df_list[["livelihoods_direct_indirect.csv"]] <- df_list[["livelihoods_direct_indirect.csv"]] %>%  # name needs to be changed from year.range to year_range, also double 
    rename(year_range=year.range) %>%
    mutate(year_range=as.character(year_range))
  # and now do the same thing for the livelihood variables

df_list[["gdp_mean_annual_2006-2016.csv"]] <- df_list[["gdp_mean_annual_2006-2016.csv"]] %>%  # name needs to be changed from year.range to year_range, also double 
  rename(year_range=year,
         iso3c=Country.Code) %>%
  mutate(
    year_range=as.character(year_range),
    iso3n=countrycode(iso3c,"iso3c","iso3n")
    )
# and now do the same thing for the GDP variables

df_list[["Ethnicity_revisions.csv"]] <- df_list[["Ethnicity_revisions.csv"]] %>%  # name needs to be changed from year.range to year_range, also double
  rename(
    year_range=year,
    country_name_en=country_name
         ) %>%
  mutate(
    year_range=as.character(year_range),
    iso3n=countrycode(iso3c,"iso3c","iso3n")
    )


  df_list[["nutrition_score_2010_2017_revisions.csv"]] <- df_list[["nutrition_score_2010_2017_revisions.csv"]] %>%  # name needs to be changed from year.range to year_range, also double
  rename(
    year_range=year,
    country_name_en=country_name
    ) %>%
  mutate(
    year_range=as.character(year_range),
    iso3n=countrycode(iso3c,"iso3c","iso3n")
    ) %>%
  select(-country_name_FAO)
# and now do the same thing for the nutrition density variables


# now collapse the list into a single data frame in order to export to CSV
df_merged <- df_list %>%
  reduce(full_join,by=c("iso3c","iso3n")) %>% #merges all by the iso codes
  do(.[!duplicated(names(.))]) %>%
  select(
    -(mean_prod_carps_etc:mean_pro_horeshoe_crabs_etc)
    ) %>%
  select(., #selects within piped data
         # -working_percent_sat_mean, #removes WorldPop dataset to address reviewer concern
         -starts_with("country"), #removes country variable (duplicated in csvs)
         -starts_with("year"),#removes year category variable (duplicated in csvs)
         -starts_with("geog"),#removes geog variable 
         -starts_with("unit")#removes unit variable (duplicated in csvs)
  ) %>% 
  filter(
    is.na(iso3n)==FALSE,
    !iso3c %in% territories, #excludes territories
    iso3c!="NA",
    duplicated(iso3c) ==FALSE #removes duplicated rows (after the first) from merge (distinct no longer works for me :( ))
    ) %>%
  mutate(
    country_name_en=countrycode(iso3n,"iso3n","country.name.en"),
    un_region_name=countrycode(iso3n,"iso3n","un.region.name"),
    un_region_code=countrycode(iso3n,"iso3n","un.region.code"),
    un_subregion_1=countrycode(iso3c,"iso3c","un.regionsub.name"),
    un_subregion_code=countrycode(iso3c,"iso3c","un.regionsub.code"),
    un_subregion_finer_name=countrycode(iso3c,"iso3c","un.regionintermediate.name")
    ) %>%
  mutate( # a few of the indicators reported their proportions as 57% instead of 0.57... so transforming those
    mean_educ=mean_educ*.01,
    mean_pov_prop=mean_pov_prop*.01,
    mean_women_parl_perc=mean_women_parl_perc*.01,
    sat_model_est_wealth=1/sat_model_est_pov,
    gender_equality=1-mean_sigi,
    fish_affordability=1/fish_relative_caloric_price,
    un_subregion_2=ifelse(un_subregion_1=="Latin America and the Caribbean",un_subregion_finer_name,un_subregion_1), 
    un_subregion_2=ifelse(un_subregion_2=="Sub-Saharan Africa",un_subregion_finer_name,un_subregion_2)
    ) %>%
  rename(
    mean_voice_account=mean.voice.and.accountability #and change variable name to underscore from period
    ) %>%
  filter(
    country_name_en!="NA"
    ) %>%
  dplyr::select(country_name_en,iso3c,iso3n,everything())



# ok, now we need to replace  NA's for landlocked countries coming from SAU data with 0s (because SAU doesn't include any information on landlocked countries
df_merged$eez_total[df_merged$iso3c %in% landlocked_countries] <- 0
df_merged$ifa_total[df_merged$iso3c %in% landlocked_countries] <- 0
df_merged$pp_eez_weighted[df_merged$iso3c %in% landlocked_countries] <- 0 
dim(df_merged)


write.csv(df_merged,
          file.path(work_dir,"data","data_clean","all_national_indicators.csv"),
          row.names=FALSE
          )

write.csv(df_merged,
          file.path(work_dir,"all_national_indicators.csv"),
          row.names=FALSE
          )

