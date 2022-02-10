# Kelvin Gorospe
# kdgorospe@gmail.com
# Get total catch and production (tonnes) from FAO Stat

# NOTE: brown seaweeds, green seaweeds, and red seaweeds (isscaap groups) and plantae aquaticae (species_major_group) are all filtered out of production

# First load function for rebuilding from FishStat ZIP file:
rm(list=ls())
library(tidyverse)
# note: Change outdir to the filepath where you want outputs to go
outdir <- "Outputs"

rebuild_fish <- function(path_to_zipfile) {
  require(tools) # needed for file_path_sans_ext
  require(dplyr)
  require(purrr)
  require(readxl) # part of tidyverse but still need to load readxl explicitly, because it is not a core tidyverse package

  # The following ensures unzipped folder is created in the same directory as the zip file (can be different from the working directory)
  # set outdir
  if (file.exists(basename(path_to_zipfile))) { # if file is in current directory and only file name was given
    outdir <- getwd()
  } else if (file.exists(path_to_zipfile)) { # if file path was given
    outdir <- dirname(path_to_zipfile)
  } else {
    stop("Check path_to_zipfile")
  }

  foldername <- file_path_sans_ext(basename(path_to_zipfile))
  outfolder <- paste(outdir, foldername, sep = "/")
  unzip(path_to_zipfile, exdir = outfolder) # Problem: if unable to unzip folder, still creates outfolder how to supress this?
  # setwd(outfolder)
  # list files
  fish_files <- list.files(outfolder)

  # read .xlsx file (explains data structure of time series)
  # IMPORTANT: column ORDER (ABCDEF) in DS file should match columns ABCDEF in time series for looping to work below
  # each row gives info for how this time series column should be merged with a code list (CL) file
  ds_file <- fish_files[grep("DSD", fish_files)]
  path_to_ds <- paste(outfolder, ds_file, sep = "/")

  # skip removes title row
  ds <- read_excel(path_to_ds, skip=1)

  # manually correct ds file's codelist ID column:
  ds <- ds %>%
    mutate(Codelist_Code_id = case_when(
      Concept_id == "SOURCE" ~ "IDENTIFIER",
      Concept_id == "SYMBOL" ~ "SYMBOL",
      Concept_id != "SYMBOL|SOURCE" ~ Codelist_Code_id
    ))

  # Multiple CL files have the following column names in common: "Identifier" and "Code"
  # Which means after merge, below, you get "Identifier.x" and "Identifier.y", etc.
  # To disambiguate, Append Codelist with Concept_id
  code_ids_to_change<-ds$Codelist_Code_id[grep("IDENTIFIER|CODE", ds$Codelist_Code_id)]
  concept_ids_to_append<-ds$Concept_id[grep("IDENTIFIER|CODE", ds$Codelist_Code_id)]
  new_code_ids <- paste(concept_ids_to_append, code_ids_to_change, sep = "_")
  ds$Codelist_Code_id[grep("IDENTIFIER|CODE", ds$Codelist_Code_id)]<-new_code_ids

  # remove non CSVs (do this to ignore "CL_History.txt" file)
  fish_files <- fish_files[grep(".csv", fish_files)]

  # read in time series.csv
  time_files <- fish_files[grep("TS", fish_files)]
  path_to_ts <- paste(outfolder, time_files, sep = "/")
  time_series <- read.csv(path_to_ts)
  names(time_series) <- tolower(names(time_series))
  time_series_join <- time_series

  for (i in 1:nrow(ds)) {
    # TRUE/FALSE: is there a filename listed in Codelist_id?
    if (!is.na(ds$Codelist_id[i])) {
      # Use ds file to generate path_to_cl individually
      code_file_i <- paste(ds$Codelist_id[i], ".csv", sep = "")
      path_to_cl <- paste(outfolder, code_file_i, sep = "/")
      cl_i <- read.csv(path_to_cl, check.names = FALSE) # check.names = FALSE to prevent R from adding "X" in front of column "3Alpha_Code" - creates problems because this is the matching column for merging with time series

      # Many CL files have "Name" as a column, also Name_En, Name_Fr, Name_Es, etc
      # Also, "Identifier", "Major Group", and "Code" are common across some CL files
      # To disambiguate, append "Concept_ID" from DS file to all columns in CL that contain these terms
      concept_names <- paste(ds$Concept_id[i], names(cl_i)[grep("Name|Major_Group|Identifier|Code", names(cl_i))], sep = "_")
      names(cl_i)[grep("Name|Major_Group|Identifier|Code", names(cl_i))] <- concept_names


      names(cl_i) <- tolower(names(cl_i)) # convert all cl headers to lowercase
      merge_col <- tolower(ds$Codelist_Code_id[i]) # do the same to DS file's code ID so it matches with cl


      # If factor...
      #if (is.factor(cl_i[[merge_col]])) {
      # ...Test if factor levels need to be merged?
      #if (!nlevels(cl_i[[merge_col]]) == nlevels(time_series_join[[names(time_series_join)[i]]])) {
      # combined <- sort(union(time_series_join[[names(time_series_join)[i]]], levels(cl_i[[merge_col]])))
      #    levels(time_series_join[[names(time_series_join)[i]]]) <- levels(cl_i[[merge_col]])
      #  }
      #}
      # This avoids warnings about unequal factor levels below

      # Try converting to character first instead
      if (is.factor(cl_i[[merge_col]])){
        cl_i[[merge_col]]<-as.character(cl_i[[merge_col]])
        time_series_join[[names(time_series_join)[i]]]<-as.character(time_series_join[[names(time_series_join)[i]]])
      }


      # Can't just merge by column number:
      # In Time Series, column COUNTRY, AREA, SOURCE, SPECIES, and UNIT correspond to column 1 in their respective CL files
      # but in Time Series, column SYMBOL corresponds to column 2

      # Note: the following code does not work: #time_series_join<-left_join(time_series, cl_i, by = c(names(time_series)[i] = merge_col))
      # the argument "by" needs to take on the form of join_cols as shown below
      firstname <- names(time_series_join)[i]
      join_cols <- merge_col
      names(join_cols) <- firstname


      time_series_join <- left_join(time_series_join, cl_i, by = join_cols)

      # Convert back to factor
      if (is.character(time_series_join[[names(time_series_join)[i]]])){
        time_series_join[[names(time_series_join)[i]]]<-as.factor(time_series_join[[names(time_series_join)[i]]])
      }
    }
    # Expected warning: Coerces from factor to character because time_series$SPECIES (nlevels=2341) and CL_FI_SPECIES_GROUPS.csv column "3alpha_code" (nlevels = 12751) have different number of factor levels
    # Expected warning: Coerces from factor to chracter because time_series$UNIT and CL_FILE_UNIT.csv column "code" have different number of factor levels
    # Expected warning: Coerces from factor to character because time_series$SYMBOL and CL_FI_SYMBOL.csv column "symbol" have diff number of factors
  }

  return(time_series_join)
}

# NOTE: Change file path to location of zip file
fishstat_dat <- rebuild_fish("/Volumes/jgephart/FishStatR/Data/Production-Global/ZippedFiles/GlobalProduction_2019.1.0.zip")

# Output total production per country
fishstat_dat_total <- fishstat_dat %>%
  rename(iso3n = country,
         iso3c = country_iso3_code) %>%
  filter(isscaap_group %in% c("Red seaweeds", "Green seaweeds", "Brown seaweeds")==FALSE) %>%
  filter(species_major_group != "PLANTAE AQUATICAE") %>% 
  filter(unit == "t") %>%
  filter(year < 2017 & year > 2005) %>% # subset 2006 to 2016
  group_by(country_name_en, iso3n, iso3c, year, unit) %>%
  summarise(total_production_by_year = sum(quantity, na.rm = TRUE)) %>% # Calculate total production for each country for each year
  ungroup() %>%
  group_by(country_name_en, iso3n, iso3c, unit) %>%
  summarise(mean_total_production = mean(total_production_by_year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate_all(~str_replace_all(., ",", "")) %>% # remove commas before writing to csv
  mutate(year_range = '2006-2016') # add metadata column

write.csv(fishstat_dat_total, file.path(outdir, "production_total_faostat_mean_2006-2016.csv"), row.names = FALSE, quote = FALSE)

# Output production split by source (i.e., capture vs aquaculture) per country
fishstat_dat_by_source <- fishstat_dat %>%
  rename(iso3n = country,
         iso3c = country_iso3_code) %>%
  filter(isscaap_group %in% c("Red seaweeds", "Green seaweeds", "Brown seaweeds")==FALSE) %>%
  filter(species_major_group != "PLANTAE AQUATICAE") %>% 
  filter(unit == "t") %>%
  filter(year < 2017 & year > 2005) %>% # subset 2006 to 2016
  group_by(country_name_en, iso3n, iso3c, source_name_en, year, unit) %>%
  summarise(isscaap_production_by_year = sum(quantity, na.rm = TRUE)) %>% # calculate production of each production source, per country, per year
  ungroup() %>%
  group_by(country_name_en, iso3n, iso3c, source_name_en, unit) %>% # now collapse year, and calculate mean across years
  summarise(mean_production_by_source = mean(isscaap_production_by_year)) %>%
  ungroup() %>% 
  mutate_all(~str_replace_all(., ",", "")) %>% # remove commas before writing to csv
  mutate(year_range = '2006-2016') %>% # add metadata column
  pivot_wider(names_from = source_name_en, values_from = mean_production_by_source) %>%
  rename(mean_aquaculture_production_freshwater = 'Aquaculture production (freshwater)',
         mean_capture_production = 'Capture production',
         mean_aquaculture_production_marine = 'Aquaculture production (marine)',
         mean_aquaculture_production_brackish = 'Aquaculture production (brackishwater)')

  
write.csv(fishstat_dat_by_source, file.path(outdir, "production_by_source_faostat_mean_2006-2016.csv"), row.names = FALSE, quote = FALSE)

# Output production split by isscaap group per country
fishstat_dat_by_isscaap <- fishstat_dat %>%
  rename(iso3n = country,
         iso3c = country_iso3_code) %>%
  filter(unit == "t") %>%
  filter(year < 2017 & year > 2005) %>% # subset 2006 to 2016
  group_by(country_name_en, iso3n, iso3c, isscaap_group, year, unit) %>%
  summarise(isscaap_production_by_year = sum(quantity, na.rm = TRUE)) %>% # calculate production of each isscaap group, per country, per year
  ungroup() %>%
  group_by(country_name_en, iso3n, iso3c, isscaap_group, unit) %>% # now collapse year, and calculate mean across years
  summarise(mean_production_by_isscaap = mean(isscaap_production_by_year)) %>%
  ungroup() %>% 
  mutate_all(~str_replace_all(., ",", "")) %>% # remove commas before writing to csv
  mutate(year_range = '2006-2016') %>% # add metadata column
  pivot_wider(names_from = isscaap_group, values_from = mean_production_by_isscaap) %>%
  rename(mean_prod_carps_etc = 'Carps barbels and other cyprinids',
         mean_prod_misc_freshwater_fishes = 'Miscellaneous freshwater fishes',
         mean_prod_salmons_etc = 'Salmons trouts smelts',
         mean_prod_clams_etc = 'Clams cockles arkshells',
         mean_prod_cod_etc = 'Cods hakes haddocks',
         mean_prod_corals = 'Corals',
         mean_prod_crabs_etc = 'Crabs sea-spiders',
         mean_prod_flounders_etc = 'Flounders halibuts soles',
         mean_prod_freshwater_crustacea = 'Freshwater crustaceans',
         mean_prod_herrings_etc = 'Herrings sardines anchovies',
         mean_prod_lobsters_etc = 'Lobsters spiny-rock lobsters',
         mean_prod_marine_fishes_noID = 'Marine fishes not identified',
         mean_prod_misc_coastal_fishes = 'Miscellaneous coastal fishes',
         mean_prod_misc_demersal_fishes = 'Miscellaneous demersal fishes',
         mean_prod_misc_pelagic_fishes = 'Miscellaneous pelagic fishes',
         mean_prod_mussels = 'Mussels',
         mean_prod_river_eels = 'River eels',
         mean_prod_sea_urchins_etc = 'Sea-urchins and other echinoderms',
         mean_prod_shads = 'Shads',
         mean_prod_sharks_etc = 'Sharks rays chimaeras',
         mean_prod_shrimps_etc = 'Shrimps prawns',
         mean_prod_squids_etc = 'Squids cuttlefishes octopuses',
         mean_prod_tunas_etc = 'Tunas bonitos billfishes',
         mean_prod_misc_marine_crustacea = 'Miscellaneous marine crustaceans',
         mean_prod_misc_marine_molluscs = 'Miscellaneous marine molluscs',
         mean_prod_oysters = 'Oysters',
         mean_prod_tilapias_etc = 'Tilapias and other cichlids',
         mean_prod_abalones_etc = 'Abalones winkles conchs',
         mean_prod_brown_seaweeds = 'Brown seaweeds',
         mean_prod_frogs_etc = 'Frogs and other amphibians',
         mean_prod_green_seaweeds = 'Green seaweeds',
         mean_prod_king_crabs_etc = 'King crabs squat-lobsters',
         mean_prod_krill_etc = 'Krill planktonic crustaceans',
         mean_prod_misc_plants = 'Miscellaneous aquatic plants',
         mean_prod_scallops_etc = 'Scallops pectens',
         mean_prod_sturgeons_etc = 'Sturgeons paddlefishes',
         mean_prod_misc_aqua_inverts = 'Miscellaneous aquatic invertebrates',
         mean_prod_misc_diadromous_fishes = 'Miscellaneous diadromous fishes',
         mean_prod_pearls_etc = 'Pearls mother-of-pearl shells',
         mean_prod_tunicates = 'Sea-squirts and other tunicates',
         mean_prod_sponges = 'Sponges',
         mean_prod_turtles = 'Turtles',
         mean_prod_red_seaweeds = 'Red seaweeds',
         mean_prod_misc_aqua_mammals = 'Miscellaneous aquatic mammals',
         mean_prod_freshwater_molluscs = 'Freshwater molluscs',
         mean_pro_horeshoe_crabs_etc = 'Horseshoe crabs and other arachnoids')
  

write.csv(fishstat_dat_by_isscaap, file.path(outdir, "production_by_isccaap_faostat_mean_2006-2016.csv"), row.names = FALSE, quote = FALSE)


