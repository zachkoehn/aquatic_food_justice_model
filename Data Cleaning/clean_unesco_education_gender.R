#######################
# Cleaning UNESCO women's education
# Zach Koehn
# zkoehn@stanford.edu
#######################


# load dataset and packages
library(tidyverse)
library(here)
library(countrycode)
library(readxl)
library(R.utils)

# data from http://data.uis.unesco.org/#
dat_raw <- as.data.frame(read_excel(
  here(
    "data",
    "data_raw",
    "unesco_education",
    "Educational_attainment_-_Niveau_d'éducation_atteint.xlsx"
  ),
  sheet="Education attainment (EN)"
)
)

column_names <- as.character(dat_raw[4,])
dat_raw <- dat_raw[-c(1:4),]

# now aggregate annual GDP values by year and take the mean across 2006:2016
educational_attainment_vars <- c(
  "Upper secondary (ISCED 3) (%)",
  "Post-secondary non-tertiary (ISCED 4) (%)",
  "Short-cycle tertiary (ISCED 5) (%)",
  "Bachelor's or equivalent (ISCED 6) (%)",
  "Master's or equivalent (ISCED 7) (%)",
  "Doctoral or equivalent (ISCED 8) (%)"
)

column_names <- replace(column_names,seq(20,35,by=3),educational_attainment_vars)

names(dat_raw) <- column_names

dat_clean <- dat_raw[-c(1,2),] %>% #note this also removes the first row that classifies gender and the second row specifying column number
  select('Country or territory','Reference year',seq(20,35,by=3)) %>% #selects only columns we want (convenient column also includes 'both' genders)
  rename(country_name_en='Country or territory',year='Reference year',
         isced_3="Upper secondary (ISCED 3) (%)",
         isced_4="Post-secondary non-tertiary (ISCED 4) (%)",
         isced_5="Short-cycle tertiary (ISCED 5) (%)",
         isced_6="Bachelor's or equivalent (ISCED 6) (%)",
         isced_7="Master's or equivalent (ISCED 7) (%)",
         isced_8="Doctoral or equivalent (ISCED 8) (%)"
  ) %>%
  mutate(across(2:8,as.numeric)) %>% #coerces to numeric
  filter(year>2005 & year <2017 ) %>%
  filter(is.na(year)==FALSE ) %>%
  # this dataset has some  values that differentiate by age range (the (b) or (g) etc
  # so we need to strip these in order to aggregate across all obs for each country
  # (h) is the only potential concern, and only for argentina, as it denotes observations only from urban areas) - see raw excel for details
  mutate(country_name_en=str_replace_all(country_name_en, "\\(.*\\)","")) %>% #remove footnotes
  group_by(country_name_en,year) %>%
  transmute(women_educational_attainment_annual=sum(c_across(isced_3:isced_8),na.rm=TRUE)) %>% # sum across secondary & tertiary education levels, remove old variables
  group_by(country_name_en) %>% #now group by country and aggregate across year values
  summarize(mean_women_educ=mean(women_educational_attainment_annual,na.rm=TRUE)) # aggregate by mean

dat_final <- dat_clean %>%
  mutate(
    iso3c=countrycode(country_name_en,"country.name","iso3c"), # assign iso3c from country name
    iso3n=countrycode(iso3c,"iso3c","iso3n"), # assign iso3n from iso3c
    year_range="2006-2016" #add the year_range category
  ) %>%
  select(country_name_en,iso3c,iso3n,mean_women_educ,year_range) #only select variable 



# and write the csv 
write.csv(
  dat_final,
  here(
    "data",
    "data_clean",
    "women_educ_attain_mean_2006-2016.csv"
  ),
  row.names = FALSE
)



