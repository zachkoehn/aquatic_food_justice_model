#######################
# Extract % working population, ageing population and under 5 years old (youth)
# from the wpp2019 R package
# Eva Maire
# e.maire@lancaster.ac.uk
#######################

directory <- "/Volumes/EM2T/Aquatic_Food_Justice"
setwd(directory)

# load dataset and packages
library(tidyverse)
library(countrycode)
library(wpp2019)
library(tidyr)

#Extract country information using {countrycode}
db <- data.frame(codelist$country.name.en,codelist$iso3c,codelist$iso3n)
names(db)<-c("country.name","iso3c","iso3n")

#Load data
data(pop)

#we focus here on popF and popM 
head(popF)
head(popM)

#Check the first 3 columns are identical
isTRUE(length(which(popF[,1] == popM[,1])==T) == nrow(popF))
isTRUE(length(which(popF[,2] == popM[,2])==T) == nrow(popF))
isTRUE(length(which(popF[,3] == popM[,3])==T) == nrow(popF))

country_code <- popF[,1]
name <-   popF[,2]
age <- popF[,3]

sumtot <- popF[,-c(1:3)] + popM[,-c(1:3)]

pop_tot <- data.frame(country_code,name,age,sumtot)
colnames(pop_tot) <- str_replace_all(colnames(pop_tot),"X","")

pop_tot_2010_2020 <- pop_tot[,which(colnames(pop_tot)>2005)]

#Change format
pop.long <- pivot_longer(pop_tot_2010_2020, cols=4:6, names_to = "year", values_to = "population")

#Extract % of specific population groups:
#working population defined as ages 15-64
#ageing population defined as population older than 64 
#youth defined as under 5 years old following World Bank thresholds

pop_stat <- pop.long %>%
  mutate(group = case_when(
                          age == "0-4" ~ "underfive",
                          age == "5-9" ~ "other",
                          age == "10-14" ~ "other",
                          age == "15-19" ~ "working",
                          age == "20-24" ~ "working",
                          age == "25-29" ~ "working",
                          age == "30-34" ~ "working",
                          age == "35-39" ~ "working",
                          age == "40-44" ~ "working",
                          age == "45-49" ~ "working",
                          age == "50-54" ~ "working",
                          age == "55-59" ~ "working",
                          age == "60-64" ~ "working",
                          age == "65-69" ~ "ageing",
                          age == "70-74" ~ "ageing",
                          age == "75-79" ~ "ageing",
                          age == "80-84" ~ "ageing",
                          age == "85-89" ~ "ageing",
                          age == "90-94" ~ "ageing",
                          age == "95-99" ~ "ageing",
                          age == "100+"  ~ "ageing" )) %>%
  mutate(iso3c = countrycode(pop.long$country_code, origin = "iso3n", destination = "iso3c")) %>% #warning message as data are provided for the World and some regions
  group_by(country_code, name, year) %>%
  mutate(pop_tot = sum(population)) %>% #compute total population for each year
  ungroup() %>%
  group_by(country_code, name, iso3c, year, group) %>%
  mutate(prop_pop = population/pop_tot) %>% #compute % population
  summarise(prop_population_group = sum(prop_pop)) %>% #and sum by group
  group_by(country_code, name, iso3c, group) %>%
  summarise(mean_prop_population_group = mean(prop_population_group)) %>% #average across 2010, 2015 and 2020
  rename(iso3n = "country_code", country.name = "name") %>%
  mutate(year.range = '2010-2015-2020') # add metadata column

#Wide format
dat.wide <- pivot_wider(pop_stat, names_from = group, values_from = mean_prop_population_group) 

dat.final<- dat.wide %>%
  rename(mean.prop.ageing = "ageing", mean.prop.working = "working", mean.prop.underfive = "underfive") %>%
  select(country.name,iso3n,iso3c,mean.prop.ageing,mean.prop.underfive,mean.prop.working,year.range) 

# and write the csv  
write.csv(dat.final, file="prop_population_groups.csv")       

#End of script
#