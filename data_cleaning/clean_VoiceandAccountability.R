#######################
# Cleaning Voice and Accountability index (dataset from Kaufmann et al 2010)
# Eva Maire
# e.maire@lancaster.ac.uk
#######################

directory <- "/Volumes/EM2T/Aquatic_Food_Justice"
setwd(directory)

# load dataset and packages
library(tidyverse)
library(countrycode)
library(xlsx)

dat_raw <- read.xlsx("wgidataset.xlsx",
  sheetName = "VoiceandAccountability",
  startRow = 14,
  header=F)

#Replace #N/A by NA
dat_raw2 <- data.frame(lapply(dat_raw, function(x) {
  gsub("#N/A", "NA", x)
}))

#Keep years 2008-2018
year <- which(dat_raw2[1,]>2007)
clean_year <- dat_raw2[,c(1,2,year)]

#Combine colnames that are provided on 2 separate lines
newcolnames <- paste(clean_year[2,], clean_year[1,], sep=".")
newcolnames[c(1,2)] <- c("Country.Territory","Code")

dat_raw3 <- clean_year[-c(1,2),]
colnames(dat_raw3) <- newcolnames
head(dat_raw3)

#Keep only Voice and Accountability estimates
estimate <- which(grepl("Estimate",colnames(dat_raw3))==TRUE)
clean_estimate <- dat_raw3[,estimate]

#Numeric estimates
dat_numeric <- data.frame(lapply(clean_estimate, function(x) {
  as.numeric(x)
})) #warning message as some values are missing

#Average estimates between 2008 and 2018
Country.Territory <- dat_raw3$Country.Territory
Code <- dat_raw3$Code
year.range <- rep("2008-2018",nrow(dat_raw3))
mean.voice.and.accountability <- apply(dat_numeric,1,mean)

dat_clean <- data.frame(Country.Territory,Code,mean.voice.and.accountability,year.range)

#Add country information using {countrycode}
db <- data.frame(codelist$country.name.en,codelist$iso3c,codelist$iso3n)
names(db)<-c("country.name","iso3c","iso3n")

#Correct ISO codes
#Andorra: iso3c = AND
isTRUE(db[which(db$country.name=="Andorra"),"iso3c"] == dat_clean[which(dat_clean[,"Country.Territory"]=="Andorra"),"Code"])
dat_clean[which(dat_clean["Country.Territory"]=="Andorra"),"Code"] <- db[which(db$country.name=="Andorra"),"iso3c"]

#Netherlands Antilles: no iso3c 
isTRUE(db[which(db$country.name=="Netherlands Antilles"),"iso3c"] == dat_clean[which(dat_clean[,"Country.Territory"]=="Netherlands Antilles (former)"),"Code"])

#Kosovo: no iso3c 
isTRUE(db[which(db$country.name=="Kosovo"),"iso3c"] == dat_clean[which(dat_clean[,"Country.Territory"]=="Kosovo"),"Code"])

#Romania: iso3c = ROU
isTRUE(db[which(db$country.name=="Romania"),"iso3c"] == dat_clean[which(dat_clean[,"Country.Territory"]=="Romania"),"Code"])
dat_clean[which(dat_clean["Country.Territory"]=="Romania"),"Code"] <- db[which(db$country.name=="Romania"),"iso3c"]

#Timor-Leste: 
isTRUE(db[which(db$country.name=="Timor-Leste"),"iso3c"] == dat_clean[which(dat_clean[,"Country.Territory"]=="Timor-Leste"),"Code"])
dat_clean[which(dat_clean["Country.Territory"]=="Timor-Leste"),"Code"] <- db[which(db$country.name=="Timor-Leste"),"iso3c"]

#Gaza: no iso3c 

#Congo:
#Congo - Brazzaville = Congo, Rep.
#Congo - Kinshasa = Congo, Dem. Rep.
isTRUE(db[which(db$country.name=="Congo - Brazzaville"),"iso3c"] == dat_clean[which(dat_clean[,"Country.Territory"]=="Congo, Rep."),"Code"])
isTRUE(db[which(db$country.name=="Congo - Kinshasa"),"iso3c"] == dat_clean[which(dat_clean[,"Country.Territory"]=="Congo, Dem. Rep."),"Code"])

dat_clean[which(dat_clean["Country.Territory"]=="Congo, Dem. Rep."),"Code"] <- db[which(db$country.name=="Congo - Kinshasa"),"iso3c"]

vaa <- merge(dat_clean,db,by.x="Code",by.y="iso3c",all.x=T)

#Check missing values
vaa[which(is.na(vaa$iso3n)==T),]

db2 <- db[,-which(colnames(db)=="iso3n")]

dat_final <- merge(vaa,db2,by="country.name",all.x=T,all.y=F)
head(dat_final) 

# and write the csv file
write.csv(dat_final,file = "voice_and_accountability_2008_2018.csv")

#End of script