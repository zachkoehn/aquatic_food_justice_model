#######################
# Cleaning Ethnologue Global dataset (Simons, Gary F. and Charles D. Fennig (eds.). 2017. Ethnologue:
# Languages of the World, Twentieth edition. Dallas, Texas: SIL International. 
# http://www.ethnologue.com.
# Eva Maire
# e.maire@lancaster.ac.uk
#######################

directory <- "/Volumes/EM2T/Aquatic_Food_Justice/ethnologue"
setwd(directory)

# load dataset and package
library(countrycode)

#Extract country information using {countrycode}
db <- data.frame(codelist$country.name.en,codelist$iso3c,codelist$iso3n,codelist$iso2c)
names(db)<-c("country_name","iso3c","iso3n","iso2c")

#Delete countries taht don't have iso3c
db <- db[-which(is.na(db$iso3c)==T),]

#Load Ethnologue datasets
lic <- read.table("Table_of_LICs.tab", sep="\t", header=TRUE)
countries <- read.table("Table_of_Countries.tab", sep="\t", header=TRUE)
languages <- read.table("Table_of_Languages.tab", sep="\t", header=TRUE)

#Extract number of Established Languages
db$nb_languages_established <- rep(-999,nrow(db))

for(k in 1:nrow(db)){
  subdata <- lic[which(lic$Country_Code==db$iso2c[k]),]
  if(nrow(subdata)>0){
    db$nb_languages_established[k] <- length(which(subdata$Is_Established=="TRUE"))
  }else{
    db$nb_languages_established[k] <- NA
  }#end of else
}#end of k

#Add Greenberg diversity index: the probability that any two people of the country selected at random would have different mother tongues. 
# The highest possible value, 1, indicates total diversity (that is, no two people have the same mother tongue) 
# while the lowest possible value, 0, indicates no diversity at all (that is, everyone has the same mother tongue).

divtomerge <- countries[,c("Country_Code","Diversity")]

#Merge data
tot <- merge(db,divtomerge,by.x="iso2c",by.y="Country_Code",all.x=T)
summary(tot)

# Add % pop with institutional language as 1st
tot$prop_pop_l1_inst <- rep(-999,nrow(tot))

for(k in 1:nrow(tot)){
  subdata <- lic[which(lic$Country_Code==tot$iso2c[k]),]
  if(nrow(subdata)>0 & length(countries$Population[which(countries$Country_Code==tot$iso2c[k])])>0 ){
    tot$prop_pop_l1_inst[k] <- sum(subdata[which(subdata$Institutional==1),"L1_Users"],na.rm=T) / sum(subdata[,"All_Users"],na.rm=T)
  }else{
    tot$prop_pop_l1_inst[k] <- "NA"
  }#end of else
}#end of k

tot$prop_pop_l1_inst<-as.numeric(tot$prop_pop_l1_inst)
dat.final <- tot
names(dat.final)

#Rename variables
names(dat.final)[which(names(dat.final)=="Diversity")] <- "language_diversity"
summary(dat.final)

# and write the csv  
write.csv(dat.final, file="Ethnicity.csv")       

#End of script
#

