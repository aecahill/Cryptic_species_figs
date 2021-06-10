library(dplyr)
library(robis)


##Part 1: pulling all animal species from OBIS

animalia<-checklist(taxonid=2)
animalspp<-animalia[animalia$taxonRank=="Species",]
animals<-cbind(animalspp$scientificName,animalspp$phylum)

#Part 2: Getting the species (ALL SPECIES) in overall latitudinal bands out of obis

taxanpol <- checklist(taxonid=2, geometry = "POLYGON ((-150 66.5, 150 66.5, 150 90, -150 90, -150 66.5))")
taxaspol<- checklist(taxonid=2, geometry = "POLYGON ((-150 -90, 150 -90, 150 -66.5, -150 -66.5, -150 -90))")
taxantemp<-checklist(taxonid=2, geometry = "POLYGON ((-150 23.5, 150 23.5, 150 66.5, -150 66.5, -150 23.5))")
taxastemp<-checklist(taxonid=2, geometry = "POLYGON ((-150 -66.5, 150 -66.5, 150 -23.5, -150 -23.5, -150 -66.5))")
taxatrop<-checklist(taxonid=2, geometry = "POLYGON ((-150 -23.5, 150 -23.5, 150 23.5, -150 23.5, -150 -23.5))")


#Part 3: making a species list for each zone

npolspecies<-taxanpol[taxanpol$taxonRank=="Species",]
ntempspecies<-taxantemp[taxantemp$taxonRank=="Species",]
tropspecies<-taxatrop[taxatrop$taxonRank=="Species",]
stempspecies<-taxastemp[taxastemp$taxonRank=="Species",]
spolspecies<-taxaspol[taxaspol$taxonRank=="Species",]



#Part 4: For the list of all animals, find what zones they are in and make table

allzonesphyla = NULL

for (i in animals$V1){
  npol_present<-match(i,npolbyphylum$V1)
  ntemp_present<-match(i,ntempbyphylum$V1)
  trop_present<-match(i,tropbyphylum$V1)
  stemp_present<-match(i,stempbyphylum$V1)
  spol_present<-match(i,spolbyphylum$V1)
  
  speciesname<-cbind(i, npol_present, ntemp_present, trop_present, stemp_present, spol_present)
  allzonesphyla<-rbind(allzonesphyla,speciesname)
}

colnames(allzonesphyla)<-c("Species","Npol","Ntemp","Trop","Stemp","Spol")

#ask if number is NA or not and then 

allzonesphyla2<-allzonesphyla>0
allzonesphyla3<-cbind(allzonesphyla[,1],allzonesphyla2[,2:6])

#add phylum names using join, rename columns

allzonesphyla4<-cbind(allzonesphyla3,as.character(animals$V2))
colnames(allzonesphyla4)<-c("Species","Npol","Ntemp","Trop","Stemp","Spol","Phylum")

#output the file

write.csv(allzonesphyla4,"C:/Users/aecsk/OneDrive/Desktop/allspecies_by_zone.csv")

