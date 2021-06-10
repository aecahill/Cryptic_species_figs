#This code pulls all animal species from the OBIS database overall, as well as specific latitudinal zones.
#The end result is a table of species and whether or not each species is present in a particular latitudinal zone.
#It considers 5 zones: 2 polar, 2 temperate, 1 tropical.
#It does not require any input files.

library(dplyr)
library(robis)


##Part 1: pulling all animal species from OBIS

animalia<-checklist(taxonid=2)   #pulls list of all animals from OBIS
animalspp<-animalia[animalia$taxonRank=="Species",]   #ONLY use those that are ID'd to the species level
animals<-as.data.frame(cbind(animalspp$scientificName,animalspp$phylum))  #create table of names and phyla

#Part 2: Getting the species (ALL SPECIES) in overall latitudinal bands out of obis
#Each line gets a list of animals from the polygon described here
#code will make five different species lists, one per zone

taxanpol <- checklist(taxonid=2, geometry = "POLYGON ((-150 66.5, 150 66.5, 150 90, -150 90, -150 66.5))")
taxaspol<- checklist(taxonid=2, geometry = "POLYGON ((-150 -90, 150 -90, 150 -66.5, -150 -66.5, -150 -90))")
taxantemp<-checklist(taxonid=2, geometry = "POLYGON ((-150 23.5, 150 23.5, 150 66.5, -150 66.5, -150 23.5))")
taxastemp<-checklist(taxonid=2, geometry = "POLYGON ((-150 -66.5, 150 -66.5, 150 -23.5, -150 -23.5, -150 -66.5))")
taxatrop<-checklist(taxonid=2, geometry = "POLYGON ((-150 -23.5, 150 -23.5, 150 23.5, -150 23.5, -150 -23.5))")


#Part 3: making a species list for each zone
#Each line filters to make a new dataframe of ONLY the taxa ID'd to species level

npolspecies<-taxanpol[taxanpol$taxonRank=="Species",]
ntempspecies<-taxantemp[taxantemp$taxonRank=="Species",]
tropspecies<-taxatrop[taxatrop$taxonRank=="Species",]
stempspecies<-taxastemp[taxastemp$taxonRank=="Species",]
spolspecies<-taxaspol[taxaspol$taxonRank=="Species",]

#Part 4: pulling scientific names and phyla for each species

npolbyphylum<-as.data.frame(cbind(npolspecies$scientificName,npolspecies$phylum))
ntempbyphylum<-as.data.frame(cbind(ntempspecies$scientificName,ntempspecies$phylum))
tropbyphylum<-as.data.frame(cbind(tropspecies$scientificName,tropspecies$phylum))
stempbyphylum<-as.data.frame(cbind(stempspecies$scientificName,stempspecies$phylum))
spolbyphylum<-as.data.frame(cbind(spolspecies$scientificName,spolspecies$phylum))


#Part 5: For the list of all animals, find what zones they are in and make table

allzonesphyla = NULL

#This loop takes each species in the animal list and matches it to the species list for each zone.
#The number in the output table is the number of the row where the species occurs, so it doesn't really mean much for our study
#The output is a table of species and whether or not they are found in each zone
#This part takes a while!

for (i in animals$V1){
  npol_present<-match(i,npolbyphylum$V1)
  ntemp_present<-match(i,ntempbyphylum$V1)
  trop_present<-match(i,tropbyphylum$V1)
  stemp_present<-match(i,stempbyphylum$V1)
  spol_present<-match(i,spolbyphylum$V1)
  
  speciesname<-cbind(i, npol_present, ntemp_present, trop_present, stemp_present, spol_present)
  allzonesphyla<-rbind(allzonesphyla,speciesname)
}

colnames(allzonesphyla)<-c("Species","Npol","Ntemp","Trop","Stemp","Spol") #relabel columns

#ask if number is NA or not and then 

allzonesphyla2<-allzonesphyla>0  #This line turns the numbers (which don't mean anything) into TRUE - means species is present in the zone.
allzonesphyla3<-cbind(allzonesphyla[,1],allzonesphyla2[,2:6])  #This line takes the species name from the first table and binds it to the TRUE/NA matrix of the second

#add phylum names, rename columns

allzonesphyla4<-cbind(allzonesphyla3,as.character(animals$V2))
colnames(allzonesphyla4)<-c("Species","Npol","Ntemp","Trop","Stemp","Spol","Phylum")

#output the file

write.csv(allzonesphyla4,"C:/Users/acahill/Documents/GitHub/Cryptic_species_figs/allspecies_by_zone.csv")

