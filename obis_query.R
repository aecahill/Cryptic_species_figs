library(dplyr)
library(robis)

#read in list of nominal species. 
#Make sure there are no spaces after species names!
specieslist<-read.csv("C:/Users/aecsk/Desktop/specieslist.csv")

#initate blank dataframe
occ = NULL

nums = NULL

for (i in specieslist$SpeciesName) { 
  sp <- try(occurrence(i), silent=TRUE) #looking for species in OBIS, pull occurrence data
 
  trop<-try(between(sp$decimalLatitude,-23.5,23.5),silent=TRUE) #check if each obs is in the tropics
  istropocc<-length(trop[trop== TRUE])>0 #see if there are any obs in the tropics
  istropnums<-length(trop[trop== TRUE])
  
  #same for northern temp zone
  ntemp<-try(between(sp$decimalLatitude,23.5,66.5),silent=TRUE)
  isntempocc<-length(ntemp[ntemp== TRUE])>0
  isntempnums<-length(ntemp[ntemp== TRUE])
  
  #same for northern polar zone
  npol<-try(between(sp$decimalLatitude,66.5,90),silent=TRUE)
  isnpolocc<-length(npol[npol== TRUE])>0
  isnpolnums<-length(npol[npol== TRUE])
  
  #same for southern temperate zone
  stemp<-try(between(sp$decimalLatitude,-66.5,-23.5),silent=TRUE)
  isstempocc<-length(stemp[stemp == TRUE])>0
  isstempnums<-length(stemp[stemp == TRUE])
  
  #same for southern polar zone
  spol<-try(between(sp$decimalLatitude,-90,-66.5),silent=TRUE)
  isspolocc<-length(spol[spol == TRUE])>0
  isspolnums<-length(spol[spol == TRUE])
  
  sprowocc<-c(i,istropocc,isntempocc,isnpolocc,isstempocc,isspolocc) #bind all T/F values for a species together
  sprownums<-c(i,istropnums,isntempnums,isnpolnums,isstempnums,isspolnums) 
  
  occ<-rbind(occ,sprowocc) #add to dataset
  nums<-rbind(nums,sprownums)
}

colnames(occ)<-c("Name","Trop","NTemp","NPol","STemp","SPol")
colnames(nums)<-c("Name","Trop","NTemp","NPol","STemp","SPol")

occ
nums

#Getting the number of species in overall latitudinal bands out of obis

taxanpol <- checklist(taxonid=2, geometry = "POLYGON ((-150 66.5, 150 66.5, 150 90, -150 90, -150 66.5))")
taxaspol<- checklist(taxonid=2, geometry = "POLYGON ((-150 -90, 150 -90, 150 -66.5, -150 -66.5, -150 -90))")
taxantemp<-checklist(taxonid=2, geometry = "POLYGON ((-150 23.5, 150 23.5, 150 66.5, -150 66.5, -150 23.5))")
taxastemp<-checklist(taxonid=2, geometry = "POLYGON ((-150 -66.5, 150 -66.5, 150 -23.5, -150 -23.5, -150 -66.5))")
taxatrop<-checklist(taxonid=2, geometry = "POLYGON ((-150 -23.5, 150 -23.5, 150 23.5, -150 23.5, -150 -23.5))")

NorthPolar<-length(taxanpol$scientificName)
SouthPolar<-length(taxaspol$scientificName)
NorthTemperate<-length(taxantemp$scientificName)
SouthTemperate<-length(taxastemp$scientificName)
Tropical<-length(taxatrop$scientificName)

f<-length(taxanpol$taxonRank[taxanpol$taxonRank=="Species"])
g<-length(taxaspol$taxonRank[taxaspol$taxonRank=="Species"])
h<-length(taxantemp$taxonRank[taxantemp$taxonRank=="Species"])
i<-length(taxastemp$taxonRank[taxastemp$taxonRank=="Species"])
j<-length(taxatrop$taxonRank[taxatrop$taxonRank=="Species"])

alltaxa<-rbind(NorthPolar,NorthTemperate,Tropical,SouthTemperate,SouthPolar)
justspp<-rbind(f,h,j,i,g)

resultstab<-cbind(alltaxa, justspp)

colnames(resultstab)<-c("All Taxa","Species Only")

#Pulling the species and finding the phyla

npolspecies<-taxanpol[taxanpol$taxonRank=="Species",]
ntempspecies<-taxantemp[taxantemp$taxonRank=="Species",]
tropspecies<-taxatrop[taxatrop$taxonRank=="Species",]
stempspecies<-taxastemp[taxastemp$taxonRank=="Species",]
spolspecies<-taxaspol[taxaspol$taxonRank=="Species",]

npolbyphylum<-as.data.frame(cbind(npolspecies$scientificName,npolspecies$phylum))
ntempbyphylum<-as.data.frame(cbind(ntempspecies$scientificName,ntempspecies$phylum))
tropbyphylum<-as.data.frame(cbind(tropspecies$scientificName,tropspecies$phylum))
stempbyphylum<-as.data.frame(cbind(stempspecies$scientificName,stempspecies$phylum))
spolbyphylum<-as.data.frame(cbind(spolspecies$scientificName,spolspecies$phylum))

#making a frequency table for each zone

npolfreqs<-table(npolbyphylum$V2)
ntempfreqs<-table(ntempbyphylum$V2)
tropfreqs<-table(tropbyphylum$V2)
stempfreqs<-table(stempbyphylum$V2)
spolfreqs<-table(spolbyphylum$V2)

#extract files

write.csv(npolbyphylum,"C:/Users/aecsk/OneDrive/Desktop/npolbyphylum.csv")
write.csv(ntempbyphylum,"C:/Users/aecsk/OneDrive/Desktop/ntempbyphylum.csv")
write.csv(tropbyphylum,"C:/Users/aecsk/OneDrive/Desktop/tropbyphylum.csv")
write.csv(stempbyphylum,"C:/Users/aecsk/OneDrive/Desktop/stempbyphylum.csv")
write.csv(spolbyphylum,"C:/Users/aecsk/OneDrive/Desktop/spolbyphylum.csv")

write.csv(npolfreqs,"C:/Users/aecsk/Desktop/npolfreqs.csv")
write.csv(ntempfreqs,"C:/Users/aecsk/Desktop/ntempfreqs.csv")
write.csv(tropfreqs,"C:/Users/aecsk/Desktop/tropfreqs.csv")
write.csv(stempfreqs,"C:/Users/aecsk/Desktop/stempfreqs.csv")
write.csv(spolfreqs,"C:/Users/aecsk/Desktop/spolfreqs.csv")

##pulling all species from OBIS to then run the code above

animalia<-checklist(taxonid=2)
animalspp<-animalia[animalia$taxonRank=="Species",]
animals<-cbind(animalspp$scientificName,animalspp$phylum)
write.csv(animals,"C:/Users/aecsk/Desktop/speciesnames.csv")


#reading back in the file? not sure why i spit it out
animals<-(read.csv("C:/Users/aecsk/Desktop/speciesnames.csv"))

# OK this loop was working but taking FOREVER.

occani<-NULL

for (i in animals$V1) { 
  npolani <- try(occurrence(i,geometry="POLYGON ((-150 66.5, 150 66.5, 150 90, -150 90, -150 66.5))"), silent=TRUE) #looking for species in OBIS, pull occurrence data
  ntempani<- try(occurrence(i,geometry="POLYGON ((-150 23.5, 150 23.5, 150 66.5, -150 66.5, -150 23.5))"), silent=TRUE) #looking for species in OBIS, pull occurrence data
  tropani<-try(occurrence(i,geometry = "POLYGON ((-150 -23.5, 150 -23.5, 150 23.5, -150 23.5, -150 -23.5))"), silent=TRUE)
  stempani<-try(occurrence(i,geometry = "POLYGON ((-150 -66.5, 150 -66.5, 150 -23.5, -150 -23.5, -150 -66.5))"), silent=TRUE)
  spolani<-try(occurrence(i,geometry = "POLYGON ((-150 -90, 150 -90, 150 -66.5, -150 -66.5, -150 -90))"), silent=TRUE)

  occzones<-c(i,length(npolani),length(ntempani),length(tropani),length(stempani),length(spolani))
  occani<-rbind(occani,occzones)
  }

colnames(occani)<-c("Name","NPol","NTemp","Trop","STemp","SPol")

dim(occani)
head(occani)


#Trying a different different loop that depends on both the animal readin and the lists of species per zone that were generated earlier.

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

