specieslist<-read.csv("C:/Users/aecsk/OneDrive/Desktop/synonyms.csv",header=TRUE)

names=NULL

for (i in specieslist$Name) { 
  sp <- try(occurrence(i), silent=TRUE) #looking for species in OBIS, pull occurrence data
  d<-sp$scientificName[1]
  
  names<-rbind(names,d)
  
}

write.csv(names,"C:/Users/aecsk/OneDrive/Desktop/syn_corrected.csv")
