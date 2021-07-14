
## How to find range size in OBIS

library(robis)

# For each species, get occurrence data (use Mark's code here)

##Part 1: pulling all animal species from OBIS

animalia<-checklist(taxonid=2)   #pulls list of all animals from OBIS
animalspp<-animalia[animalia$taxonRank=="Species",]   #ONLY use those that are ID'd to the species level
animals<-as.data.frame(cbind(animalspp$scientificName,animalspp$phylum))  #create table of names and phyla
animals2<-na.omit(animals)

## Part 2: get occurrences

diffs = NULL
colname<- c("scientificName", "decimalLatitude", "decimalLongitude")
species_diffs = NULL

for (i in animals2$V1[5603:120780]) { 
  occ<-try(occurrence(i,fields=colname),silent=TRUE)
  try(colnames(occ)<-c("Name","Longitude","Latitude"),silent = TRUE)
  range_long<-try(max(occ$Longitude, na.rm=TRUE) - min(occ$Longitude, na.rm=TRUE),silent=TRUE)
  range_lat<-try(max(occ$Latitude, na.rm=TRUE) - min(occ$Latitude, na.rm=TRUE),silent=TRUE)
  diffs<-try(cbind(i,range_long,range_lat),silent=TRUE)
  species_diffs = try(as.data.frame(rbind(species_diffs,diffs)),silent=TRUE)

  }

# Find difference; convert to km
# End goal: A table with species name, diff lat, diff long, which is bigger (lat or long)

