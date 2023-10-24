#Writing a script to do OBIS code for paper 3 (missing species)

library(dplyr)
library(robis)

setwd("C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs")

##Part 1: pulling all animal species from OBIS

animalia<-checklist(taxonid=2)   #pulls list of all animals from OBIS
write.csv(animalia, "animal_list.csv") #writing so I don't have to pull the data every time
animalia<-read.csv("animal_list.csv") #read back in if needed
animalspp<-animalia[animalia$taxonRank=="Species",]   #ONLY use those that are ID'd to the species level
animals<-as.data.frame(cbind(animalspp$scientificName,animalspp$phylum,animalspp$class))  #create table of names and phyla
colnames(animals)<-c("Species","Phylum","Class")


##Part2: need polygons for all seas - read in Mark's code
Arctic_coor<-read.csv("Arctic_coor.csv")
Atlantic_coor<-read.csv("Atlantic_coor.csv")
Indian_coor<-read.csv("Indian_coor.csv")
Pacific_coor<-read.csv("Pacific_coor.csv")
SouthOcean_coor<-read.csv("SouthOcean_coor.csv")

##Part3: create checklists for each sea
#Arctic Ocean
num_poly_Arctic<-c(1:length(Arctic_coor[,3])) #Number of polygons to search in
Art_check<-data.frame()

for (i in num_poly_Arctic){
  check<-checklist(taxonid=2, geometry = Arctic_coor[i,3])
  extr<-as.data.frame(cbind(check$scientificName,check$phylum,check$class))  #create table of names and phyla
  Art_check<-rbind(Art_check,extr)
}

colnames(Art_check)<-c("Species","Phylum","Class")
Art_check2 <- Art_check %>% distinct(Species, .keep_all = TRUE)  #Remove duplicates (ie spp found in multiple polygons)


#Indian Ocean
num_poly_Indian<-c(1:length(Indian_coor[,3])) #Number of polygons to search in
Ind_check<-data.frame()

for (i in num_poly_Indian){
  check<-checklist(taxonid=2, geometry = Indian_coor[i,3])
  extr<-as.data.frame(cbind(check$scientificName,check$phylum,check$class))  #create table of names and phyla
  Ind_check<-rbind(Ind_check,extr)
}
colnames(Ind_check)<-c("Species","Phylum","Class")
Ind_check2 <- Ind_check %>% distinct(Species, .keep_all = TRUE)  #Remove duplicates (ie spp found in multiple polygons)

#Southern Ocean
num_poly_South<-c(1:length(SouthOcean_coor[,3])) #Number of polygons to search in
South_check<-data.frame()

for (i in num_poly_South){
  check<-checklist(taxonid=2, geometry = SouthOcean_coor[i,3])
  extr<-as.data.frame(cbind(check$scientificName,check$phylum,check$class))  #create table of names and phyla
  South_check<-rbind(South_check,extr)
}
colnames(South_check)<-c("Species","Phylum","Class")
South_check2 <- South_check %>% distinct(Species, .keep_all = TRUE)  #Remove duplicates (ie spp found in multiple polygons)

#Atlantic Ocean
num_poly_Atlantic<-c(1:length(Atlantic_coor[,3])) #Number of polygons to search in
Atl_check<-data.frame()

for (i in num_poly_Atlantic){
  check<-checklist(taxonid=2, geometry = Atlantic_coor[i,3])
  extr<-as.data.frame(cbind(check$scientificName,check$phylum,check$class))  #create table of names and phyla
  Atl_check<-rbind(Atl_check,extr)
}
colnames(Atl_check)<-c("Species","Phylum","Class")
Atl_check2 <- Atl_check %>% distinct(Species, .keep_all = TRUE)  #Remove duplicates (ie spp found in multiple polygons)

#Pacific Ocean
num_poly_Pacific<-c(1:length(Pacific_coor[,3])) #Number of polygons to search in
Pac_check<-data.frame()

for (i in num_poly_Pacific){
  check<-checklist(taxonid=2, geometry = Pacific_coor[i,3])
  extr<-as.data.frame(cbind(check$scientificName,check$phylum,check$class))  #create table of names and phyla
  Pac_check<-rbind(Pac_check,extr)
}
colnames(Pac_check)<-c("Species","Phylum","Class")
Pac_check2 <- Pac_check %>% distinct(Species, .keep_all = TRUE)  #Remove duplicates (ie spp found in multiple polygons)


# For all animal species, need following info:
# CS or no
# Present in each ocean, yes or no
# So read in the CS list
# Loop over the animal list:
# Is in CS (T/F)?
# Is in .... each ocean, 5 calls, T/F
# Wind up with table of Species, Phylum, Class, then those columns.
# Maybe use animalspp instead of the smaller df? I don't think it'll do much for calc time to use all the info, better not to lose it.