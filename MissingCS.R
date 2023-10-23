#Writing a script to do OBIS code for paper 3 (missing species)

library(dplyr)
library(robis)

setwd("C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs")

##Part 1: pulling all animal species from OBIS

animalia<-checklist(taxonid=2)   #pulls list of all animals from OBIS
write.csv(animalia, "animal_list.csv") #writing mostly so I don't lose it
animalspp<-animalia[animalia$taxonRank=="Species",]   #ONLY use those that are ID'd to the species level
animals<-as.data.frame(cbind(animalspp$scientificName,animalspp$phylum,animalspp$class))  #create table of names and phyla

##Part2: need polygons for all seas - read in Mark's code
Arctic_coor<-read.csv("Arctic_coor.csv")
Atlantic_coor<-read.csv("Atlantic_coor.csv")
Indian_coor<-read.csv("Indian_coor.csv")
Pacific_coor<-read.csv("Pacific_coor.csv")
SouthOcean_coor<-read.csv("SouthOcean_coor.csv")

##Part3: create checklists for each sea

#need to somehow get these checklists into a single polygon or something, look at Mark's code