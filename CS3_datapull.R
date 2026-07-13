library(dplyr) # to manipulate data
library(stringr)
library(GeoRange)
library(robis)
library(rentrez)
library(tidyr)
library(ggplot2)
library(mregions2) # package with marine geographic information

#read in worms
#This file is from a May 2026 download of WoRMS and has been manipulated to turn it into a csv and put the names in the first column
#Otherwise it wasn't reading in properly
# I think this file is already set to include only extant and marine species

taxonfile<-read.csv("C:/Users/acahill/Documents/WoRMS_download_2026-05-01/taxon_try2.csv", header=T)

#Now, remove all taxa we don't want.

wormsdata<-taxonfile[taxonfile$taxonRank=="Species",] #only species
wormsdata<-wormsdata[wormsdata$kingdom=="Animalia",]  #only animals

# Remove classes not considered in CS paper
wormsdata<-wormsdata %>% filter((phylum != 'Acanthocephala') %>% replace_na(TRUE))
wormsdata<-wormsdata %>% filter((class != 'Myxozoa') %>% replace_na(TRUE))
wormsdata<-wormsdata %>% filter((class != 'Cestoda') %>% replace_na(TRUE))
wormsdata<-wormsdata %>% filter((class != 'Monogenea') %>% replace_na(TRUE))
wormsdata<-wormsdata %>% filter((class != 'Monogenoidea') %>% replace_na(TRUE))
wormsdata<-wormsdata %>% filter((class != 'Monopisthocotyla') %>% replace_na(TRUE))
wormsdata<-wormsdata %>% filter((class != 'Polyopisthocotyla') %>% replace_na(TRUE))
wormsdata<-wormsdata %>% filter((class != 'Trematoda') %>% replace_na(TRUE))
wormsdata<-wormsdata %>% filter((class != 'Aves') %>% replace_na(TRUE)) #removing birds - there's loads of them but we didn't consider in the CS paper

#write out wormsdata file just because
#write.csv(wormsdata,"wormsdata.csv")

# Now I need to know if these species have NCBI data.

num_in_ncbi_sciname<-c()

for (i in wormsdata$scientificName[1:length(wormsdata$scientificName)]){
  spname<-paste(i,"[ORG]")    
  b<-entrez_search(db="nuccore",term=spname)
  d<-cbind(i,b$count)    
  num_in_ncbi_sciname<-rbind(num_in_ncbi_sciname,d)
}

