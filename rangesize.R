
## How to find range size in OBIS

library(robis)
library(dplyr)

# For each species, get occurrence data 

##Part 1: pulling all animal species from OBIS

animalia<-checklist(taxonid=2)   #pulls list of all animals from OBIS
animalspp<-animalia[animalia$taxonRank=="Species",]   #ONLY use those that are ID'd to the species level
animals<-as.data.frame(cbind(animalspp$scientificName,animalspp$phylum))  #create table of names and phyla
animals2<-na.omit(animals) #removing species whose name is "NA"

## Part 2: get occurrences

diffs = NULL
colname<- c("scientificName", "decimalLatitude", "decimalLongitude")
species_diffs = NULL

for (i in animals2$V1) { 
  occ<-try(occurrence(i,fields=colname),silent=TRUE)
  try(colnames(occ)<-c("Name","Longitude","Latitude"),silent = TRUE)
  #range_long<-try(max(occ$Longitude, na.rm=TRUE) - min(occ$Longitude, na.rm=TRUE),silent=TRUE)
  #The below code is the new longitude code
  if ((max(occ$decimalLongitude)>27) & (min(occ$decimalLongitude)<(-67))) {  #I got these numbers from marine regions
    #27 is the western edge of the Indian Ocean and -67 is the eastern edge of the Pacific
    east<-occ[occ$decimalLongitude > 0,]
    west<-occ[occ$decimalLongitude < 0,]
    range_long<-(180-min(east$decimalLongitude))+(180-abs(max(west$decimalLongitude)))  
  } else {
    range_long<-max(occ$decimalLongitude) - min(occ$decimalLongitude) #this is the original line and will pick up any species not purely indo-Pacific
    #a species must be TRANS-pacific (across the 180 mark) to meet the first statement
  }
  
  range_lat<-try(max(occ$Latitude, na.rm=TRUE) - min(occ$Latitude, na.rm=TRUE),silent=TRUE)
  diffs<-try(cbind(i,range_long,range_lat),silent=TRUE)
  species_diffs = try(as.data.frame(rbind(species_diffs,diffs)),silent=TRUE)

}

write.csv(species_diffs,"C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs/species_diffs.csv")


## Here is where I checked to see if the species contain CS based on the survey

specieslist<-read.csv("C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs/specieslist.csv")

species_diffstable = NULL
in_survey = NULL

for (i in species_diffs$i) {
  b <- i %in% specieslist$SpeciesName
  in_survey <- rbind(in_survey,b)
  
}

species_diffstable = cbind(species_diffs,in_survey)

write.csv(species_diffstable,"C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs/species_diffstable.csv")

species_diffstable2<-read.csv("C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs/species_diffstable.csv")

#Attempting some stats

#First, remove species without any occurrence data (gave -Inf results)
#AND removing data with no longitude variation (range = 0)

#sort on longitude
specieslong<-species_diffstable2[order(species_diffstable2$range_long),]

#remove -INF and zero values
#Note: I just did this by figuring out where they were in the sorted data frame; did not write code for it.

specieslong_noNA<-specieslong[30616:120772,]

#Mean and sd for in survey vs not

tapply(as.numeric(specieslong_noNA$range_long),specieslong_noNA$in_survey,mean)
tapply(as.numeric(specieslong_noNA$range_long),specieslong_noNA$in_survey,sd)

#repeat for latitude

specieslat<-species_diffstable2[order(species_diffstable2$range_lat),]
specieslat_noNA<-specieslat[30801:120772,]

tapply(as.numeric(specieslat_noNA$range_lat),specieslat_noNA$in_survey,mean)
tapply(as.numeric(specieslat_noNA$range_lat),specieslat_noNA$in_survey,sd)


long<-ggplot(specieslong_noNA,aes(y=as.numeric(as.character(range_long)),x=in_survey))+
  #geom_jitter(position=position_jitter(0.2),alpha=0.75, cex=1)+
  geom_boxplot(alpha=0.75)+
  stat_summary(fun=mean, geom="point", shape=18,
               size=5, color="black")+
  #stat_summary(fun.data=data_summary, color="black", size=1)+
  labs(x ="Is In Survey", y = "Longitudinal Range (degrees)")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  theme(axis.text.x = element_text(angle = 65,hjust=1))

lat<-ggplot(specieslat_noNA,aes(y=as.numeric(as.character(range_lat)),x=in_survey))+
  #geom_jitter(position=position_jitter(0.2),alpha=0.75, cex=1)+
  geom_boxplot(alpha=0.75)+
  stat_summary(fun=mean, geom="point", shape=18,
               size=5, color="black")+
  #stat_summary(fun.data=data_summary, color="black", size=1)+
  labs(x ="Is In Survey", y = "Latitudinal Range (degrees)")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  theme(axis.text.x = element_text(angle = 65,hjust=1))

library(cowplot)

plot_grid(lat,long,labels=c("A","B"),ncol=2)