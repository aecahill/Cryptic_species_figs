# Note June 13 2024: I am modifying this code from the BioRev paper
# I will remove the WRIMS overlap species from the big dataset to re-test the range size info


## STATISTICS BASED ON THE NOMINAL SPECIES DATASET - Cryptic species meta-analysis - august 2023

library(dplyr) # to manipulate data
library(vioplot) # remove this one if violin plots redone with ggplot (draw violin plots)
library(ggplot2) # used to make plots
library(ggpubr)    # to share common legend for several plots (with ggplot)
library(gridExtra) # to arrange several plots with ggplot
library(bestglm) # compares glm models based on a criterion (BIC was chosen here)
library(car) # used to check variance inflation factors
library(stringr)

###########################################################################
# Part_1 # Preparation of the data (load data, create variables, etc.)
###########################################################################
data<-read.csv2( "DATASET_187603_NominalSpecies.csv" ,header=T)
data<-as.data.frame(unclass(data),     # Convert all columns to factor (instead of characters, numericals do not change)
                    stringsAsFactors = TRUE)
data$dna<-factor(data$dna, levels=c("No seq","nuc","mito","nuc&mito")) # so the reference level becomes "No seq" for statistics
data$mitonuc <-(data$dna=="nuc&mito")  # boolean used below to build table per decade
data$hasRIOK[is.na(data$RIOK)]<-FALSE     # converts NA values to FALSE
data$phylum_class<-factor(paste0(data$phylum, data$class))



# test (required in minor revision step)
wilcox.test(dato$CHull_r100[dato$hasCS==TRUE], dato$CHull_r100[dato$hasCS==FALSE])
wilcox.test(dato$range_lat_r100[dato$hasCS==TRUE], dato$range_lat_r100[dato$hasCS==FALSE])
wilcox.test(dato$range_long_r100[dato$hasCS==TRUE], dato$range_long_r100[dato$hasCS==FALSE])


# NEW CODE FROM ABBY, June 2024
# Need to remove invasive species from database

#Read in wrims
wrims<-read.csv(file="WRIMS_taxon.csv"   , header=TRUE )

#Now filter to only animal species
wrims<-filter(wrims,(kingdom=="Animalia" & taxonRank=="Species")) 

non_invasive<- data #rename to make a separate dataframe
matchlist<- data$scientificName %in% wrims$acceptedNameUsage  #make vector of cases in wrims that have a match in the CS survey

ismatch<-which(matchlist==TRUE)

non_invasive2<-non_invasive[-ismatch,] #remove values that are not on the wrims list
invasive<-non_invasive[ismatch,]

#removing species without geographic info to make sure I don't screw up the math
invasive<-filter(invasive, range_lat_r100 != "NA")
non_invasive2<-filter(non_invasive2,range_lat_r100 != "NA")

#Why are there 300 species in wrims that are not in WoRMS???
# And why does the order of the matching make a difference?
#Did Anne already filter to marine = True, and is that the difference?

wilcox.test(non_invasive2$CHull_r100[non_invasive2$hasCS==TRUE], non_invasive2$CHull_r100[non_invasive2$hasCS==FALSE])
wilcox.test(non_invasive2$range_lat_r100[non_invasive2$hasCS==TRUE], non_invasive2$range_lat_r100[non_invasive2$hasCS==FALSE])
wilcox.test(non_invasive2$range_long_r100[non_invasive2$hasCS==TRUE], non_invasive2$range_long_r100[non_invasive2$hasCS==FALSE])

wilcox.test(invasive$CHull_r100[invasive$hasCS==TRUE], invasive$CHull_r100[invasive$hasCS==FALSE])
wilcox.test(invasive$range_lat_r100[invasive$hasCS==TRUE], invasive$range_lat_r100[invasive$hasCS==FALSE])
wilcox.test(invasive$range_long_r100[invasive$hasCS==TRUE], invasive$range_long_r100[invasive$hasCS==FALSE])

# Ok, for both the WRIMS and non-WRIMS set, all comparisons still hold 
# Species with CS have larger ranges in all dimensions than those without


## How about adding a column to the data instead of splitting it?? 
# This might be useful later
data$NI  <- data$scientificName %in% wrims$acceptedNameUsage

ggplot(invasive)+
  aes(x=hasCS, y=range_lat_r100)+
  geom_violin(aes(fill=hasCS))+
  geom_boxplot(width=0.1)+ 
  stat_summary(fun.y=mean, geom="point", size=2, color="red")+
  # scale_fill_manual(values=c("#00000099","#CCCCCC"))+ labs(fill="CS reported",y=NULL,x=NULL)+
  scale_fill_manual(values=c("#00000099","#CCCCCC"))+ 
  labs(fill=NULL,y=NULL,x=NULL)+
  theme(legend.position="none")+
  My_Theme

ggplot(non_invasive2)+
  aes(x=hasCS, y=range_lat_r100)+
  geom_violin(aes(fill=hasCS))+
  geom_boxplot(width=0.1)+ 
  stat_summary(fun.y=mean, geom="point", size=2, color="red")+
  # scale_fill_manual(values=c("#00000099","#CCCCCC"))+ labs(fill="CS reported",y=NULL,x=NULL)+
  scale_fill_manual(values=c("#00000099","#CCCCCC"))+ 
  labs(fill=NULL,y=NULL,x=NULL)+
  theme(legend.position="none")+
  My_Theme

ggplot(invasive)+
  aes(x=hasCS, y=range_long_r100)+
  geom_violin(aes(fill=hasCS))+
  geom_boxplot(width=0.1)+ 
  stat_summary(fun.y=mean, geom="point", size=2, color="red")+
  # scale_fill_manual(values=c("#00000099","#CCCCCC"))+ labs(fill="CS reported",y=NULL,x=NULL)+
  scale_fill_manual(values=c("#00000099","#CCCCCC"))+ 
  labs(fill=NULL,y=NULL,x=NULL)+
  theme(legend.position="none")+
  My_Theme

ggplot(non_invasive2)+
  aes(x=hasCS, y=range_long_r100)+
  geom_violin(aes(fill=hasCS))+
  geom_boxplot(width=0.1)+ 
  stat_summary(fun.y=mean, geom="point", size=2, color="red")+
  # scale_fill_manual(values=c("#00000099","#CCCCCC"))+ labs(fill="CS reported",y=NULL,x=NULL)+
  scale_fill_manual(values=c("#00000099","#CCCCCC"))+ 
  labs(fill=NULL,y=NULL,x=NULL)+
  theme(legend.position="none")+
  My_Theme


# Trying to pull list of species that are both NI and CS

NICS<-filter(invasive,hasCS == "TRUE")
write.csv(NICS,"invasive-and-cryptic.csv")
table(NICS$class)


withgeo<-filter(data, range_lat_r100 != "NA")

withgeo$CSNI<-((withgeo$hasCS==TRUE) & (withgeo$NI == TRUE))


ggplot(withgeo)+
  aes(x=CSNI,y=range_lat_r100)+
  geom_jitter(aes(fill=CSNI))+
  geom_boxplot(width=0.1,alpha=0.5)+ 
  stat_summary(fun.y=mean, geom="point", size=2, color="red")+
  # scale_fill_manual(values=c("#00000099","#CCCCCC"))+ labs(fill="CS reported",y=NULL,x=NULL)+
  #scale_fill_manual(values=c("#00000099","#CCCCCC"))+ 
  labs(fill=NULL,y=NULL,x=NULL)+
  theme(legend.position="none")


# Are CS+NI more likely to be in sympatry? Need to connect NI to the survey

survey <- read.csv(file="978_species_clean_may17.csv"   , header=TRUE ) #Adjusted Nov 15 2023 to fix errors in duplicated species
survey$NI  <- survey$acceptedName_wormsV1 %in% wrims$acceptedNameUsage

