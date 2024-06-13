# Note June 13 2024: I am modifying this code from the BioRev paper
# I will remove the WRIMS overlap species from the big dataset to re-test the range size info


## STATISTICS BASED ON THE NOMINAL SPECIES DATASET - Cryptic species meta-analysis - august 2023

library(dplyr) # to manipulate data
library(corrplot) # draws colored correlation tables (not strictly necessary here)
library(bcp)  # to identify change points in a series of data
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
list.files()
data<-read.csv2( "DATASET_187603_NominalSpecies.csv" ,header=T)
data<-as.data.frame(unclass(data),     # Convert all columns to factor (instead of characters, numericals do not change)
                    stringsAsFactors = TRUE)
data$dna<-factor(data$dna, levels=c("No seq","nuc","mito","nuc&mito")) # so the reference level becomes "No seq" for statistics
data$mitonuc <-(data$dna=="nuc&mito")  # boolean used below to build table per decade
data$hasRIOK[is.na(data$RIOK)]<-FALSE     # converts NA values to FALSE
data$phylum_class<-factor(paste0(data$phylum, data$class))


data$area<-cut(data$CHull_r100, breaks=20, labels = 1:20,ordered_result = T)
table(data$area, useNA="always") # range size category: the smaller have more observations (so cuts are equal range sizes)
min(data$CHull_r100, na.rm=T)
max(data$CHull_r100, na.rm=T)

dato<-data[is.na(data$nb_zones)==F,] #99614 observations remaining in file with OBIS data
Npol<-dato[dato$Npol==T,]    # dataframe of species found in North Polar zone
Ntemp<-dato[dato$Ntemp==T,]  # dataframe of species found in North temperate zone
Trop<-dato[dato$Trop==T,]    # dataframe of species found in Tropical zone
Stemp<-dato[dato$Stemp==T,]  # dataframe of species found in South temperate zone
Spol<-dato[dato$Spol==T,]    # dataframe of species found in South Polar zone

dati<-data[data$ncbi==T,] # 38156
datrange<-data[(is.na(data$range_lat_r100)==F)&(is.na(data$range_long_r100)==F),] #13560
databigphyl <-data.frame(filter(data, (phylum=="Arthropoda")|(phylum=="Annelida")|(phylum=="Bryozoa")|(phylum=="Chordata")|(phylum=="Cnidaria")|(phylum=="Echinodermata")|(phylum=="Mollusca")|(phylum=="Nemertea")|(phylum=="Porifera")))
databigphyl$phylum<-factor(databigphyl$phylum, levels=c("Nemertea","Bryozoa","Echinodermata","Porifera","Cnidaria","Annelida","Chordata","Mollusca","Arthropoda")) #reorder by size

## C- Range size comparison with/out CS ####

# sdlat<-as.data.frame(tapply(data$range_lat_r100, data$hasCS,sd, na.rm=T))
# sdlong<-as.data.frame(tapply(data$range_long_r100, data$hasCS,sd, na.rm=T))
# sdCHull<-as.data.frame(tapply(data$CHull_r100, data$hasCS,sd, na.rm=T))

meanlat<-as.data.frame(tapply(data$range_lat_r100, data$hasCS,mean, na.rm=T))
meanlong<-as.data.frame(tapply(data$range_long_r100, data$hasCS,mean, na.rm=T))
meanCHull<-as.data.frame(tapply(data$CHull_r100, data$hasCS,mean, na.rm=T))
meanlat[2,1]/meanlat[1,1]       # 1.253349 (OLD 1.224714 (before rarefaction, it was 2.571744 probably strongly biased by effort!)
meanlong[2,1]/meanlong[1,1]     # 1.343295 (OLD 1.324631 (before rarefaction, it was 2.768405 probably strongly biased by effort!)
meanCHull[2,1]/meanCHull[1,1]   # 1.623933 (OLD 1.579785)

### Violin plots for range sizes with/out CS (Figure 2 main manuscript) 
My_Theme = theme(
  # axis.title.x = element_text(size = 16),
  axis.text.y = element_text(size = 14),
  legend.text=element_text(size=12),
  legend.title=element_text(size=16))

a<-ggplot(data)+
  aes(x=hasCS, y=range_lat_r100)+
  geom_violin(aes(fill=hasCS))+
  geom_boxplot(width=0.1)+ 
  stat_summary(fun.y=mean, geom="point", size=2, color="red")+
  # scale_fill_manual(values=c("#00000099","#CCCCCC"))+ labs(fill="CS reported",y=NULL,x=NULL)+
  scale_fill_manual(values=c("#00000099","#CCCCCC"))+ 
  labs(fill=NULL,y=NULL,x=NULL)+
  theme(legend.position="none")+
  My_Theme
b<-ggplot(data)+aes(x=hasCS, y=range_long_r100)+geom_violin(aes(fill=hasCS))+
  geom_boxplot(width=0.1)+ stat_summary(fun.y=mean, geom="point", size=2, color="red")+
  scale_fill_manual(values=c("#00000099","#CCCCCC"))+ labs(fill="CS reported",y=NULL,x=NULL)+
  # scale_x_discrete(labels = NULL)+
  theme(legend.position="none")+
  My_Theme
c<-ggplot(data)+aes(x=hasCS, y=CHull_r100)+geom_violin(aes(fill=hasCS))+ 
  geom_boxplot(width=0.1)+ stat_summary(fun.y=mean, geom="point", size=2, color="red")+
  scale_fill_manual(values=c("#00000099","#CCCCCC"))+ labs(fill="CS reported",y=NULL,x=NULL)+
  # scale_x_discrete(labels = NULL)+
  theme(legend.position="none")+
  My_Theme
g<-ggarrange(a,b,c, ncol=3,nrow=1, 
             # common.legend=TRUE, 
             # legend="top",
             labels=c("A","B","C"),
             font.label=list(size=18,color="black"))
g


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
