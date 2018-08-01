##graphs for CS poster

library(ggplot2)
library(cowplot)
library(reshape2)
library(wesanderson)

#what phyla are the CS in?

cs_species<-read.table("C:/Users/acahill/Desktop/cs_species.txt",header=T)

#reorder factors

cs_species$Phylum <- factor(cs_species$Phylum, levels = cs_species$Phylum[order(-cs_species$total)])
cs_species$Phylum  # notice the changed order of factor levels

a<-ggplot(cs_species, aes(x = Phylum, y = total, fill=Phylum)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Total number of studies\n")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position="none")+
  scale_fill_hue(c=100, l=45)

#corrected for the percentage of species in worms

#cs_correct<-read.table("C:/Users/Abigail/Desktop/cs_percentages.txt",header=T)

#cs_correct$Phylum <- factor(cs_correct$Phylum, levels = cs_correct$Phylum[order(-cs_correct$Percent_CS)])
#cs_correct$Phylum  # notice the changed order of factor levels

#ggplot(cs_correct, aes(x = Phylum, y = Percent_CS, fill=Phylum)) + 
  #theme_bw() + 
  #theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  #geom_bar(stat = "identity")+
  #ylab("Percent CS\n")+
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14))+
  #theme(axis.title.x = element_text(size = 16))+
  #theme(axis.title.y = element_text(size = 16))+
  #theme(axis.text.y = element_text(size = 14))+
  #theme(legend.position="none")+
  #scale_fill_hue(c=100, l=45)


#corrected for expected values

cs_correct<-read.table("C:/Users/acahill/Desktop/CS_phylum_corrected.txt",header=T)

cs_correct$Phylum <- factor(cs_correct$Phylum, levels = cs_correct$Phylum[order(-cs_correct$Difference)])
cs_correct$Phylum  # notice the changed order of factor levels

b<-ggplot(cs_correct, aes(x = Phylum, y = Difference, fill=Phylum)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Difference from Expected\n")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position="none")+
  scale_fill_hue(c=100, l=45)

plot_grid(a,b,labels=c("a","b"))


#what habitat are the CS in?

cs_habitat<-read.table("C:/Users/acahill/Desktop/cs_habitat.txt",header=T)

ggplot(cs_habitat, aes(x = Habitat, y = Total, fill=Habitat)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Total number of studies\n")+
  xlab("\nHabitat")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position="none")+
  scale_fill_hue(c=100, l=45)

#do the CS have morphological differences?

morphodiff<-read.table("C:/Users/Abigail/Desktop/morphodiff.txt",header=T)

#reorder factors

morphodiff$Difference <- factor(morphodiff$Difference, levels = morphodiff$Difference[order(-morphodiff$Number)])
morphodiff$Difference   # notice the changed order of factor levels


ggplot(morphodiff, aes(x = Difference, y = Number, fill=Difference)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Total number of studies\n")+
  xlab("\nMorphological differentiation?")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position="none")+
  scale_fill_hue(c=100, l=45)

#in molluscs, what are the larval types of CS?

moll_larvae<-read.table("C:/Users/acahill/Desktop/cs_larvae.txt",header=F)

#reorder factors

moll_larvae$V2 <- factor(moll_larvae$V2, levels = moll_larvae$V2[order(-moll_larvae$V1)])
moll_larvae$V2   # notice the changed order of factor levels


ggplot(moll_larvae, aes(x = V2, y = V1, fill=V2)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Total number of species\n")+
  xlab("\nLarval Type")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position="none")+
  scale_fill_hue(c=100, l=45)


#in molluscs, what is the geography of CS?

moll_geo<-read.table("C:/Users/acahill/Desktop/mollusc_geo.txt",header=F)

#reorder factors

moll_geo$V2 <- factor(moll_geo$V2, levels = moll_geo$V2[order(-moll_geo$V1)])
moll_geo$V2   # notice the changed order of factor levels


ggplot(moll_geo, aes(x = V2, y = V1, fill=V2)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Total number of species\n")+
  xlab("\nGeographic Distribution")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position="none")+
  scale_fill_hue(c=100, l=45)


#marker type overall

markers<-read.table("C:/Users/acahill/Desktop/markers.txt",header=F)

#reorder factors

markers$V1 <- factor(markers$V1, levels = markers$V1[order(-markers$V2)])
markers$V1   # notice the changed order of factor levels


c<-ggplot(markers, aes(x = V1, y = V2, fill=V1)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Number of studies\n")+
  xlab("\nData used")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 14))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position="none")+
  scale_fill_hue(c=100, l=45)


#non genetic differences

nongen<-read.table("C:/Users/acahill/Desktop/nongendiffs.txt",header=F)

#reorder factors

nongen$V1 <- factor(nongen$V1, levels = nongen$V1[order(-nongen$V2)])
nongen$V1   # notice the changed order of factor levels


d<-ggplot(nongen, aes(x = V1, y = V2, fill=V1)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Number of studies\n")+
  xlab("\nData used")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 14))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position="none")+
  scale_fill_hue(c=100, l=45)

plot_grid(c,d,labels=c("a","b"))

##graphing change in markertype through time

throughtime<-read.table("C:/users/acahill/Desktop/throughtime.txt",header=T)

#change dataformat to group by year

byyear<-melt(throughtime,id.vars=c("Year"))

colnames(byyear)<-c("Year","Marker","value")

ggplot(byyear, aes(x = Year, y = value, fill=Marker)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = wes_palette("Zissou1",5,type="continuous"))+
  ylab("Number of studies\n")+
  xlab("\nYear")
  #theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 14))+
  #theme(axis.title.x = element_text(size = 16))+
  #theme(axis.title.y = element_text(size = 16))+
  #theme(axis.text.y = element_text(size = 14))+
  #theme(legend.position="none")+
  