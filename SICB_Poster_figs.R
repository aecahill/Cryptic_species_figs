##graphs for CS poster

library(ggplot2)
library(cowplot)
library(reshape2)
library(wesanderson)

#what phyla are the CS in?

#cs_species<-read.table("C:/Users/acahill/Desktop/cs_species.txt",header=T)
cs_correct<-read.table("C:/Users/aecsk/Desktop/cs_phylum_corrected_nodups2.txt",header=T)

#reorder factors

cs_correct$Phylum <- factor(cs_correct$Phylum, levels = cs_correct$Phylum[order(-cs_correct$total_CS)])
cs_correct$Phylum  # notice the changed order of factor levels

#colorscale<-c("#FF0033","#FF3366","#FF6633","#FFCC00","#FFF333","#99FF33","#66FF66","black","#33FF33","#339966","#66CC99","#009999","#003333","#003366","#0000FF","#330099","#330066","#660066","#330033","#FF00CC","#990066")
colorscale<-rainbow(21)


cs_wcolor<-cbind(cs_correct,colorscale)

a<-ggplot(cs_wcolor, aes(x = Phylum, y = total_CS, fill=colorscale)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Number of nominal species\n")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position="none")+
  scale_fill_manual(values=colorscale)

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

#cs_correct<-read.table("C:/Users/acahill/Desktop/CS_phylum_corrected.txt",header=T)

cs_correct$Phylum <- factor(cs_correct$Phylum, levels = cs_correct$Phylum[order(-cs_correct$Difference)])
cs_correct$Phylum  # notice the changed order of factor levels

b<-ggplot(cs_correct, aes(x = Phylum, y = Difference, fill=colorscale)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Difference from Expected\n")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position="none")+
  scale_fill_manual(values=colorscale)

plot_grid(a,b,labels=c("a","b"))


#what habitat are the CS in?

cs_habitat<-read.table("C:/Users/acahill/Desktop/cs_habitat.txt",header=T)

ggplot(cs_habitat, aes(x = Habitat, y = Total)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Total number of studies\n")+
  xlab("\nHabitat")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position="none")
  #scale_fill_hue(c=100, l=45)

#graphing habitat fragmentation

cs_frag<-read.table("C:/Users/Abigail/Desktop/hab_frag.txt",header=T)

ggplot(cs_frag, aes(x = Code, y = Number_CS)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Total number of studies\n")+
  xlab("\nDegree of Fragmentation")+
  #scale_x_discrete(labels=c("large pelagic","small pelagic", "abyssal","coastal","estuary","coral reef","caves"))
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


ggplot(morphodiff, aes(x = Difference, y = Number)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Total number of studies\n")+
  xlab("\nMorphological differentiation?")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position="none")
  #scale_fill_hue(c=100, l=45)

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

markers<-read.table("C:/Users/aecsk/Desktop/markers.txt",header=F)

#reorder factors

markers$V1 <- factor(markers$V1, levels = markers$V1[order(-markers$V2)])
markers$V1   # notice the changed order of factor levels


c<-ggplot(markers, aes(x = V1, y = V2)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Number of studies\n")+
  xlab("\nData used")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 14))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position="none")
  #scale_fill_hue(c=100, l=45)


#non genetic differences

nongen<-read.table("C:/Users/aecsk/Desktop/nongendiffs.txt",header=F)

#reorder factors

nongen$V1 <- factor(nongen$V1, levels = nongen$V1[order(-nongen$V2)])
nongen$V1   # notice the changed order of factor levels


d<-ggplot(nongen, aes(x = V1, y = V2)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Number of studies\n")+
  xlab("\nData used")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 14))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position="none")
  #scale_fill_hue(c=100, l=45)

plot_grid(c,d,labels=c("a","b"))

##graphing change in markertype through time

throughtime<-read.table("C:/Users/aecsk/Desktop/throughtime.txt",header=T)

#change dataformat to group by year

byyear<-melt(throughtime,id.vars=c("Year"))

colnames(byyear)<-c("Year","Marker","value")

#timecolors<-c("#3399FF","#FF3300","#000099","#FFFF00","#FF0000","#330066","#FF00CC","#990066")
timecolors<-rainbow(5)

ggplot(byyear, aes(x = Year, y = value, fill=Marker)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  #scale_fill_manual(values = wes_palette("Zissou1",5,type="continuous"))+
  scale_fill_manual(values=timecolors)+
  ylab("Number of studies\n")+
  xlab("\nYear")
  #theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 14))+
  #theme(axis.title.x = element_text(size = 16))+
  #theme(axis.title.y = element_text(size = 16))+
  #theme(axis.text.y = element_text(size = 14))+
  #theme(legend.position="none")+


#devtype figure

devtype<-read.table("C:/Users/aecsk/Desktop/devtype.txt",header=T)

molluscs<-devtype[1:3,]
echinos<-devtype[4:6,]
annelid<-devtype[7:9,]

mol<-ggplot(molluscs, aes(x = Larval_type, y = Difference)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), axis.title.x = element_blank())+
  geom_bar(stat = "identity")+
  ylab("Difference\n")
  #xlab("\nYear")

ech<-ggplot(echinos, aes(x = Larval_type, y = Difference)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), axis.title.x = element_blank())+
  geom_bar(stat = "identity")+
  ylab("Difference\n")

ann<-ggplot(annelid, aes(x = Larval_type, y = Difference)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Difference\n")+
  xlab("\nLarval Type")

plot_grid(mol,ech,ann,labels=c("a","b","c"),ncol=1)

#graphing eco differences

ecodiffs<-read.table("C:/Users/aecsk/Desktop/ecodiff.txt",header=T)

ecoplot<-ggplot(ecodiffs, aes(x = Sympatric, y = Number, fill=Eco_Difference)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),axis.title.x = element_blank(),axis.text.x=element_blank())+
  geom_bar(stat = "identity",position="dodge")+
  ylab("Number of studies\n")+
  ylim(0,62)+
  #xlab("\nAre CS sympatric?")+
  #theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 14))+
  #theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 12))+
  geom_text(aes(label = Number, group = Eco_Difference), position = position_dodge(0.8),
    vjust = -0.3, size = 3.5)+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.text=element_text(size=10))+
  scale_fill_manual(values=c("black","dark grey"))

#graphing diagnostic morpho differences in allo vs sympatry

morphodiffs<-read.table("C:/Users/aecsk/Desktop/diagmorphodiff.txt",header=T)

morphoplot<-ggplot(morphodiffs, aes(x = Sympatric, y = Number, fill=Morpho_Difference)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity",position="dodge")+
  ylab("Number of studies\n")+
  xlab("\nAre CS sympatric?")+
  ylim(0,165)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 14))+
  theme(axis.title.x = element_text(size = 12))+
  geom_text(aes(label = Number, group = Morpho_Difference), position = position_dodge(0.8),
            vjust = -0.3, size = 3.5)+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 14))+
  scale_fill_manual(values=c("black","dark grey"))

plot_grid(ecoplot,morphoplot,labels=c("a","b"),ncol=1)
