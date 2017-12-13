##graphs for CS poster

cs_species<-read.table("C:/Users/acahill/Desktop/cs_species.txt",header=T)

#reorder factors

cs_species$Phylum <- factor(cs_species$Phylum, levels = cs_species$Phylum[order(-cs_species$total)])
cs_species$Phylum  # notice the changed order of factor levels

ggplot(cs_species, aes(x = Phylum, y = total, fill=Phylum)) + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_bar(stat = "identity")+
  ylab("Total number of studies\n")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.text.y = element_text(size = 14))+
  theme(legend.position="none")+
  scale_fill_hue(c=100, l=45)


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

morphodiff<-read.table("C:/Users/acahill/Desktop/morphodiff.txt",header=T)

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
