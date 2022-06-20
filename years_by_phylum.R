#code for fig to look at year of description across phyla

library(ggplot2)

yearphyla<-na.omit(read.table("C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs/year_all_spp.txt",header=T))

year_phylum<-ggplot(data=yearphyla, aes(x=Phylum, y=year,fill=CS)) +
  #geom_point(alpha=0.1,cex=0.1)+
  #geom_jitter(alpha=0.1,cex=0.1,width=0.1)+
  geom_boxplot(alpha=0.6, width=0.65)+
  scale_fill_manual(values=c("black","#CCCCCC"))+
  coord_flip()+
  ylab("\nYear of Description")+xlab("Phylum\n")+
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

year_annelid<-yearphyla[yearphyla$Phylum=="Annelida",]
year_arthropod<-yearphyla[yearphyla$Phylum=="Arthropoda",]
year_bryozoa<-yearphyla[yearphyla$Phylum=="Bryozoa",]
year_chordata<-yearphyla[yearphyla$Phylum=="Chordata",]
year_cnidaria<-yearphyla[yearphyla$Phylum=="Cnidaria",]
year_echinoderm<-yearphyla[yearphyla$Phylum=="Echinodermata",]
year_mollusca<-yearphyla[yearphyla$Phylum=="Mollusca",]
year_nematode<-yearphyla[yearphyla$Phylum=="Nematoda",]
year_nemertea<-yearphyla[yearphyla$Phylum=="Nemertea",]
year_platys<-yearphyla[yearphyla$Phylum=="Platyhelminthes",]
year_porifera<-yearphyla[yearphyla$Phylum=="Porifera",]

year_reduced<-rbind(year_annelid,year_arthropod,year_bryozoa,year_chordata,year_cnidaria,year_echinoderm,year_mollusca,year_nematode,year_nemertea,year_platys,year_porifera)
colnames(year_reduced)<-c("Species","Phylum","hasCS","year")

year_phylum_reduced<-ggplot(data=year_reduced, aes(x=Phylum, y=year,fill=hasCS)) +
  #geom_point(alpha=0.1,cex=0.1)+
  #geom_jitter(alpha=0.1,cex=0.1,width=0.1)+
  geom_boxplot(alpha=0.6, width=0.65)+
  scale_fill_manual(values=c("#CCCCCC","black"))+
  coord_flip()+
  ylab("\nYear of Description")+xlab("Phylum\n")+
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

#June 20, code based on latest files from Anne to make reduced boxplot

nomspp<-read.csv("C:/Users/aecsk/Desktop/nomspp.csv",header=TRUE,sep=";")
has_CS<-gsub("FALSE","No",nomspp$hasCS)
has_CS<-gsub("TRUE","Yes",has_CS)
nomspp<-cbind(nomspp,has_CS)

year_annelid2<-nomspp[nomspp$phylum=="Annelida",]
year_arthropod2<-nomspp[nomspp$phylum=="Arthropoda",]
year_bryozoa2<-nomspp[nomspp$phylum=="Bryozoa",]
year_chordata2<-nomspp[nomspp$phylum=="Chordata",]
year_cnidaria2<-nomspp[nomspp$phylum=="Cnidaria",]
year_echinoderm2<-nomspp[nomspp$phylum=="Echinodermata",]
year_mollusca2<-nomspp[nomspp$phylum=="Mollusca",]
year_nematode2<-nomspp[nomspp$phylum=="Nematoda",]
year_nemertea2<-nomspp[nomspp$phylum=="Nemertea",]
year_platys2<-nomspp[nomspp$phylum=="Platyhelminthes",]
year_porifera2<-nomspp[nomspp$phylum=="Porifera",]

year_reduced2<-rbind(year_annelid2,year_arthropod2,year_bryozoa2,year_chordata2,year_cnidaria2,year_echinoderm2,year_mollusca2,year_nematode2,year_nemertea2,year_platys2,year_porifera2)

year_phylum_reduced2<-ggplot(data=year_reduced2, aes(x=phylum, y=yearb,fill=has_CS)) +
  #geom_point(alpha=0.1,cex=0.1)+
  #geom_jitter(alpha=0.1,cex=0.1,width=0.1)+
  geom_boxplot(alpha=0.6, width=0.65)+
  scale_fill_manual(values=c("#CCCCCC","black"))+
  coord_flip()+
  ylab("\nYear of Description")+xlab("Phylum\n")+
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

