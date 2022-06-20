# code for figures involving year of description

library(ggplot2)
library(cowplot)

yearsdata<-read.table("C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs/years.txt",header=T)

#adding ratios to plot
yearratios<-read.table("C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs/year_ratios2.txt",header=T)

#With year ratios, where ratio is of the NUMBERS of species in CS over NS
years<-ggplot() +
  geom_bar(data=yearsdata, aes(x=Year, y=Proportion, fill=hasCS),stat="identity", colour=c("black"), position=position_dodge())+
  scale_fill_manual(values=c("#CCCCCC", "black"))+
  geom_point(data=yearratios,aes(x=Year, y=Ratio),shape = 21, colour = "blue", fill = "white", size = 1, stroke = 2)+
  xlab("\nYear of Description")+ylab("Proportion of Species\n")+
  annotate("segment", x = 1974, xend = 1974, y = 0.149, yend = 0.13, colour = "#cccccc", size=2,  arrow=arrow())+
  annotate("segment", x = 1888, xend = 1888, y = 0.149, yend = 0.13, colour = "black", size=2,  arrow=arrow())+
  annotate("segment", x = 1942, xend = 1942, y = 0.149, yend = 0.13, colour = "#cccccc", linetype=2, size=1,  arrow=arrow())+
  annotate("segment", x = 1859, xend = 1859, y = 0.149, yend = 0.13, colour = "black", linetype=2, size=1,  arrow=arrow())+
  theme_bw()+
   theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


#With year ratios, where ratio is of the PROPORTIONS of species in CS over NS
ggplot() +
  geom_bar(data=years, aes(x=Year, y=Proportion, fill=IsInSurvey),stat="identity", colour=c("black"), position=position_dodge())+
  scale_fill_manual(values=c("#CCCCCC", "black"))+
  geom_point(data=yearratios,aes(x=Year, y=Ratio_proportion),shape = 21, colour = "blue", fill = "blue", size = 1, stroke = 1)+
  xlab("\nYear of Description")+ylab("Proportion of Species\n")+
  #annotate("segment", x = 1974, xend = 1974, y = 0.149, yend = 0.13, colour = "#cccccc", size=2,  arrow=arrow())+
  #annotate("segment", x = 1888, xend = 1888, y = 0.149, yend = 0.13, colour = "black", size=2,  arrow=arrow())+
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


#plot looking at correlation btwn number of zones and year

zonesandyear<-na.omit(read.table("C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs/zonesandyear.txt",header=T))

ggplot(data=zonesandyear, aes(x=as.character(nb_zones), y=year)) +
  #geom_point(alpha=0.1,cex=0.1)+
  geom_jitter(alpha=0.1,cex=0.1,width=0.1)+
  geom_boxplot(alpha=0.6)+
  coord_flip()+
  ylab("\nYear of Description")+xlab("Number of Zones\n")+
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

yearphyla<-na.omit(read.table("C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs/year_all_spp.txt",header=T))

year_phylum<-ggplot(data=yearphyla, aes(x=Phylum, y=year,fill=CS)) +
  #geom_point(alpha=0.1,cex=0.1)+
  #geom_jitter(alpha=0.1,cex=0.1,width=0.1)+
  geom_boxplot(alpha=0.6, width=0.65)+
  scale_fill_manual(values=c("#CCCCCC","black"))+
  coord_flip()+
  ylab("\nYear of Description")+xlab("Phylum\n")+
  theme_bw()+
  theme(legend.position="none")+
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

year_phylum_reduced2<-year_reduced2 %>%
  mutate(phylum = fct_relevel(phylum, "Nemertea","Platyhelminthes","Nematoda","Bryozoa","Echinodermata","Porifera","Cnidaria","Annelida","Chordata","Mollusca","Arthropoda")) %>%
  ggplot( aes(x=phylum, y=yearb,fill=has_CS)) +
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


years<-ggplot() +
  geom_bar(data=yearsdata, aes(x=Year, y=Proportion, fill=has_CS),stat="identity", colour=c("black"), position=position_dodge())+
  scale_fill_manual(values=c("#CCCCCC", "black"))+
  geom_point(data=yearratios,aes(x=Year, y=Ratio),shape = 21, colour = "#FF3300", fill = "#FF3300", size = 1, stroke = 2)+
  xlab("\nYear of Description")+ylab("Proportion of Species\n")+
  #annotate("segment", x = 1974, xend = 1974, y = 0.129, yend = 0.11, colour = "#cccccc", size=2,  arrow=arrow())+
  #annotate("segment", x = 1888, xend = 1888, y = 0.129, yend = 0.11, colour = "black", size=2,  arrow=arrow())+
  #annotate("segment", x = 1942, xend = 1942, y = 0.129, yend = 0.11, colour = "#cccccc", linetype=2, size=1,  arrow=arrow())+
  #annotate("segment", x = 1859, xend = 1859, y = 0.129, yend = 0.11, colour = "black", linetype=2, size=1,  arrow=arrow())+
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


plot_grid(years,year_phylum_reduced2,labels=c("A","B"),ncol=2)