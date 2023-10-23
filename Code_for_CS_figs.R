## Code for figures in Cahill et al. paper 

library(ggplot2)
library(cowplot)
library(dplyr)
library(ggtree)
library(forcats)

# Figure 2: Boxplots through time
yearsdata<-read.table("C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs/years.txt",header=T)
yearratios<-read.table("C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs/year_ratios2.txt",header=T)
nomspp<-read.csv("C:/Users/aecsk/Desktop/nomspp.csv",header=TRUE,sep=";")

#Panel 2a

years<-ggplot() +
  geom_bar(data=yearsdata, aes(x=Year, y=Proportion, fill=has_CS),stat="identity", colour=c("black"), position=position_dodge())+
  scale_fill_manual(values=c("#CCCCCC", "#00000099"))+
  geom_point(data=yearratios,aes(x=Year, y=Ratio),shape = 21, colour = "#FF3300", fill = "#FF3300", size = 1, stroke = 2)+
  xlab("\nYear of Description")+ylab("Proportion of Species\n")+
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


# Panel 2b

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
  ggplot(aes(x=phylum, y=yearb,fill=has_CS)) +
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

plot_grid(years,year_phylum_reduced2,labels=c("A","B"),ncol=2)


## Figure 4 -- Number of zones

numzones<-read.table("C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs/num_zones.txt",header=T)

ggplot(data=numzones, aes(x=Number_zones, y=Proportion, fill=has_CS)) +
  geom_bar(stat="identity", colour=c("black"), position=position_dodge())+
  scale_fill_manual(values=c("#CCCCCC","black"))+
  xlab("\nNumber of Zones")+ylab("Proportion of Species\n")+
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

## Figure 5 -- Phylogenetic tree and heatmap

tree57data<-read.tree("C:/Users/acahill/Documents/GitHub/Cryptic_species_figs/57_tree.tre")
traits57<-read.table("C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs/traits_57_aug2023.txt",header=T)


tree57<-ggtree(tree57data)

supp.labs <- c("Fertilisation Mode", "Hard skeletal elements","Image-forming eyes","Residuals")

names(supp.labs) <- c("Fertilization", "Hard_skeleton","Image","Residuals")

heatmap<-ggplot(data = traits57, mapping = aes(x = Variable,
                                               y = Class_num,
                                               fill = z_score)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red')+
  theme_bw()+
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_blank(),axis.text.y=element_blank())+
  geom_hline(yintercept=4.5)+
  geom_hline(yintercept=9.5)+
  geom_hline(yintercept=16.5)+
  geom_hline(yintercept=17.5)+
  geom_hline(yintercept=18.5)+
  geom_hline(yintercept=23.5)+
  geom_hline(yintercept=24.5)+
  geom_hline(yintercept=25.5)+
  geom_hline(yintercept=27.5)+
  geom_hline(yintercept=28.5)+
  geom_hline(yintercept=31.5)+
  geom_hline(yintercept=39.5)+
  geom_hline(yintercept=41.5)+
  geom_hline(yintercept=43.5)+
  facet_grid(~Variable,labeller = labeller(Variable=supp.labs),switch = "x", scales = "free_x", space = "free_x")
