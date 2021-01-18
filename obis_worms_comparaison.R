#code to compare OBIS and WORMS across phyla

library(ggplot2)

phyladb<-read.table("C:/Users/aecsk/OneDrive/Desktop/phyla_databases.txt",header=T)

#this code reorders the data
phyladb$Phylum<-factor(phyladb$Phylum,levels=c("1_Arthropoda","2_Mollusca","3_Chordata","4_Annelida","5_Cnidaria","6_Porifera","7_Echinodermata","8_Bryozoa","9_Nematoda","10_Platyhelminthes","11_Nemertea","12_Gastrotricha","13_Xenacoelomorpha","14_Brachiopoda","15_Tardigrada","16_Ctenophora","17_Entoprocta","18_Kinorhyncha","19_Sipuncula","20_Rotifera","21_Chaetognatha","22_Hemichordata","23_Dicyemida","24_Gnathostomulida","25_Loricifera","26_Orthonectida","27_Priapulida","28_Phoronida","29_Nematomorpha","30_Placozoa","31_Cycliophora"))


ggplot(data=phyladb, aes(x=Phylum, y=Proportion, fill=Database)) +
  geom_bar(stat="identity", colour=c("black"), position=position_dodge())+
  scale_fill_manual(values=c("black","#CCCCCC"))+
  xlab("\nPhylum")+ylab("Proportion of Species\n")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())