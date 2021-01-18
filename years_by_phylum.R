#code for fig to look at year of description across phyla

library(ggplot2)

yearphyla<-na.omit(read.table("C:/Users/aecsk/OneDrive/Desktop/year_all_spp.txt",header=T))

ggplot(data=yearphyla, aes(x=Phylum, y=year,fill=isInSurvey)) +
  #geom_point(alpha=0.1,cex=0.1)+
  #geom_jitter(alpha=0.1,cex=0.1,width=0.1)+
  geom_boxplot(alpha=0.6)+
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
