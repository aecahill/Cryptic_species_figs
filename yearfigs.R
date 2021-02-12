# code for figures involving year of description

library(ggplot2)

years<-read.table("C:/Users/aecsk/OneDrive/Desktop/years.txt",header=T)

ggplot(data=years, aes(x=Year, y=Proportion, fill=IsInSurvey)) +
  geom_bar(stat="identity", colour=c("black"), position=position_dodge())+
  scale_fill_manual(values=c("#CCCCCC", "black"))+
  xlab("\nYear of Description")+ylab("Proportion of Species\n")+
  annotate("segment", x = 1974, xend = 1974, y = 0.149, yend = 0.13, colour = "#cccccc", size=2,  arrow=arrow())+
  annotate("segment", x = 1888, xend = 1888, y = 0.149, yend = 0.13, colour = "black", size=2,  arrow=arrow())+
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

#plot looking at correlation btwn number of zones and year

zonesandyear<-na.omit(read.table("C:/Users/aecsk/OneDrive/Desktop/zonesandyear.txt",header=T))

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
  