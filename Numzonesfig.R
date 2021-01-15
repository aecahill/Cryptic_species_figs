# code to make some histograms for cryptic species paper

library(ggplot2)

numzones<-read.table("C:/Users/aecsk/OneDrive/Desktop/num_zones.txt",header=T)

ggplot(data=numzones, aes(x=Number_zones, y=Proportion, fill=IsInSurvey)) +
  geom_bar(stat="identity", position=position_dodge())+
  xlab("\nNumber of Zones")+ylab("Proportion of Studies\n")+
  theme_bw()+
  theme(axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())