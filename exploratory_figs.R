library(ggplot2)
library(RColorBrewer)

ggplot(csdata,aes(One_mito,Num_CS))+
  geom_smooth(method='lm',se=F)+
  geom_point(aes(color=Phylum), size = 4)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  #scale_colour_manual(values=c("green","darkorange2","gold","black","purple","red"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  ylim(0,120)+
  xlim(0,10000)+
  xlab("\nNumber with one mito seq")+ylab("Number CS\n")

ggplot(csdata,aes(One_mito,Num_CS))+
  geom_smooth(method='lm',se=F)+
  geom_point(aes(color=Repro_mode), size = 4)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  scale_colour_manual(values=c("green","blue","black","red"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  ylim(0,120)+
  xlim(0,10000)+
  xlab("\nNumber with one mito seq")+ylab("Number CS\n")

ggplot(csdata,aes(One_mito,Num_CS))+
  geom_smooth(method='lm',se=F)+
  geom_point(aes(color=Genitalia), size = 4)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  scale_colour_manual(values=c("green","blue","black","red"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  ylim(0,120)+
  xlim(0,10000)+
  xlab("\nNumber with one mito seq")+ylab("Number CS\n")


ggplot(csdata,aes(One_mito,Num_CS))+
  geom_smooth(method='lm',se=F)+
  geom_point(aes(color=Class_size), size = 4)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  scale_color_gradient(low="blue", high="red")+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  ylim(0,120)+
  xlim(0,10000)+
  xlab("\nNumber with one mito seq")+ylab("Number CS\n")