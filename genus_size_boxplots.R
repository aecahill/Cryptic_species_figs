#This is code to make boxplots of genus size for groups with CS and all groups in a phylum

library(ggplot2)

size<-read.table("C:/Users/aecsk/Desktop/genre_size.txt",header=T)

ggplot(size, aes(fill=Type, y=Size,x=Phylum))+
  geom_boxplot()+
  #geom_jitter(position=position_jitter(w=0.1, h=0.1),size=2, alpha=0.5)+
  theme_bw()+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 2.5)+
  geom_vline(xintercept = 3.5)+
  geom_vline(xintercept = 4.5)+
  geom_vline(xintercept = 5.5)+
  geom_vline(xintercept = 6.5)+
  geom_vline(xintercept = 7.5)+
  geom_vline(xintercept = 8.5)+
  geom_vline(xintercept = 9.5)+
  geom_vline(xintercept = 10.5)+
  geom_vline(xintercept = 11.5)+
  geom_vline(xintercept = 12.5)+
  geom_vline(xintercept = 13.5)+
  geom_vline(xintercept = 14.5)+
  geom_vline(xintercept = 15.5)+
  geom_vline(xintercept = 16.5)+
  geom_vline(xintercept = 17.5)+
  geom_vline(xintercept = 18.5)+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  theme(axis.text.x = element_text(angle = 65,hjust=1))

##redoing this but after taking out the genera with no CS

sizenoNA<-read.table("C:/Users/aecsk/Desktop/genre_size_noNA.txt",header=T)

ggplot(sizenoNA, aes(fill=Type, y=Size,x=Phylum))+
  geom_boxplot()+
  #geom_jitter(position=position_jitter(w=0.1, h=0.1),size=1.5, alpha=0.5)+
  theme_bw()+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 2.5)+
  geom_vline(xintercept = 3.5)+
  geom_vline(xintercept = 4.5)+
  geom_vline(xintercept = 5.5)+
  geom_vline(xintercept = 6.5)+
  geom_vline(xintercept = 7.5)+
  geom_vline(xintercept = 8.5)+
  geom_vline(xintercept = 9.5)+
  geom_vline(xintercept = 10.5)+
  geom_vline(xintercept = 11.5)+
  geom_vline(xintercept = 12.5)+
  geom_vline(xintercept = 13.5)+
  geom_vline(xintercept = 14.5)+
  geom_vline(xintercept = 15.5)+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  theme(axis.text.x = element_text(angle = 65,hjust=1))

#same thing but with facets

ggplot(size, aes(fill=Type, y=Size))+
  geom_boxplot()+
  #geom_jitter(position=position_jitter(w=0.1, h=0.1),size=2, alpha=0.5)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  theme(axis.text.x = element_blank())+
  facet_wrap(~ Phylum)

ggplot(sizenoNA, aes(fill=Type, y=Size))+
  geom_boxplot()+
  #geom_jitter(position=position_jitter(w=0.1, h=0.1),size=1.5, alpha=0.5)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  theme(axis.text.x = element_blank())+
  facet_wrap(~Phylum)