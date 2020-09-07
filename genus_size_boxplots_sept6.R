genus_list<-read.csv("C:/Users/aecsk/OneDrive/Desktop/genussize4.csv",header=TRUE)
cs_genus<-read.table("C:/Users/aecsk/OneDrive/Desktop/cs_genus.txt",header=TRUE)


names=NULL

for (i in genus_list$genus) { 
  cs_present<-(i %in% cs_genus$genus)
  d<-which(genus_list$genus==i)
  size<-genus_list$size[d]
  phylum<-as.character(genus_list$phylum[d])
    
  genus_wcs<-as.data.frame(cbind(i, cs_present, size, phylum))
  
  names<-rbind(names,genus_wcs)
  
 
  
}

write.csv(names,"C:/Users/aecsk/OneDrive/Desktop/genus_size_cs.csv")

try<-gsub(TRUE, "CS", names$cs_present)
try2<-gsub(FALSE, "no_CS", try)

genus_cs<-cbind(names,try2)

ggplot(genus_cs, aes(x=try2, fill=try2, y=as.numeric(size)))+
  geom_boxplot()+
  #geom_jitter(position=position_jitter(w=0.1, h=0.1),size=2, alpha=0.5)+
  theme_bw()+
  xlab("CS present")+
  ylab("Genus size")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  theme(axis.text.x = element_blank())+
  facet_wrap(~ phylum)

genus_cs_noNA<-read.csv("C:/Users/aecsk/OneDrive/Desktop/genus_size_cs_noNA.csv",header=TRUE)

try3<-gsub(TRUE, "CS", genus_cs_noNA$cs_present)
try4<-gsub(FALSE, "no_CS", try3)

genus_cs_noNA<-cbind(genus_cs_noNA,try4)

ggplot(genus_cs_noNA, aes(x=try4, fill=try4, y=as.numeric(size)))+
  geom_boxplot()+
  #geom_jitter(position=position_jitter(w=0.1, h=0.1),size=2, alpha=0.5)+
  theme_bw()+
  xlab("CS present")+
  ylab("Genus size")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  theme(axis.text.x = element_blank())+
  facet_wrap(~ phylum)