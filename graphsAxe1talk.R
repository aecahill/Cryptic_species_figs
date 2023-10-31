# Graph of NCBI data for presentation

percents<-c(0.44,2.06,2.77)
cats<-c("All species","NCBI","Nuclear and mtDNA")
ncbidat<-as.data.frame(cbind(percents,cats))

ggplot(ncbidat,aes(x=cats,y=as.numeric(percents)))+geom_bar(stat="identity")+
  labs(x ="Data subset", y = "Percent with CS")+
  theme_bw()

zoneCS<-c(105,	588,	560,	405,	38)
zonenames<-c("1Npol","2Ntemp","3Trop","4Stemp","5Spol")
zoneNS<-c(2045,	19778,	21187,	11917,	1063)
zonedat<-as.data.frame(cbind(zonespp,zonenames,zoneNS))
zonepercent<-as.numeric(zonedat[,1])/as.numeric(zonedat[,3])
zonedat<-cbind(zonedat,zonepercent)

zonenums<-ggplot(zonedat,aes(x=zonenames,y=as.numeric(zonespp)))+geom_bar(stat="identity")+
  labs(x ="Zones", y = "Number NS containing CS")+
  theme_bw()
zoneper<-ggplot(zonedat,aes(x=zonenames,y=as.numeric(zonepercent)))+geom_bar(stat="identity")+
  labs(x ="Zones", y = "Percent NS containing CS")+
  theme_bw()

plot_grid(zonenums,zoneper,ncol=1)

#Expected and observed per ocean
#Chisq with those numbers
expected<-c(61.85,451.21,383,668,67.46)
observed<-c(130,453,430,562,59)
oceans<-(rbind(expected,observed))
offexp<-oceans[2,]-oceans[1,]
oceans<-t(rbind(oceans,offexp))
oceans<-as.data.frame(cbind(oceans,c("Arctic","Atlantic","Indian","Pacific","Southern")))
expectedpercent<-as.numeric(oceans[,1])/sum(expected)
obspercent<-as.numeric(oceans[,2])/sum(observed)
diffpercent<-obspercent-expectedpercent
oceans<-cbind(oceans,expectedpercent,obspercent,diffpercent)
colnames(oceans)<-c("expected","observed","difference","Ocean","expectedpercent","observedpercent","diffpercent")

oceancolors<-c("#58cc00","#cc3300","#cccc00","#00c5cc","#b400cc")

ggplot(oceans,aes(x=Ocean,y=as.numeric(diffpercent),fill=Ocean))+geom_bar(stat="identity")+
  scale_fill_manual(values=oceancolors)+
  labs(x ="Oceans", y = "Difference between Percent Expected and Observed")+
  theme_bw()


notfound<-read.table("not_yet_found.txt",header=T)
ggplot(notfound,aes(x=Ocean,y=as.numeric(Number),fill=Ocean))+geom_bar(stat="identity")+
  scale_fill_manual(values=oceancolors)+
  #labs(x ="Oceans", y = "Difference between Percent Expected and Observed")+
  theme_bw()+
  facet_wrap(~Variable,scales="free_y")

notfoundphyla<-read.table("not_yet_found_phyla.txt",header=T)
ggplot(notfoundphyla,aes(x=Phylum,y=as.numeric(Number),fill=Phylum))+geom_bar(stat="identity")+
  #scale_fill_manual(values=oceancolors)+
  #labs(x ="Oceans", y = "Difference between Percent Expected and Observed")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust=1))+
  facet_wrap(~Variable,scales="free_y")

