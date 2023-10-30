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