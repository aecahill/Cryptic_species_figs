library(ggplot2)
library(cowplot)

#New figures May 2026, for CS2 - Biol Rev

#Load data
survey <- read.csv(file="978_species_clean_may17.csv" , header=TRUE ) 
survey <-as.data.frame(unclass(survey),stringsAsFactors=TRUE)
surveymorpho<-filter(survey, Morpho_diff != "NA") 

#Histogram to compare number of individuals for genetics and morphology
surveymorpho$logNig<-log10(surveymorpho$Nig)
surveymorpho$logNim<-log10(surveymorpho$Nim)

histgenlog<-ggplot(surveymorpho, aes(x=logNig)) + 
  geom_histogram(binwidth = 0.1)+
  xlim(-0.1,4)+
  ylim(0,50)+
  geom_segment(aes(x = 1.731, y = 49, xend = 1.731, yend = 0),
               linetype = "dashed" ,color="darkgrey",size=0.75)+
  xlab("Log (Individuals Used for Genetics)")+
  ylab("Number of Species")+
  theme_bw()+
  theme(panel.grid.minor=element_blank())


histmorphlog<-ggplot(surveymorpho, aes(x=logNim)) + 
  geom_histogram(binwidth = 0.1)+
  xlim(-0.1,4)+
  ylim(0,50)+
  geom_segment(aes(x = 1.734, y = 49, xend = 1.734, yend = 0),
                           linetype = "dashed" ,color="darkgrey",size=0.75)+
  xlab("Log (Individuals Used for Morphology)")+
  ylab("Number of Species")+
  theme_bw()+
  theme(panel.grid.minor=element_blank())

plot_grid(histgenlog,histmorphlog,labels=c("A","B"),nrow=2)

#trying with log scale 

histgen<-ggplot(surveymorpho, aes(x=Nig)) + 
  geom_histogram(binwidth = 0.1)+
  scale_x_log10(limits=c(-.1,10000))+
  ylim(0,40)+
  geom_segment(aes(x = 144.19, y = 39, xend = 144.19, yend = 0),
               linetype = "dashed" ,color="darkgrey",size=0.75)+
  xlab("Individuals Used for Genetics")+
  ylab("Number of Species")+
  theme_bw()+
  theme(panel.grid.minor=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


histmorph<-ggplot(surveymorpho, aes(x=Nim)) + 
  geom_histogram(binwidth = 0.1)+
  scale_x_log10(limits=c(-.1,10000))+
  ylim(0,40)+
  geom_segment(aes(x = 183.48, y = 39, xend = 183.48, yend = 0),
               linetype = "dashed" ,color="darkgrey",size=0.75)+
  xlab("Individuals Used for Morphology")+
  ylab("Number of Species")+
  theme_bw()+
  theme(panel.grid.minor=element_blank())

plot_grid(histgen,histmorph,labels=c("A","B"),nrow=2)


#Now do barplot by larval type

#surveylarv<-filter(surveymorpho, Larv_type != "NA") -- in case we want to use only those with known data

ggplot(surveylarv,aes(x=phylum_wormsV1,fill=CSss))+
  geom_bar()+
  xlab("Phylum")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Larv_type,ncol=1)

# OR

larv_type<-ggplot(surveymorpho,aes(x=phylum_wormsV1,fill=Larv_type))+
  geom_bar()+
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 3, type="continuous"))) +
  xlab("Phylum")+
  ylab("Number of Species")+
  labs(fill = "Larval Type")+
  theme_bw()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))


#Habitat graphs

#surveyhab<-filter(surveymorpho, HKKv3 != "NA") -- in case we want to use only those with known data


hkk<-ggplot(surveymorpho,aes(x=phylum_wormsV1,fill=as.factor(HKKv3)))+
  geom_bar()+
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 6, type="continuous"))) +
  ylab("Number of Species")+
  labs(fill = "Extent Index")+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))


ggplot(surveyhab,aes(x=phylum_wormsV1,fill=CSss))+
  geom_bar()+
  xlab("Phylum")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  facet_wrap(~ HKKv3,ncol=1)

plot_grid(hkk,larv_type,labels=c("A","B"),nrow=2)


# What about planktonic and benthic
# We are not using this (May 26 2026)

surveypel<-filter(surveymorpho, adultpelagic != "NA") #-- in case we want to use only those with known data


ggplot(surveypel,aes(x=adultpelagic,fill=CSss))+
  geom_bar()+
  xlab("Pelagic adults")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))