## STATISTICS BASED ON THE NOMINAL SPECIES DATASET - Cryptic species meta-analysis - august 2023
# Do not forget to specify the working directory before starting
# USE arrows in the left margin to collapse rows and visualize the organization/contents (3 main parts) of the file ("

library(dplyr) # to manipulate data
library(corrplot) # draws colored correlation tables (not strictly necessary here)
library(bcp)  # to identify change points in a series of data
library(vioplot) # remove this one if violin plots redone with ggplot (draw violin plots)
library(ggplot2) # used to make plots
library(ggpubr)    # to share common legend for several plots (with ggplot)
library(gridExtra) # to arrange several plots with ggplot
library(bestglm) # compares glm models based on a criterion (BIC was chosen here)
library(car) # used to check variance inflation factors
library(stringr)

###########################################################################
# Part_1 # Preparation of the data (load data, create variables, etc.)
###########################################################################
list.files()
data<-read.csv2( "DATASET_187603_NominalSpecies.csv" ,header=T)
data<-as.data.frame(unclass(data),     # Convert all columns to factor (instead of characters, numericals do not change)
              stringsAsFactors = TRUE)
data$dna<-factor(data$dna, levels=c("No seq","nuc","mito","nuc&mito")) # so the reference level becomes "No seq" for statistics
data$mitonuc <-(data$dna=="nuc&mito")  # boolean used below to build table per decade
data$hasRIOK[is.na(data$RIOK)]<-FALSE     # converts NA values to FALSE
data$phylum_class<-factor(paste0(data$phylum, data$class))

table(data$hasRIOK, useNA="always") # 446 RIOK
table(data$has1cpa, useNA="always") # 492 1cpa
table(data$hasCSss, useNA="always") # 267 CSss
table(data$mitonuc, useNA="always") # 23037 TRUE, 3 NA, 164563 F

breaks=c(1750,1760,1770,1780,1790,1800,1810,1820,1830,1840,1850,1860,1870,1880,1890,1900,1910,
         1920,1930,1940,1950,1960,1970,1980,1990,2000,2010,2020)
data$decade<-cut(data$yearb,breaks=breaks)
levels(data$decade)<-c("1751-1760","1761-1770","1771-1780","1781-1790","1791-1800",
                       "1801-1810","1811-1820","1821-1830","1831-1840","1841-1850",
                       "1851-1860","1861-1870","1871-1880","1881-1890","1891-1900",
                       "1901-1910","1911-1920","1921-1930","1931-1940","1941-1950",
                       "1951-1960","1961-1970","1971-1980","1981-1990","1991-2000",
                       "2001-2010","2011-2020")
# min(data$yearb) # 1756
# max(data$yearb) # 2020
# data[500:520,c("yearb", "decade")]

#LET US CHECk DECADES limits are OK (no NA)
table(data$decade, useNA="always")

break3<-c(1755,1859,1980,2020) # periods limited / Darwin, Kimura, then end of survey
# break3<-c(1756,1859,1980,2020) # This was a mistake since the 1756 species was excluded from these break !
data$y3<-cut(data$yearb,breaks=break3)
levels(data$y3)<-c("1756-1859","1860-1980","1981-2020")
table(data$y3, useNA="always")

break3b<-c(1755,1859,1942,2020) # periods limited / Darwin, Mayr, then end of survey
data$y3b<-cut(data$yearb,breaks=break3b)
levels(data$y3b)<-c("1756-1859","1860-1942","1943-2020")
table(data$y3b, useNA="always")

break4<-c(1755,1859,1942,1980,2020) # periods limited / Darwin, Mayr, Kimura, then end of survey
data$y4<-cut(data$yearb,breaks=break4)
levels(data$y4)<-c("1756-1859","1860-1942","1943-1980","1981-2020")
table(data$y4, useNA="always")

data$area<-cut(data$CHull_r100, breaks=20, labels = 1:20,ordered_result = T)
table(data$area, useNA="always") # range size category: the smaller have more observations (so cuts are equal range sizes)
min(data$CHull_r100, na.rm=T)
max(data$CHull_r100, na.rm=T)

dato<-data[is.na(data$nb_zones)==F,] #99614 observations remaining in file with OBIS data
Npol<-dato[dato$Npol==T,]    # dataframe of species found in North Polar zone
Ntemp<-dato[dato$Ntemp==T,]  # dataframe of species found in North temperate zone
Trop<-dato[dato$Trop==T,]    # dataframe of species found in Tropical zone
Stemp<-dato[dato$Stemp==T,]  # dataframe of species found in South temperate zone
Spol<-dato[dato$Spol==T,]    # dataframe of species found in South Polar zone
 
dati<-data[data$ncbi==T,] # 38156
datrange<-data[(is.na(data$range_lat_r100)==F)&(is.na(data$range_long_r100)==F),] #13560
databigphyl <-data.frame(filter(data, (phylum=="Arthropoda")|(phylum=="Annelida")|(phylum=="Bryozoa")|(phylum=="Chordata")|(phylum=="Cnidaria")|(phylum=="Echinodermata")|(phylum=="Mollusca")|(phylum=="Nemertea")|(phylum=="Porifera")))
databigphyl$phylum<-factor(databigphyl$phylum, levels=c("Nemertea","Bryozoa","Echinodermata","Porifera","Cnidaria","Annelida","Chordata","Mollusca","Arthropoda")) #reorder by size
###########################################################################
# Part_2 # Descriptions: histograms, correlograms, barplot & boxplots, building some tables
###########################################################################
## A- Histograms and correlogram for numerical variables ####
hist(data$yearb)  # can replace 'data' by each zone, e.g. hist(Npol$yearb). Not shown here for concision.
hist(data$gs)
hist(data$range_lat_r100)
hist(data$range_long_r100)
hist(data$CHull_r100, breaks=30)

mat<-select(dato,c(yearb,gs,nb_zones,range_lat_r100,range_long_r100,CHull_r100))
matrix<-cor(mat,use="complete.obs")
corrplot(matrix,type="upper", order="hclust", tl.col="black", tl.srt=45)

## B- Table of CS presence per type of sequence data available ####
table(data$hasCS, data$dna)    # Table in manuscript
# No seq    nuc   mito nuc&mito
# FALSE 149403   3131  11836    22398
# TRUE      44     14    135      639

table(data$hasRIOK, data$dna)     # same thing for  CS where Reproductive isolation is confirmed
# No seq    nuc   mito nuc&mito
# FALSE 149421   3134  11918    22681
# TRUE      26     11     53      356

## C- Range size comparison with/out CS ####

# sdlat<-as.data.frame(tapply(data$range_lat_r100, data$hasCS,sd, na.rm=T))
# sdlong<-as.data.frame(tapply(data$range_long_r100, data$hasCS,sd, na.rm=T))
# sdCHull<-as.data.frame(tapply(data$CHull_r100, data$hasCS,sd, na.rm=T))

meanlat<-as.data.frame(tapply(data$range_lat_r100, data$hasCS,mean, na.rm=T))
meanlong<-as.data.frame(tapply(data$range_long_r100, data$hasCS,mean, na.rm=T))
meanCHull<-as.data.frame(tapply(data$CHull_r100, data$hasCS,mean, na.rm=T))
meanlat[2,1]/meanlat[1,1]       # 1.253349 (OLD 1.224714 (before rarefaction, it was 2.571744 probably strongly biased by effort!)
meanlong[2,1]/meanlong[1,1]     # 1.343295 (OLD 1.324631 (before rarefaction, it was 2.768405 probably strongly biased by effort!)
meanCHull[2,1]/meanCHull[1,1]   # 1.623933 (OLD 1.579785)

### Violin plots for range sizes with/out CS (Figure 2 main manuscript) 
My_Theme = theme(
  # axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    legend.text=element_text(size=12),
    legend.title=element_text(size=16))

a<-ggplot(data)+
  aes(x=hasCS, y=range_lat_r100)+
  geom_violin(aes(fill=hasCS))+
  geom_boxplot(width=0.1)+ 
  stat_summary(fun.y=mean, geom="point", size=2, color="red")+
  # scale_fill_manual(values=c("#00000099","#CCCCCC"))+ labs(fill="CS reported",y=NULL,x=NULL)+
  scale_fill_manual(values=c("#00000099","#CCCCCC"))+ 
  labs(fill=NULL,y=NULL,x=NULL)+
  theme(legend.position="none")+
  My_Theme
b<-ggplot(data)+aes(x=hasCS, y=range_long_r100)+geom_violin(aes(fill=hasCS))+
  geom_boxplot(width=0.1)+ stat_summary(fun.y=mean, geom="point", size=2, color="red")+
  scale_fill_manual(values=c("#00000099","#CCCCCC"))+ labs(fill="CS reported",y=NULL,x=NULL)+
  # scale_x_discrete(labels = NULL)+
  theme(legend.position="none")+
  My_Theme
c<-ggplot(data)+aes(x=hasCS, y=CHull_r100)+geom_violin(aes(fill=hasCS))+ 
  geom_boxplot(width=0.1)+ stat_summary(fun.y=mean, geom="point", size=2, color="red")+
  scale_fill_manual(values=c("#00000099","#CCCCCC"))+ labs(fill="CS reported",y=NULL,x=NULL)+
  # scale_x_discrete(labels = NULL)+
  theme(legend.position="none")+
  My_Theme
g<-ggarrange(a,b,c, ncol=3,nrow=1, 
          # common.legend=TRUE, 
          # legend="top",
          labels=c("A","B","C"),
          font.label=list(size=18,color="black"))
g

png(file = "Fig.2.png", width = 18, height = 8, res=600, units="cm") # can change unit:'px' by default, 'cm', 'in'...
g
dev.off() # OK
pdf(file = "Fig.2.pdf", width = 7, height = 4, pointsize = 10)# inches
g
dev.off() # problem pdf does not print the red mean!

# test (required in minor revision step)
wilcox.test(dato$CHull_r100[dato$hasCS==TRUE], dato$CHull_r100[dato$hasCS==FALSE])
wilcox.test(dato$range_lat_r100[dato$hasCS==TRUE], dato$range_lat_r100[dato$hasCS==FALSE])
wilcox.test(dato$range_long_r100[dato$hasCS==TRUE], dato$range_long_r100[dato$hasCS==FALSE])

### Boxplots range CHull/hasCS/big phyla OR all phyla
ggplot(data = databigphyl, aes(x=phylum, y=CHull_r100)) + geom_boxplot(aes(fill=hasCS),position=position_dodge(0.6))+ coord_flip()+theme(axis.text.y = element_text(size = 12))

## D- Effects of time of species description ####
###   a- Rapid histogram and boxplot with/out CS and Year (nicer Figure below) ####

breakq <-quantile(data$year,probs=seq(0,1,0.05),na.rm=T)
no<-hist(data$yearb[data$hasCS==F],breaks=breakq)
yes<-hist(data$yearb[data$hasCS==T],breaks=breakq)
plot(no,col="blue", main="with CS (green), without CS (blue)",freq=F)
plot(yes,col=rgb(0,1,0,0.5),add=T,freq=F) 

ggplot(data = databigphyl, aes(x=phylum, y=yearb)) + geom_boxplot(aes(fill=hasCS),position=position_dodge(0.6))+ coord_flip()+theme(axis.text.y = element_text(size = 12))

###   b- Exploration of temporal changes for all variables (tables,change point detection, supplementary figure) ####

#### Range sizes 
vioplot(data$CHull_r100~data$decade)
means <- tapply(datrange$CHull_r100,datrange$decade,mean)
points(means,col="red",pch=15) 
vioplot(data$range_lat_r100~data$decade)
means <- tapply(datrange$range_lat_r100,datrange$decade,mean)
points(means,col="red",pch=15)
vioplot(data$range_long_r100~data$decade)
means <- tapply(datrange$range_long_r100,datrange$decade,mean)
points(means,col="red",pch=15)

#### BAYESIAN CHANGE POINT analyses for range size
means <- tapply(data$CHull_r100,data$decade,mean,na.rm=T) # done also with latitude or longitude range
bcp<-bcp(as.vector(means))
plot(bcp,xaxlab=rownames(means),xlab="",
     outer.margins = list(left = unit(4, "lines"), 
                          bottom = unit(3, "lines"), right = unit(3, "lines"),top = unit(2, "lines")),
                          main="") # to plot posterior values and probas of change
bcp       # to check point after which change is most likely (highest probability)

# write in a png file:
png(file = "Fig.S3.png", width = 18, height = 15, res=600, units="cm") # can change unit:'px' by default, 'cm', 'in'...
plot(bcp,xaxlab=rownames(means),xlab="",
     outer.margins = list(left = unit(4, "lines"), bottom = unit(3, "lines"), right = unit(3, "lines"),top = unit(2, "lines")),
     main="") # 
dev.off() # OK
# write in a pdf file:
pdf(file = "Fig.S3.pdf", width = 7, height = 6, pointsize = 10)# inches
plot(bcp,xaxlab=rownames(means),xlab="",
     outer.margins = list(left = unit(4, "lines"), bottom = unit(3, "lines"), right = unit(3, "lines"),top = unit(2, "lines")),
     main="") # 
dev.off() # 


#### Boxplot combining Range size, Decade and CS presence (Figure S4)
My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 16),
  legend.title = element_text(size=16))

a<-ggplot(data = data, aes(x=decade, y=CHull_r100/1000)) + 
  geom_boxplot(aes(fill=hasCS),position=position_dodge(0.6))+ 
  scale_fill_manual(values = c(c("#00000099","#CCCCCC")))+
  labs(fill="CS reported")+
  theme_bw()+
  xlab("Decade") + ylab("Range size")+ scale_x_discrete(breaks = c("1760","1780","1800","1820","1840","1860","1880","1900","1920","1940","1960","1980","2000"))+
  My_Theme
b<-ggplot(data = data, aes(x=area, y=yearb)) + 
  geom_boxplot(aes(fill=hasCS),position=position_dodge(0.6))+ 
  scale_fill_manual(values = c(c("#00000099","#CCCCCC")))+
  theme_bw() +
  xlab("Range size category") + ylab("Year")+
  My_Theme
g<-ggarrange(a, b, ncol=1, nrow=2, common.legend = TRUE, legend="top")  
g
# printing in full page width (18 cm)
png(file = "Fig.S4.png", width = 18, height = 15, res=600, units="cm") # can change unit:'px' by default, 'cm', 'in'...
g
dev.off()
pdf(file = "Fig.S4.pdf", width = 7, height = 7, pointsize = 10)# inches
g
dev.off()

#### Genus size 
vioplot(data$gs~data$decade)
mean<-tapply(data$gs,data$decade,mean)
points(mean, col="red",pch=15)

#### Summary TABLE per decade (species counts and %, mean ranges)
m<-as.matrix(table(data$decade,data$hasCS))
colnames(m)<-c("hasNoCS","hasCS")
n<-as.matrix(table(data$decade,data$NbSitesUsed_r100==100))
colnames(n)<-c("has100sites")
i<-as.matrix(table(data$decade,data$ncbi))
colnames(i)<-c("no_NCBI","has_ncbi")
j<-as.matrix(table(data$decade, data$mitonuc)) 
colnames(j)<-c("FALSE","mito_nuc")
k<-as.matrix(table(dati$decade, dati$NbSitesUsed_r100==100)) #change dataframe to have NCBI
colnames(k)<-c("NCBI&100sites") # nb NS with NCBI data and 100 sites OBIS
l<-as.matrix(table(dati$decade, dati$hasCS==T & dati$NbSitesUsed_r100==100))
colnames(l)<-c("FALS","CS_nbci_100s")
# o<-as.matrix(table(data$decade, data$hasCS==T & data$ncbi==T)) 
o<-as.matrix(table(dati$decade, dati$hasCS==T))
colnames(o)<-c("FAL","CS_nbci")
  
mean_CHull<-tapply(data$CHull_r100,data$decade,FUN="mean",na.rm=T)
mnir<-cbind(m,n,i,j,k,l,o,mean_CHull)
mnir<-as.data.frame(mnir)
mnir$Nb_NS<-mnir$hasNoCS+mnir$hasCS
mnir<-select(mnir,c("Nb_NS","hasCS","has_ncbi","has100sites","NCBI&100sites","CS_nbci_100s","CS_nbci","mean_CHull","mito_nuc"))#
mnir$meanChull_exp6<-mnir$mean_CHull/1000000
mnir<-select(mnir, -c(mean_CHull))
mnir$percent_cs<-100*(mnir$hasCS/mnir$Nb_NS)
mnir$percent_ncbi<-100*mnir$has_ncbi/mnir$Nb_NS
mnir$percent_mitonuc <-100*mnir$mito_nuc/mnir$Nb_NS
mnir$percent_cs_ncbi<-100*o[,2]/(o[,1]+o[,2]) # 
write.csv2(mnir, file="table_per_decade.csv")

#### Decade Figures (y= , x=decade) (Figure S2)
mnir$decade<-row.names(mnir)
mytheme<-theme(axis.title.x =element_blank())
#rename decades for nicer plots below (CAUTION BE AWARE that the round year is the upper limit of the decade)
mnir$decade <-substring(mnir$decade, 6,10)            
a<-ggplot(data=mnir)+geom_point(mapping=aes(x=decade,y=percent_ncbi))+scale_x_discrete(breaks=c("1750","1800", "1850","1900", "1950","2000"))+ mytheme + scale_y_continuous(name="% NS in NCBI")
b<-ggplot(data=mnir)+geom_point(mapping=aes(x=decade,y=has100sites))+scale_x_discrete(breaks=c("1750","1800", "1850","1900", "1950","2000"))+ mytheme + scale_y_continuous(name="Number of NS with 100 OBIS sites")
c<-ggplot(data=mnir)+geom_point(mapping=aes(x=decade,y=meanChull_exp6))+scale_x_discrete(breaks=c("1750","1800", "1850","1900", "1950","2000"))+ mytheme + ylab(bquote('Mean range size (million '~ km^2~')'))
d<-ggplot(data=mnir)+geom_point(mapping=aes(x=decade,y=percent_cs_ncbi))+scale_x_discrete(breaks=c("1750","1800", "1850","1900", "1950","2000"))+ mytheme + scale_y_continuous(name="% CS /NS in NCBI")
g<-ggarrange(a,b,c,d,ncol=2,nrow=2,labels=c("A","B","C","D"))
g
png(file = "Fig.S2.png", width = 18, height = 16, res=600, units="cm") # can change unit:'px' by default, 'cm', 'in'...
g
dev.off()
pdf(file = "Fig.S2.pdf", width = 7, height = 6, pointsize = 10)# inches
g
dev.off()


####  summary table per period (y4)
m<-as.matrix(table(data$y4,data$hasCS))
colnames(m)<-c("hasNoCS","hasCS")
n<-as.matrix(table(data$y4,data$NbSitesUsed_r100==100))
colnames(n)<-c("has100sites")
i<-as.matrix(table(data$y4,data$ncbi))
colnames(i)<-c("no_NCBI","has_ncbi")
j<-as.matrix(table(data$y4, data$mitonuc))
colnames(j)<-c("no_mi-&-nuc","mito_nuc")
k<-as.matrix(table(dati$y4, dati$NbSitesUsed_r100==100))
colnames(k)<-c("NCBI&100sites") # nb NS with NCBI data and 100 sites OBIS
l<-as.matrix(table(dati$y4, dati$hasCS==T & dati$NbSitesUsed_r100==100))
colnames(l)<-c("FALSE","CS_nbci_100s")
o<-as.matrix(table(dati$y4, dati$hasCS==T))
colnames(o)<-c("FALSE","CS_nbci")
mean_CHull<-tapply(datrange$CHull_r100,datrange$y4,FUN="mean")
mnir4<-cbind(m,n,i,j,k,l,o,mean_CHull)
mnir4<-as.data.frame(mnir4)
mnir4$Nb_NS<-mnir4$hasNoCS+mnir4$hasCS
mnir4<-select(mnir4,c("Nb_NS","hasCS","has_ncbi","has100sites","NCBI&100sites","CS_nbci_100s","CS_nbci","mean_CHull","mito_nuc"))# summary table per period
mnir4$meanChull_exp6<-mnir4$mean_CHull/1000000
mnir4<-select(mnir4, -c("mean_CHull"))
#compute percent_cs_ncbi for complex figure with barplots BELOW (CS parmi les esp avec NCBI data seulement, moins de CS que totalité de CS, certaines n'ayant pas de seq dans NCBI)
mnir4$percent_cs_ncbi<-100*o[,2]/(o[,1]+o[,2])  
write.csv2(mnir4, "table per period.csv",row.names=T)

###   c- Barplot with proportions CS/NCBI per decades (Figure 3) ####

# Figure 3A
mnir$noCS<-mnir$Nb_NS-mnir$hasCS
mnir$decade<-row.names(mnir)
library(tidyverse)
decad<-select(mnir,c(decade, noCS, hasCS))
# Scaling factor
sf <- max(decad$noCS)/max(decad$hasCS)
# Transform
DF_long <- decad %>%
  mutate(hasCS = hasCS*sf) %>%
  pivot_longer(names_to = "y_new", values_to = "val", noCS:hasCS)
# Plot
theme<-theme(legend.position = 'top',
             # plot.title = element_text(color='black',face='bold',hjust=0.5),
             axis.text.x = element_text(color='black',face='plain',size=12),#was 14
             axis.text.y = element_text(color='black',face='plain',size=9),
             axis.title = element_text(color='black',face='plain', size=12),
             legend.text = element_text(color='black',face='plain', size=12),#was 14
             legend.title = element_text(color='black',face='plain',size=12)) #was 16
A<- ggplot(DF_long, aes(x=decade)) +
  geom_bar( aes(y = val, fill = y_new, group = y_new),stat="identity", position=position_dodge(),color="black", alpha=.6)+
  scale_fill_manual(values = c(c("#CCCCCC","#00000099")),labels=c('with CS', 'without CS')) +
  scale_x_discrete(name="",breaks = c("1750","1800","1850","1900","1950","2000"))+
  scale_y_continuous(name = "       without",  
                     labels = scales::comma,position = "right",sec.axis = sec_axis(~./sf, name="with",
                                                                      labels = scales::comma))+
  labs(fill="Number of nominal species described  ")+
  theme_bw() + theme +
  geom_point(data=mnir, aes(y=2000*percent_cs_ncbi), shape = 21, colour = "#FF3300", fill = "#FF3300", size = 1.5, stroke = 1)+ # fonctionne mais rajouter l'axe +
  geom_hline(yintercept=2000, linetype="dashed", 
              color = "red", size=0.5) +# corresponds to scale for red dots ~1 % (to precise)
  geom_hline(yintercept=10000, linetype="dashed", 
             color = "red", size=0.8)+
  annotate("text", x = "1780", y = 10000, label = "5% CS", vjust = -0.5,color="red")+
  theme(plot.margin = margin(0.5,0.5,0.5,2.5, "cm")) # to shift right the A plot (relative to B, so that years are aligned)
# remark: the argument "position = "right"" resulted in moving the secondary axis left and primary right (this is less misleading than default due to excess CS for small x)

# Figure 3B
B<-ggplot(data=databigphyl, aes(x=phylum, y=yearb,fill=hasCS)) +
  geom_boxplot(alpha=0.6, width=0.65)+
  scale_fill_manual(values=c("black","#CCCCCC"))+
  scale_y_continuous(name="")+
  coord_flip()+
  ylab("\nYear of Description")+xlab("Phylum\n")+
  theme_bw()+
  theme(axis.title.x = element_text(size=14),#not used
        axis.text.x=element_text(size=12),#was 14
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=12),#was 14
        legend.position = "none",
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank()) +
  theme(plot.margin = margin(0.5,1.5,1,0.5, "cm"))# top right bottom left margins

g<-ggarrange(A,B,labels=c("A","B"), nrow=2, heights=c(4,3.2))  
g

png(file = "Fig.3.png", width = 18, height = 19, res=600, units="cm") # can change unit:'px' by default, 'cm', 'in'...
g
dev.off()
pdf(file = "Fig.3.pdf", width = 7, height = 8, pointsize = 10)# inches
g
dev.off()


## E- exploration and summary table per phylum ####

ggplot(databigphyl)+aes(x=phylum,y=CHull_r100)+geom_boxplot()+coord_flip()
ggplot(databigphyl)+aes(x=phylum,y=yearb)+geom_boxplot()+coord_flip()
ggplot(data)+aes(x=phylum,y=CHull_r100)+geom_boxplot()+coord_flip()
ggplot(data)+aes(x=phylum,y=yearb)+geom_boxplot()+coord_flip()

m<-as.matrix(table(data$phylum,data$hasCS))
colnames(m)<-c("hasNoCS","hasCS")
n<-as.matrix(table(data$phylum,data$NbSitesUsed_r100==100))
colnames(n)<-c("has100sites")
i<-as.matrix(table(data$phylum,data$ncbi))
colnames(i)<-c("no_NCBI","has_ncbi")
j<-as.matrix(table(data$phylum, data$mitonuc))
colnames(j)<-c("no_mi-&-nuc","mito_nuc")
k<-as.matrix(table(data$phylum, data$ncbi==T & data$NbSitesUsed_r100==100))
colnames(k)<-c("FALSE","NCBI&100sites") # nb NS with NCBI data and 100 sites OBIS
l<-as.matrix(table(data$phylum, data$hasCS==T & data$ncbi==T & data$NbSitesUsed_r100==100))
colnames(l)<-c("FALSE","CS_nbci_100s")

mean_CHull<-tapply(data$CHull_r100,data$phylum,FUN="mean",na.rm=T)
mnir_phylum<-cbind(m,n,i,j,k,l,mean_CHull)
mnir_phylum<-as.data.frame(mnir_phylum)
mnir_phylum$Nb_NS<-mnir_phylum$hasNoCS+mnir_phylum$hasCS
mnir_phylum<-select(mnir_phylum,c("Nb_NS","hasCS","has_ncbi","has100sites","NCBI&100sites","CS_nbci_100s","mean_CHull","mito_nuc"))# summary table per period
mnir_phylum$meanChull_exp6<-mnir_phylum$mean_CHull/1000000
mnir_phylum<-select(mnir_phylum, -c("mean_CHull"))
mnir_phylum$percent_cs<-100*(mnir_phylum$hasCS/mnir_phylum$Nb_NS)
mnir_phylum$percent_ncbi<-100*mnir_phylum$has_ncbi/mnir_phylum$Nb_NS
write.csv2(mnir_phylum, file="table_per_phylum.csv")
#conclusion: chaetognatha>nematoda> ctenophores> cnidaire> arthropoda>annelida ... for range size

## F- Latitude zones table ####

summary_per_zone  <- function(zone){
  nbNS<-nrow(zone)
  nbCS<-nrow(filter(zone, hasCS==T))
  nbNCBI<-nrow(filter(zone,ncbi==T))
  nbCS_in_NCBI<-nrow(filter(zone, (ncbi==T) & (hasCS==T)))
  nb100sites<-nrow(filter(zone, NbSitesUsed_r100==100))
  averageChull<-mean(zone$CHull_r100, na.rm=T)/1000000
  medianYear<-median(zone$yearb, na.rm=T)
  tabZone <-c(nbNS, nbCS, nbNCBI,nbCS_in_NCBI,nb100sites,averageChull,medianYear)
}

tablZones<-as.data.frame(cbind(summary_per_zone(Npol), 
                               summary_per_zone(Ntemp), 
                               summary_per_zone(Trop),
                               summary_per_zone(Stemp), 
                               summary_per_zone(Spol)))
row.names(tablZones)<-c("nbNS","nbCS","nbNCBI","nbCS_in_NCBI","nb100sites","averageCHull_exp6","medianYear")
colnames(tablZones) <-c("Npol","Ntemp","Trop", "Stemp","Spol")
write.csv2(tablZones, file = "table_of_zones bis.csv") # Table in the manuscript

## G - Mean year description with/without CS ####
meanyear<-as.data.frame(tapply(data$yearb, data$hasCS,mean, na.rm=T))
meanyear[1,1] - meanyear[2,1] # 62.4 years

#####################################################################################
# Part_3 # MODELS OF LOGISTIC REGRESSION GLM (comparison of a priori non redundant models, to find lowest AIC)
#####################################################################################

## A - Effect of SINGLE variables on 'having Cryptic species' ####
#### a- Factor of effort of genetic data (sequences in NCBI) and Bayesian change point analysis ... ####

summary(glm(hasCS~dna, data, family="binomial"))  # highly significant, not surprisingly
#AIC=8312 (oLD 5858.7) highly significant BEST AIC
drop1(glm(hasCS~dna, data, family="binomial"),test="Chisq") # ***

summary(glm(hasCS~ncbi, data, family="binomial"))  # highly significant, not surprisingly
#AIC=8481.7 (OLD 5933) highly significant

summary(glm(hasCS~mito, data, family="binomial"))  # highly significant, not surprisingly
#AIC=8464.9 (OLD 5962.6), highly significant  better AIC than ncbi

summary(glm(hasCS~mitonuc, data, family="binomial"))  # highly significant, not surprisingly
#AIC= 8836.3 worst, highly significant  much worse AIC

mat<-select(data, c(ncbi, mito, dna, mitonuc, hasCS))
outBIC<-bestglm(mat,IC="BIC", family=binomial,TopModels=8)  
outBIC$BestModels 
# ncbi  mito   dna mitonuc Criterion
# 1  TRUE  TRUE FALSE    TRUE  8340.533
# 2 FALSE FALSE  TRUE   FALSE  8340.533 # same model as above by construction of dna and same AIC
# 3  TRUE FALSE FALSE    TRUE  8342.590
# 4  TRUE FALSE  TRUE   FALSE  8352.675
# 5 FALSE  TRUE  TRUE   FALSE  8352.675
# 6 FALSE FALSE  TRUE    TRUE  8352.675
# 7  TRUE  TRUE  TRUE   FALSE  8364.818
# 8  TRUE FALSE  TRUE    TRUE  8364.818
outBIC$BestModel # same with AIC criterion

mat<-select(data, c(ncbi, mito, dna, mitonuc, hasCS))
outBIC<-bestglm(mat,IC="BIC", family=binomial,TopModels=8,nvmax=1)  
outBIC$BestModels 
# ncbi  mito   dna mitonuc Criterion
# 1 FALSE FALSE  TRUE   FALSE  8340.533
# 2 FALSE  TRUE FALSE   FALSE  8472.994
# 3  TRUE FALSE FALSE   FALSE  8489.887
# 4 FALSE FALSE FALSE    TRUE  8844.461
# 5 FALSE FALSE FALSE   FALSE 10676.274

##### Bayesian Change Point analysis on residuals of model hasCS~ncbi (Figure S1)
decades<-as.data.frame(table(data$decade), useNA="always")
dcs<-as.data.frame(table(data$decade[data$hasCS==T]))
decades$nbCS <-dcs$Freq[match(decades$Var1,dcs$Var1)]
decades
names(decades) <-c("decade","nbNS","nbCS")
# decades$nbCS[is.na(decades$nbCS)==T]<-"0"
nb_ncbi<-as.data.frame(table(data$decade[data$ncbi==T]))
decades$nb_ncbi <-nb_ncbi$Freq[match(decades$decade,nb_ncbi$Var1)]
decades$nb_ncbi[is.na(decades$nb_ncbi)==T]<-"0"
decades$nbCS<-as.numeric(decades$nbCS)
decades$nb_ncbi<-as.numeric(decades$nb_ncbi)

glm<-glm(nbCS~nb_ncbi+0,decades,family="poisson"(link="identity"))
decades$res_ncbi<-residuals(glm)
# Plot for Figure S1 below: 

plot(bcp(decades$res_ncbi),xaxlab=rownames(means),xlab="",main="")
dbcp<-bcp(decades$res_ncbi)
decades$posterior.mean<-dbcp$posterior.mean
decades$probaChange<-dbcp$posterior.prob

png(file = "Fig.S1.png", width = 18, height = 15, res=600, units="cm") # can change unit:'px' by default, 'cm', 'in'...
plot(bcp(decades$res_ncbi),xaxlab=rownames(means),xlab="",main="")
dev.off() # OK
pdf(file = "Fig.S1.pdf", width = 7, height = 6, pointsize = 10)# inches
plot(bcp(decades$res_ncbi),xaxlab=rownames(means),xlab="",main="")
dev.off() 

# same thing with mito instead of ncbi for bcp
nb_mito<-as.data.frame(table(data$decade[data$mito==T]))
decades$nb_mito <-nb_mito$Freq[match(decades$decade,nb_mito$Var1)]
decades$nb_mito[is.na(decades$nb_mito)==T]<-"0"
decades$nb_mito<-as.numeric(decades$nb_mito)

glm<-glm(nbCS~nb_mito+0,decades,family="poisson"(link="identity"))
decades$res_mito<-residuals(glm)
plot(bcp(decades$res_mito),xlab="Decade (1:[1750-1759]), 10:[1840,1849], 25:[1990,1999])",main="Residuals of: Nb CS ~ Nb mito spp (bcp analysis)")

glm<-glm(nbCS~nb_mito+nb_ncbi+0,decades,family="poisson"(link="identity"))
decades$res_ncbimito<-residuals(glm)
plot(bcp(decades$res_ncbimito),xlab="Decade (1:[1750-1759]), 10:[1840,1849], 25:[1990,1999])",main="Residuals of: Nb CS ~ n+m (bcp analysis)")


#### b- year or period (various variables tested individually) ####

summary(glm(hasCS~yearb, data, family="binomial"))
#AIC= 9892.8 (OLD 6960.8), highly significant

summary(glm(hasCS~y3, data, family="binomial"))
drop1(glm(hasCS~y3, data, family="binomial"),test="Chisq")
#AIC=10009 (OLD 7015), highly significant

summary(glm(hasCS~y4, data, family="binomial"))
drop1(glm(hasCS~y4, data, family="binomial"),test="Chisq")
#AIC= 9963.5 (OLD 6991), highly significant, better than y3

summary(glm(hasRIOK~yearb, data, family="binomial")) # best
summary(glm(hasRIOK~y3, data, family="binomial"))
summary(glm(hasRIOK~y4, data, family="binomial")) # better than y3

summary(glm(has1cpa~yearb, data, family="binomial")) # best
summary(glm(has1cpa~y3, data, family="binomial"))
summary(glm(has1cpa~y4, data, family="binomial")) # better than y3


#### c- range size variables (various variables) ####

summary(glm(hasCS~range_lat_r100, data, family="binomial"))
#AIC=3710.5 (OLD 2749), highly significant

summary(glm(hasCS~range_long_r100, data, family="binomial"))
#AIC= 3691.4 (OLD 2732.9), highly significant

summary(glm(hasCS~CHull_r100, data, family="binomial"))
#AIC= 3691.6 (OLD 2734.2), highly significant

mat<-select(data,c(CHull_r100, range_long_r100,range_lat_r100,hasCS))
outBIC<-bestglm(mat,IC="BIC", family=binomial,nvmax=1,TopModels=8)  
outBIC$BestModels 
# CHull_r100 range_long_r100 range_lat_r100 Criterion
# 1      FALSE            TRUE          FALSE  3699.560
# 2       TRUE           FALSE          FALSE  3699.768
# 3      FALSE           FALSE           TRUE  3718.675
# 4      FALSE           FALSE          FALSE 10677.358
# 5      FALSE            TRUE           TRUE       Inf
# 6       TRUE           FALSE           TRUE       Inf
# 7       TRUE            TRUE          FALSE       Inf

## B - Effect of TWO variables (1 for sequencing effort and another one) #### 
###  a - 2 variables: DNA with another variable ####

#### year or period variables
mat<-select(data,c(yearb,y3,y4,dna,hasCS))
outBIC<-bestglm(mat,IC="BIC", family=binomial,nvmax=3, TopModels=8)  #
outBIC$BestModels 
# yearb    y3    y4   dna Criterion
# 1  TRUE FALSE FALSE  TRUE  8243.596
# 2  TRUE  TRUE FALSE  TRUE  8246.279
# 3  TRUE FALSE  TRUE  TRUE  8258.208
# 4 FALSE  TRUE FALSE  TRUE  8273.740
# 5 FALSE FALSE  TRUE  TRUE  8276.943
# 6 FALSE  TRUE  TRUE  TRUE  8301.228
# 7 FALSE FALSE FALSE  TRUE  8341.131
# 8  TRUE  TRUE FALSE FALSE  9847.881
# OLD was close:
# # yearb    y3    y4   dna Criterion
# # 1  TRUE FALSE FALSE  TRUE  5853.098
# # 2  TRUE  TRUE FALSE  TRUE  5855.632
# # 3 FALSE  TRUE FALSE  TRUE  5862.727
# # 4  TRUE FALSE  TRUE  TRUE  5867.566
# # 5 FALSE FALSE  TRUE  TRUE  5870.775
# # 6 FALSE FALSE FALSE  TRUE  5886.940
# # 7 FALSE  TRUE  TRUE  TRUE  5894.952
# # 8  TRUE  TRUE FALSE FALSE  6926.949
outBIC<-bestglm(mat,IC="BIC", family=binomial,nvmax=2, TopModels=8)  #
outBIC$BestModels  
# yearb    y3    y4   dna Criterion
# 1  TRUE FALSE FALSE  TRUE  8243.596
# 2 FALSE  TRUE FALSE  TRUE  8273.740
# 3 FALSE FALSE  TRUE  TRUE  8276.943
# 4 FALSE FALSE FALSE  TRUE  8341.131
# 5  TRUE  TRUE FALSE FALSE  9847.881
# 6  TRUE FALSE  TRUE FALSE  9859.448
# 7  TRUE FALSE FALSE FALSE  9901.504
# 8 FALSE FALSE  TRUE FALSE  9992.573
# CONCLUSION: based on BIC, yearb (numeric) better than y3 (three periods,+ Darwin + Kimura) better than y4 (+ Mayr)

outAIC<-bestglm(mat,IC="AIC", family=binomial,nvmax=2, TopModels=8)  #
outAIC$BestModels  
# yearb    y3    y4   dna Criterion
# 1  TRUE FALSE FALSE  TRUE  8203.025
# 2 FALSE FALSE  TRUE  TRUE  8216.086
# 3 FALSE  TRUE FALSE  TRUE  8223.027
# 4 FALSE FALSE FALSE  TRUE  8310.703
# 5  TRUE  TRUE FALSE FALSE  9817.453
# 6  TRUE FALSE  TRUE FALSE  9818.877
# 7  TRUE FALSE FALSE FALSE  9891.361
# 8 FALSE FALSE  TRUE FALSE  9962.145

# OLD was slightly distinct, y4 became better
# # yearb    y3    y4   dna Criterion
# # 1 FALSE FALSE  TRUE  TRUE  5810.243
# # 2 FALSE  TRUE FALSE  TRUE  5812.284
# # 3  TRUE FALSE FALSE  TRUE  5812.744
# # 4 FALSE FALSE FALSE  TRUE  5856.674
# # 5  TRUE  TRUE FALSE FALSE  6896.683
# # 6  TRUE FALSE  TRUE FALSE  6897.824
# # 7  TRUE FALSE FALSE FALSE  6958.808
# # 8 FALSE FALSE  TRUE FALSE  6988.968

glm<-glm(hasCS~dna+yearb, data, family="binomial")
summary(glm) #AIC:8205  old  was 5814.7

glm<-glm(hasCS~dna+y3, data, family="binomial")
summary(glm) #AIC:8225 old was identical 5814.3

glm<-glm(hasCS~dna+y4, data, family="binomial")
summary(glm) #AIC:8218.1 (old was 5812.2 & y4 was very slightly better)

# ATTENTION MANUSCRIT SI ARGUMENT DONNE OLD VERSION LE CHANGER (voir aussi avec subsets: RIOK, CSss, ...)
# With new dataset, we can no longer claim that, based on AIC, When not considering range size, one observes that it is not bad to consider Mayr's period in addition to Darwin and Kimura.
# yearb (continuous) is better explaining than periods


#### range variables 
mat<-select(data,c(range_lat_r100,range_long_r100,CHull_r100,dna,hasCS))
outBIC<-bestglm(mat,IC="BIC", family=binomial,nvmax=3, TopModels=8)  #
outBIC$BestModels 
# range_lat_r100 range_long_r100 CHull_r100   dna Criterion
# 1          FALSE           FALSE       TRUE  TRUE  3523.877
# 2          FALSE            TRUE      FALSE  TRUE  3529.296
# 3          FALSE            TRUE       TRUE  TRUE  3529.508
# 4           TRUE            TRUE      FALSE  TRUE  3534.763
# 5           TRUE           FALSE       TRUE  TRUE  3535.995
# 6           TRUE           FALSE      FALSE  TRUE  3539.501
# 7          FALSE            TRUE       TRUE FALSE  3698.632
# 8          FALSE            TRUE      FALSE FALSE  3699.560

mat<-select(data,c(range_lat_r100,range_long_r100,CHull_r100,dna,hasCS))
outAIC<-bestglm(mat,IC="AIC", family=binomial,nvmax=3, TopModels=8)  #
outAIC$BestModels 

###  b - 2 variables: NCBI ####
# Note: NCBI is the least circular reasoning seq. effort variable, and the best based on BIC full models as done below in c,c',c") 
# temporal variables compared
mat<-select(data,c(yearb,y3,y4,ncbi,hasCS))
outBIC<-bestglm(mat,IC="BIC", family=binomial,nvmax=2)  #
outBIC$BestModels 
# yearb    y3    y4  ncbi Criterion
# 1  TRUE FALSE FALSE  TRUE  8337.793
# 2 FALSE  TRUE FALSE  TRUE  8377.966
# 3 FALSE FALSE  TRUE  TRUE  8378.204
# 4 FALSE FALSE FALSE  TRUE  8491.070
# 5  TRUE  TRUE FALSE FALSE  9847.881


## C - Effect of THREE types of variables :description time, DNA sequence, range size) TABLE_S2 ####
#### a- building TABLE S2 #### 
# 1st row table S2 (range variable==CHull)
mat<-select(data, c(yearb, y3,y3b,y4,dna, CHull_r100, hasCS))
outBIC<-bestglm(mat,IC="BIC", family=binomial,TopModels=8)  
outBIC$BestModels # the output provides values for Table S2 column 1
mat<-select(data, c(yearb, y3,y3b,y4,ncbi, CHull_r100, hasCS))
outBIC<-bestglm(mat,IC="BIC", family=binomial,TopModels=8)  
outBIC$BestModels 
mat<-select(data, c(yearb, y3,y3b,y4,mito, CHull_r100, hasCS))
outBIC<-bestglm(mat,IC="BIC", family=binomial,TopModels=8)  
outBIC$BestModels 
# 2nd row table S2 (range variable==range_long_r100)
mat<-select(data, c(yearb, y3,y3b,y4,mito, range_long_r100, hasCS))
outBIC<-bestglm(mat,IC="BIC", family=binomial,TopModels=8)  
outBIC$BestModels 
mat<-select(data, c(yearb, y3,y3b,y4,ncbi, range_long_r100, hasCS))
outBIC<-bestglm(mat,IC="BIC", family=binomial,TopModels=8)  
outBIC$BestModels 
mat<-select(data, c(yearb, y3,y3b,y4,dna, range_long_r100, hasCS))
outBIC<-bestglm(mat,IC="BIC", family=binomial,TopModels=8)  
outBIC$BestModels 
# 3rd row table S2 (range variable==range_lat_r100)
mat<-select(data, c(yearb, y3,y3b,y4,dna, range_lat_r100, hasCS))
outBIC<-bestglm(mat,IC="BIC", family=binomial,TopModels=8)  
outBIC$BestModels 
mat<-select(data, c(yearb, y3,y3b,y4,ncbi, range_lat_r100, hasCS))
outBIC<-bestglm(mat,IC="BIC", family=binomial,TopModels=8)  
outBIC$BestModels 
mat<-select(data, c(yearb, y3,y3b,y4,mito, range_lat_r100, hasCS))
outBIC<-bestglm(mat,IC="BIC", family=binomial,TopModels=8)  
outBIC$BestModels

#### b- same thing with hasRIOK instead of hasCS ####

mat<-select(data, c(yearb, y3,y3b,y4,dna, CHull_r100, hasRIOK))
outBIC<-bestglm(mat,IC="BIC", family=binomial,TopModels=8)  
outBIC$BestModels 
##same thing with AIC
outAIC<-bestglm(mat,IC="AIC", family=binomial,TopModels=8)  
outAIC$BestModels # same as with hasCS: best model includes both yearb and y3b, then yearb+y4 (and dna + CHull)

## check that y3b (Darwin + Mayr) is significant in models: yes, 3 variables ***
glm<-glm(hasRIOK~y3b + dna + CHull_r100, data,family="binomial")
drop1(glm, test="Chisq")
glm<-glm(hasRIOK~yearb+ y3b + dna + CHull_r100, data,family="binomial")
drop1(glm, test="Chisq") # still: yearb**, y3b***, CHUll***, dna***
glm<-glm(hasRIOK~yearb+ y3 + dna + CHull_r100, data,family="binomial")
drop1(glm, test="Chisq") # still: yearb**, y3**, CHUll***, dna***
glm<-glm(hasRIOK~yearb+ y4 + dna + CHull_r100, data,family="binomial")
drop1(glm, test="Chisq") # still: yearb**, y4***, CHUll***, dna***

## D_Comparison among models proposing ALSO other variables: genus size , phylum_class or phylum ####
# (except sequence variables, computation too long, we chose to, use 'dna' because it was in best models and best also alone)
mat<-select(data, c(yearb, y3,y3b,y4,dna, CHull_r100, range_long_r100, range_lat_r100,phylum, hasRIOK))
outAIC<-bestglm(mat,IC="AIC", family=binomial,TopModels=12,nvmax=5)  
outAIC$BestModels 
# yearb    y3   y3b    y4  dna CHull_r100 range_long_r100 range_lat_r100 phylum Criterion
# 1   TRUE FALSE  TRUE FALSE TRUE       TRUE           FALSE          FALSE   TRUE  2061.146
# 2   TRUE FALSE  TRUE FALSE TRUE      FALSE           FALSE           TRUE   TRUE  2062.191
# 3   TRUE FALSE  TRUE FALSE TRUE      FALSE            TRUE          FALSE   TRUE  2062.879
# 4   TRUE FALSE FALSE  TRUE TRUE       TRUE           FALSE          FALSE   TRUE  2063.145
# 5   TRUE FALSE FALSE  TRUE TRUE      FALSE           FALSE           TRUE   TRUE  2064.190
# 6   TRUE FALSE FALSE FALSE TRUE       TRUE           FALSE          FALSE   TRUE  2064.868
# 7   TRUE FALSE FALSE  TRUE TRUE      FALSE            TRUE          FALSE   TRUE  2064.879
# 8   TRUE  TRUE FALSE FALSE TRUE       TRUE           FALSE          FALSE   TRUE  2065.238
# 9   TRUE FALSE  TRUE FALSE TRUE       TRUE           FALSE          FALSE  FALSE  2065.481
# 10  TRUE FALSE FALSE FALSE TRUE      FALSE           FALSE           TRUE   TRUE  2065.955
# 11  TRUE  TRUE FALSE FALSE TRUE      FALSE           FALSE           TRUE   TRUE  2066.261
# 12  TRUE FALSE FALSE FALSE TRUE      FALSE            TRUE          FALSE   TRUE  2066.737
outBIC<-bestglm(mat,IC="BIC", family=binomial,TopModels=12,nvmax=5)  
outBIC$BestModels 
# yearb    y3   y3b    y4  dna CHull_r100 range_long_r100 range_lat_r100 phylum Criterion
# 1   TRUE FALSE FALSE FALSE TRUE       TRUE           FALSE          FALSE  FALSE  2118.130
# 2   TRUE FALSE FALSE FALSE TRUE      FALSE           FALSE           TRUE  FALSE  2120.585
# 3   TRUE FALSE FALSE FALSE TRUE      FALSE            TRUE          FALSE  FALSE  2121.925
# 4   TRUE FALSE FALSE FALSE TRUE       TRUE            TRUE          FALSE  FALSE  2130.223
# 5   TRUE FALSE FALSE FALSE TRUE       TRUE           FALSE           TRUE  FALSE  2130.252
# 6  FALSE FALSE FALSE FALSE TRUE       TRUE           FALSE          FALSE  FALSE  2131.099
# 7   TRUE FALSE FALSE FALSE TRUE      FALSE            TRUE           TRUE  FALSE  2131.818
# 8  FALSE FALSE  TRUE FALSE TRUE       TRUE           FALSE          FALSE  FALSE  2133.268
# 9  FALSE  TRUE FALSE FALSE TRUE       TRUE           FALSE          FALSE  FALSE  2133.897
# 10 FALSE FALSE FALSE FALSE TRUE      FALSE           FALSE           TRUE  FALSE  2134.522
# 11 FALSE FALSE  TRUE FALSE TRUE      FALSE           FALSE           TRUE  FALSE  2136.070
# 12 FALSE FALSE FALSE FALSE TRUE      FALSE            TRUE          FALSE  FALSE  2136.387

glm<-glm(hasCS~dna + yearb+y3b+CHull_r100+phylum+gs,data, family="binomial")
drop1(glm, test="Chisq")
glm<-glm(hasCS~dna + yearb+y3b+CHull_r100+phylum,data, family="binomial")
drop1(glm, test="Chisq")
# glm<-glm(hasCS~dna + y3b+CHull_r100+phylum,data, family="binomial")
# drop1(glm, test="Chisq") # all ***

mat<-select(data, c(yearb,y3b,y4,dna, CHull_r100,phylum,gs,hasCS))
outAIC<-bestglm(mat,IC="AIC", family=binomial,TopModels=12,nvmax=5)  
outAIC$BestModels 

glm<-glm(hasCS~dna+CHull_r100+yearb+y3b+phylum+gs, data, family="binomial")
drop1(glm, test="Chisq") # gs not significant, all others are ** (yearb) or ***(y3, dna, CHUll phylum)
summary(glm)
#conclusion gs not significant but coefficient is positive

#answer article lines 450 ...
mat <-select(data,c(dna,CHull_r100,yearb,phylum,phylum_class,gs,hasCS))
outAIC<-bestglm(mat,IC="AIC", family=binomial,TopModels=12,nvmax=5)  
outAIC$BestModels 
# dna CHull_r100 yearb phylum phylum_class    gs Criterion
# 1  TRUE       TRUE  TRUE   TRUE        FALSE FALSE  3395.350
# 2  TRUE       TRUE  TRUE   TRUE        FALSE  TRUE  3396.065
# 3  TRUE       TRUE FALSE   TRUE        FALSE FALSE  3425.524
# 4  TRUE       TRUE FALSE   TRUE        FALSE  TRUE  3425.787
# 5  TRUE       TRUE  TRUE  FALSE        FALSE  TRUE  3457.653
# 6  TRUE       TRUE  TRUE  FALSE        FALSE FALSE  3458.486
# 7  TRUE       TRUE  TRUE  FALSE         TRUE FALSE  3458.550
# 8  TRUE       TRUE  TRUE  FALSE         TRUE  TRUE  3460.007
# 9  TRUE       TRUE FALSE  FALSE        FALSE  TRUE  3481.032
# 10 TRUE       TRUE FALSE  FALSE        FALSE FALSE  3483.306
# 11 TRUE       TRUE FALSE  FALSE         TRUE FALSE  3492.036
# 12 TRUE       TRUE FALSE  FALSE         TRUE  TRUE  3493.324
outBIC<-bestglm(mat,IC="BIC", family=binomial,TopModels=12,nvmax=5)  
outBIC$BestModels
#     dna  CHull_r100 yearb phylum phylum_class    gs Criterion
# 1   TRUE       TRUE  TRUE  FALSE        FALSE FALSE  3509.196
# 2   TRUE       TRUE  TRUE  FALSE        FALSE  TRUE  3518.505
# 3   TRUE       TRUE FALSE  FALSE        FALSE FALSE  3523.874
# 4   TRUE       TRUE FALSE  FALSE        FALSE  TRUE  3531.742
# 5  FALSE       TRUE  TRUE  FALSE        FALSE FALSE  3640.356
# 6  FALSE       TRUE  TRUE  FALSE        FALSE  TRUE  3650.526
# 7  FALSE       TRUE FALSE  FALSE        FALSE FALSE  3699.768
# 8  FALSE       TRUE FALSE  FALSE        FALSE  TRUE  3708.290
# 9   TRUE       TRUE  TRUE   TRUE        FALSE FALSE  3740.181
# 10  TRUE       TRUE  TRUE   TRUE        FALSE  TRUE  3751.038
# 11  TRUE       TRUE FALSE   TRUE        FALSE FALSE  3760.213
# 12  TRUE       TRUE FALSE   TRUE        FALSE  TRUE  3770.618

# computation of BIC increase when "phylum, or "phylum_class" included in model 
(3740.181-3509.196)/3509.196 #0.066

mat <-select(data,c(dna,CHull_r100,yearb,phylum_class,hasCS))
outBIC<-bestglm(mat,IC="BIC", family=binomial,TopModels=12,nvmax=5)  
outBIC$BestModels

## E - Effect of changes between CONSECUTIVE PERIODS ####
###   a - Without a variable for range ####
# Note: test done here without range variable BECAUSE  history of science should lead authors to assume different expected ranges) 
dat12<-data[data$yearb<1943,]
dat12$y4<-factor(dat12$y4)
dat23<-data[(data$yearb>1859)&(data$yearb<1981),]
dat23$y4<-factor(dat23$y4)
dat34<-data[data$yearb>1942,]
dat34$y4<-factor(dat34$y4)

summary(glm(hasCS~y4+ncbi, dat12,family="binomial")) # ***    (Darwin) AIC: 6187   
summary(glm(hasCS~y4+ncbi, dat23,family="binomial")) # **      (Mayr)  AIC: 3400.8 
summary(glm(hasCS~y4+ncbi, dat34,family="binomial")) # 0.071 (Kimura) AIC: 2154  

summary(glm(hasCS~y4*ncbi, dat12,family="binomial")) # y4 ns (old**)  (Darwin)
summary(glm(hasCS~y4*ncbi, dat23,family="binomial")) # y4 ns (old*)   (Mayr) PAS COOL
summary(glm(hasCS~y4*ncbi, dat34,family="binomial")) # y4 ns          (Kimura)

###   b - With a variable for range ####

summary(glm(hasCS~y4+ncbi+CHull_r100, dat12,family="binomial")) #  period still ***  (Darwin)
summary(glm(hasCS~y4+ncbi+CHull_r100, dat23,family="binomial")) #  period not significant (Mayr) when a range variable is in the model
summary(glm(hasCS~y4+ncbi+CHull_r100, dat34,family="binomial")) #  period not significant (Kimura) when a range variable is in the model


## F - Testing biogeographical latitudinal configurations (panpolar, antitropical) ####
panpolar <-filter(data, Npol==TRUE & Spol ==TRUE)
table(panpolar$hasCS) # 14 TRUE /(148+14) = 8.64 % even without being in NCBI !!!
table(panpolar$hasCS, panpolar$dna)
# No seq nuc mito nuc&mito
# FALSE     36   7   18       87
# TRUE       0   0    1       13
14/(14+7+18+87) # 11.1%
table(data$hasCS, data$dna)
# compare with metazoans in obis (not all with 100 sites but do not care for this comparison since no dist range computed)
table(dato$hasCS, dato$dna)
(13+116+595)/(13+116+595+2361+9071+18477) #2.4%

antitrop<-filter(data, (Npol==TRUE | Ntemp ==T) & (Spol==T |Stemp==T) & (Trop==F))
table(antitrop$hasCS) # 22/(22+1421)
table(antitrop$hasCS, antitrop$dna) # 
# No seq nuc mito nuc&mito
# FALSE    853  67  126      375
# TRUE       1   1    2       18
(1+2+18)/(1+2+18+67+126+375) # 3.57%
table(dato$hasCS, dato$dna)

panpolar <-filter(dato, Npol==TRUE & Spol ==TRUE) #check same thing as with data
table(panpolar$hasCS) # 14 TRUE /(148+14) = 8.64 % even without condition being in NCBI !!!
table(panpolar$hasCS, panpolar$dna)

antitrop<-filter(dato, (Npol==TRUE | Ntemp ==T) & (Spol==T |Stemp==T) & (Trop==F))
table(antitrop$hasCS) #
table(antitrop$hasCS, antitrop$dna) #



## G- best model with unambiguous CS (reproductive isolation) ####

# Do results change when only presence of unambiguous CS are considered ?
drop1(glm(hasRIOK~yearb + CHull_r100 + dna,data ,family="binomial"),test="Chisq")  # CHUll**, dna***,yearb***
# Conclusion: all explanatory variables remain significant (with even lower p-values)

# summary(glm(hasRIOK~yearb + CHull_r100 + ncbi,data ,family="binomial")) # CHUll**, ncbi***,yearb***


