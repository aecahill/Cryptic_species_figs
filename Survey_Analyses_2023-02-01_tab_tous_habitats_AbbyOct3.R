# Statistical explorations of the survey dataset (each line is a cryptic species complex, some of which idetified at species level, others at genus.
# Notes for Oct 3 from Abby:
# Still unclear if I need to rerun the OBIS code, and I'm not sure how to check that - think about
# I would like to also do these analyses with a version of WORMS that considers Copepods/Thecostraca, but again, not sure how to do this


library(dplyr)
library(stringr)
library(ggplot2)
library(vioplot)
library(bestglm)
list.files()

survey <- read.csv(file="977_CScpx_100cols_Habitat&Larva_Completed_20230928_comma.csv"   , header=TRUE ) # file ready for analyses with new variables ,# data with range sizes rarefied for 100 sites, and RIOK
#survey <- read.csv2(file="670spp_65var_HKKv3_2023-01-17.csv"   , header=TRUE ) # file ready for analyses with new variables ,# data with range sizes rarefied for 100 sites, and RIOK
nomsp <-read.csv2(file="file_S2_oct.csv" ,header=T)# need to load all nominal spp data for some comparisons 
bio<-read.csv2(file="input_BIOLOGY_classes.csv")

# cleaned from some original variables and temporary variable (CAUTION: CHECK COLUMN nb!):
#survey <-select(survey, -c(2:9))  # <- Abby commented this out because the old and new versions of 'survey' do not have the same number of columns in the same order. WORK WITH NAMES.
# reintroduce genus but several possibilities: solution below not OK for CS at genus level (no data), can also use NameBoforeCor instead of acceptedName
survey$genus <-word(survey$acceptedName_wormsV1,1)
survey$genusBC <-word(survey$NameBeforeCor,1) # see if this genusBC changes results / genus.

# ATTENTION BELOW! I had to remove this for loops below, to compare survey$phylum_worms with phyla[i], survey$phylum_worms had to be a "character" not a factor.
# survey <-as.data.frame(unclass(survey),stringsAsFactors=TRUE) # thus this is moved after section A

# NOVEL VARIABLES in dataset : (add here if created, before A, B, C ... sections)
# simplification NB CATEGORIES GEO DIFF(consider PARApatry = ALLOpatry)
survey <-as.data.frame(unclass(survey),stringsAsFactors=TRUE)
survey$Geo_dif3 <-survey$Geo_diff
levels(survey$Geo_dif3)
levels(survey$Geo_dif3)<-c("","Allopatric", "Allopatric", "Allopatric","Sympatric","Sympatric, Allopatric", "Sympatric, Allopatric", "Sympatric, Allopatric")
levels(survey$Geo_dif3)
levels(survey$Geo_dif3)<-c(NA, "Allopatric","Sympatric","Sympatric, Allopatric")
levels(survey$Geo_dif3)# da$symp <-str_detect(da$Geo_diff,"Sympatric")
# add booleans for geographical differentiation and morphological differentiation variables
survey$symp    <- str_detect(survey$Geo_diff,"Sympatric")
survey$sympara <- str_detect(survey$Geo_diff,"Sympatric") | str_detect(survey$Geo_diff,"Parapatric")
survey$allop   <- str_detect(survey$Geo_diff,"Allopatric")
survey$NoMorphoDiff    <- survey$Morpho_diff=="No differentiation"
survey$StatMorphoDiff  <- survey$Morpho_diff=="Statistic"
survey$DiagMorphoDiff  <- survey$Morpho_diff=="Diagnostic"
# add biological traits of the class to which the CScpx belongs
survey$phylum_class <-paste0(survey$phylum_worms,"_",survey$class_worms)
survey$hard_skeleton  <- factor(bio$hard_skeleton[match(survey$phylum_class,bio$phylum_class)])
survey$ferti          <- factor(bio$ferti[match(survey$phylum_class,bio$phylum_class)])
survey$image          <- factor(bio$image[match(survey$phylum_class,bio$phylum_class)])
survey$genitals       <- factor(bio$genitals[match(survey$phylum_class,bio$phylum_class)])

# A- Number of CS/cpx and prop. missed biological sp (latit. zone, class, phylum) ####

# 1- By ZONE ####

# FACULTATIVE: TYPE COMMAND LINE BELOW TO restrict these data to species with NCBI data (check below the name of file to write table into):
nomsp<-filter(nomsp, ncbi==T)

#### For North polar zone

# nb of nominal species in zone
table(nomsp$Npol, useNA="always") # to check there are several cases and their numbers
a <- nrow(filter(nomsp,Npol==TRUE))
# nb of these which have CS in this zone
b <- nrow(filter(nomsp, (Npol==T)&(hasCS==T)))
# mean nb of CS per nominal sp with CS (based on survey dataset)
c <- mean((filter(survey, Npol==T))[,"Nb_CS"], na.rm=T)
sd <- sd((filter(survey, Npol==T))[,"Nb_CS"], na.rm=T)
# number of biological species MISSED due to CS when counting only nominal species (indirect estimation and direct count, differences unexplained)
m <- b*(c-1)                        
m2 <- sum(filter(survey, Npol==T)[,"Nb_CS"], na.rm=T) - nrow(filter(survey, Npol==T))   # direct count, quite distinct (but error since a few NS appear several times in survey)
#proportions of biological species missed
prop.m<-m/(a+m)
prop.m2<-m2/(a+m2)

missNpol<-c(a,b,c,sd,m,m2,prop.m,prop.m2)

#### For North temperate zone

# nb of nominal species in zone
table(nomsp$Ntemp, useNA="always") # to check
a <- nrow(filter(nomsp,Ntemp==TRUE))
# nb of these which have CS in this zone
b <- nrow(filter(nomsp, (Ntemp==T)&(hasCS==T)))
# mean nb of CS per nominal sp with CS (based on survey dataset)
c <- mean((filter(survey, Ntemp==T))[,"Nb_CS"], na.rm=T)
sd <-sd((filter(survey, Ntemp==T))[,"Nb_CS"], na.rm=T)
# number of biological species MISSED due to CS when counting only nominal species (both indirect estimation and direct count, differences may be due to CS with NA for Nb_CS but surprising because this is very exceptional)
m <- b*(c-1)                        
m2 <- sum(filter(survey, Ntemp==T)[,"Nb_CS"], na.rm=T) - nrow(filter(survey, Ntemp==T))   # direct count, quite distinct
#proportions
prop.m<-m/(a+m)
prop.m2<-m2/(a+m2)

missNtemp<-c(a,b,c,sd,m,m2,prop.m,prop.m2)

#### For Tropical zone

# nb of nominal species in zone
table(nomsp$Trop, useNA="always") # to check
a <- nrow(filter(nomsp,Trop==TRUE))
# nb of these which have CS in this zone
b <- nrow(filter(nomsp, (Trop==T)&(hasCS==T)))
# mean nb of CS per nominal sp with CS (based on survey dataset)
c <- mean((filter(survey, Trop==T))[,"Nb_CS"], na.rm=T)
sd <- sd((filter(survey, Trop==T))[,"Nb_CS"], na.rm=T)
# number of biological species MISSED due to CS when counting only nominal species (both indirect estimation and direct count, differences may be due to CS with NA for Nb_CS but surprising because this is very exceptional)
m <- b*(c-1)                        
m2 <- sum(filter(survey, Trop==T)[,"Nb_CS"], na.rm=T) - nrow(filter(survey, Trop==T))   # direct count, quite distinct
#proportions
prop.m<-m/(a+m)
prop.m2<-m2/(a+m2)

missTrop<-c(a,b,c,sd,m,m2,prop.m,prop.m2)

#### For South temperate zone

# nb of nominal species in zone
table(nomsp$Stemp, useNA="always") # to check
a <- nrow(filter(nomsp,Stemp==TRUE))
# nb of these which have CS in this zone
b <- nrow(filter(nomsp, (Stemp==T)&(hasCS==T)))
# mean nb of CS per nominal sp with CS (based on survey dataset)
c <- mean((filter(survey, Stemp==T))[,"Nb_CS"], na.rm=T)
sd <- sd((filter(survey, Stemp==T))[,"Nb_CS"], na.rm=T)
# number of biological species MISSED due to CS when counting only nominal species (both indirect estimation and direct count, differences may be due to CS with NA for Nb_CS but surprising because this is very exceptional)
m <- b*(c-1)                        
m2 <- sum(filter(survey, Stemp==T)[,"Nb_CS"], na.rm=T) - nrow(filter(survey, Stemp==T))   # direct count, quite distinct
#proportions
prop.m<-m/(a+m)
prop.m2<-m2/(a+m2)

missStemp<-c(a,b,c,sd,m,m2,prop.m,prop.m2)

#### For South Polar zone

# nb of nominal species in zone
table(nomsp$Spol, useNA="always") # to check
a <- nrow(filter(nomsp,Spol==TRUE))
# nb of these which have CS in this zone
b <- nrow(filter(nomsp, (Spol==T)&(hasCS==T)))
# mean nb of CS per nominal sp with CS (based on survey dataset)
c <- mean((filter(survey, Spol==T))[,"Nb_CS"], na.rm=T)
sd <-sd((filter(survey, Spol==T))[,"Nb_CS"], na.rm=T)
# number of biological species MISSED due to CS when counting only nominal species (both indirect estimation and direct count, differences may be due to CS with NA for Nb_CS but surprising because this is very exceptional)
m <- b*(c-1)                        
m2 <- sum(filter(survey, Spol==T)[,"Nb_CS"], na.rm=T) - nrow(filter(survey, Spol==T))   # direct count, quite distinct
#proportions
prop.m<-m/(a+m)
prop.m2<-m2/(a+m2)

missSpol<-c(a,b,c,sd,m,m2,prop.m,prop.m2)

# Joining all zones together
MissPerZone<-data.frame(missNpol,missNtemp, missTrop, missStemp, missSpol)
rownames(MissPerZone)<-c("nb nominal spp","nb NS having CS","mean nb CS/sp.with","sd_nb CS/sp.with", "estim. missed Biol sp", "count missed Biol sp","prop.m","prop.m2")

# WRITE TABLE in a file: choose name carefully (either restricted to NCBI species or not, below)
# write.csv2(MissPerZone, file="Missed biological species per zone.csv")
write.csv(MissPerZone, file="Missed biological species per zone NCBI TRUE Oct3 Good.csv")

# DO NOT FORGET: Need to reload nomsp with ALL data for other use of nomsp below:
nomsp <-read.csv2(file="C:/Users/aecsk/Downloads/file_S2_oct.csv" ,header=T)# need to load all nominal spp data for some comparisons 
# REMOVE temporary variables
rm(a,b,c,m,m2,missNpol, missNtemp, missTrop, missStemp, missSpol,prop.m,prop.m2,sd,MissPerZone)



# 2- By PHYLUM ####

phyla <- factor(unique(nomsp$phylum))
columns <- c("nb_NS","nb_NS_ncbi","nb_NS_withCS","nb_NS_withCS_ncbi","mean_Nb_CS_perCpx")
info    <- data.frame(matrix(nrow = length(phyla), ncol = length(columns))) 
colnames(info) = columns
row.names(info)<- phyla

survey$phylum_worms<-as.character(survey$phylum_worms) # required for the loop below to work

for (i in 1:length(phyla)){ 
  info$nb_NS[i] <-nrow(filter(nomsp,phylum==phyla[i]))
  info$nb_NS_ncbi[i] <-nrow(filter(nomsp,(phylum==phyla[i]) & (ncbi==T)))
  info$nb_NS_withCS[i] <-nrow(filter(nomsp,(phylum==phyla[i]) & (hasCS==T)))
  info$nb_NS_withCS_ncbi[i] <-nrow(filter(nomsp,(phylum==phyla[i])& (ncbi==T) & (hasCS==T)))
  info$mean_Nb_CS_perCpx[i] <-mean(filter(survey,(phylum_worms==phyla[i]))[,"Nb_CS"], na.rm=T)
  info$sd_Nb_CS_perCpx[i] <-sd(filter(survey,(phylum_worms==phyla[i]))[,"Nb_CS"], na.rm=T)
}  

info$missed_BS_estim <- info$nb_NS_withCS * (info$mean_Nb_CS_perCpx-1)
info$prop.missed_BS_estim <- info$missed_BS_estim / (info$missed_BS_estim + info$nb_NS )
info$prop.missed_BS_estim_ncbi <- info$missed_BS_estim / (info$missed_BS_estim + info$nb_NS_ncbi )  

write.csv(info, file="Missed biological species per phylum.csv")

# 3- By CLASS ####

nomsp$phylum_class<-paste0(nomsp$phylum,"_",nomsp$class)

# Initialise df and do a loop to compute, for each class, nb of missing biological species
classes <- factor(unique(nomsp$phylum_class))
columns <- c("nb_NS","nb_NS_ncbi","nb_NS_withCS","nb_NS_withCS_ncbi","mean_Nb_CS_perCpx")
info    <- data.frame(matrix(nrow = length(classes), ncol = length(columns))) 
colnames(info) = columns
row.names(info)<- classes

for (i in 1:length(classes)){ 
  info$nb_NS[i] <-nrow(filter(nomsp,phylum_class==classes[i]))
  info$nb_NS_ncbi[i] <-nrow(filter(nomsp,(phylum_class==classes[i]) & (ncbi==T)))
  info$nb_NS_withCS[i] <-nrow(filter(nomsp,(phylum_class==classes[i]) & (hasCS==T)))
  info$nb_NS_withCS_ncbi[i] <-nrow(filter(nomsp,(phylum_class==classes[i])& (ncbi==T) & (hasCS==T)))
  info$mean_Nb_CS_perCpx[i] <-mean(filter(survey,(phylum_class==classes[i]))[,"Nb_CS"], na.rm=T)
  info$sd_Nb_CS_perCpx[i] <-sd(filter(survey,(phylum_class==classes[i]))[,"Nb_CS"], na.rm=T)
}  
info$missed_BS_estim <- info$nb_NS_withCS * (info$mean_Nb_CS_perCpx-1)
info$prop.missed_BS_estim <- info$missed_BS_estim / (info$missed_BS_estim + info$nb_NS )
info$prop.missed_BS_estim_ncbi <- info$missed_BS_estim / (info$missed_BS_estim + info$nb_NS_ncbi )  

write.csv(info, file="Missed biological species per class.csv")


rm(info,classes,columns,phyla,i) # a bit of cleaning of the environment
####################################################################
survey <-as.data.frame(unclass(survey),stringsAsFactors=TRUE) # This could not be placed before A (transforms string variables into factors).


# B- Genus size per zone (CS and not CS)####

# Genus size / zones for CS ATTENTION eviter ERREUR CAR NE PAS COMPTER PLUSIEURS FOIS LE MEME GENRE AVANT LES CALCULS DE MOYENNES
genNpol <-unique(filter(survey,Npol==T)[,c("genus","gs")])
mean(genNpol$gs)  # 35.88608
# genNpol <-unique(filter(survey,Npol==T)[,c("genusBC","gs")])
# mean(genNpol$gs) # changes only second decimal
genNtemp <-unique(filter(survey,Ntemp==T)[,c("genus","gs")])
mean(genNtemp$gs) # 34.49107
# genNtemp <-unique(filter(survey,Ntemp==T)[,c("genusBC","gs")])
# mean(genNtemp$gs) # same thing, negligible difference
genTrop <-unique(filter(survey,Trop==T)[,c("genus","gs")])
mean(genTrop$gs)  # 35.85246
genStemp <-unique(filter(survey,Stemp==T)[,c("genus","gs")])
mean(genStemp$gs) # 34.10924
genSpol <-unique(filter(survey,Spol==T)[,c("genus","gs")])
mean(genSpol$gs)  # 26.46429   ### GENRES PLUS PETITS  POUR LES COMPX SPP CRYPTIQUES DU POLE SUD

# genus size / zone latitude for ANY NOMINAL spp (incl. non cryptic):
genNpol <-unique(filter(nomsp,Npol==T)[,c("genus","gs")])
mean(genNpol$gs)  # 22.15156
genNtemp <-unique(filter(nomsp,Ntemp==T)[,c("genus","gs")])
mean(genNtemp$gs) # 11.4279
genTrop<-unique(filter(nomsp, Trop==T)[,c("genus","gs")])
mean(genTrop$gs)  # 11.19951
genStemp<-unique(filter(nomsp, Stemp==T)[,c("genus","gs")])
mean(genStemp$gs) # 12.97221
genSpol<-unique(filter(nomsp, Spol==T)[,c("genus","gs")])
mean(genSpol$gs)  # 20.74541

#cleaning environment
rm(genNpol, genNtemp, genTrop, genStemp, genSpol)


# C- Summary tables per phylum (could have been added to table produced in A, but chose to print another dataframe for clarity)  ####

tab1<-table(survey$phylum_worms,survey$Morpho_diff)
colnames(tab1) <-c("Diag_morphoDiff","No_morphoDiff","Statistic_morphoDiff")
tab2<-table(survey$phylum_worms,survey$Geo_dif3)
colnames(tab2)
tab3<-table(survey$phylum_worms,survey$Eco_diff)
colnames(tab3)<-c("Eco_diff_F","Eco_diff_T")  
tab4<-table(survey$phylum_worms,survey$larv.pktro)
colnames(tab4)<-c("larv.pktro_F","larv.pktro_T")
tab5<-table(survey$phylum_worms,survey$larv.leci)
colnames(tab5)<-c("larv.leci_F","larv.leci_T")
tab6<-table(survey$phylum_worms,survey$larv.dd)
colnames(tab6)<-c("larv.dd_F","larv.dd_T")
tab7<-table(survey$phylum_worms,survey$larv.pel)
colnames(tab7)<-c("larv.pel_F","larv.pel_T")
tab8 <-table(survey$phylum_worms,survey$HKKv2)
colnames(tab8)<-c("HKK.1","HKK.5","HKK.10","HKK.25","HKK.100","HKK.1000")
tab9 <-table(survey$phylum_worms,survey$Hsubstrate)
colnames(tab9)

# tab12<-as.data.frame(aggregate(survey$Nb_CS,FUN=mean,by=list(survey$phylum_worms),na.rm=T))
# names(tab12)<-c("Phyl","Nb_CS (mean)")
# row.names(tab12)<-tab12$Phyl
# tab<-select(tab12,2)

#MERGE ALL TABLES IN A SINGLE DATA.FRAME (then can delete in excel unnecessary ones)
tab_phyla<-cbind(tab1,tab2,tab3,tab4,tab5,tab6,tab7,tab8,tab9)
#COpy in a file all above table counts per phylum

write.csv(tab_phyla,file="Joint_Table_19phyla_977NS_Oct3.csv",row.names=T) 

rm(tab1,tab2,tab3,tab4,tab5,tab6,tab7,tab8,tab9) # cleaning, may also remove (tab_phyla)
  

# D- Tests for pairs of categorical variables on contingency tables ####

######## AUTOMATIZED PAIRWISE FACTOR TESTS (contingency tables) AND OUTPUT IN A SINGLE MATRIx 
### CAUTION: for sub-datasets, remove factors which have a single level (eg: Diag_morpho in CSss)
## To avoid too big output, I used either phylum or image, or hard_skeleton, or ferti, or genitals (change 4 lines in comment: factors<-, row.names<-, colNames<- and write.csv2<-)

## (1) I did not use all habitat categories in this round below (see other loop below)

#Create data.frame containing only factors to allow simple indexing on columns
factors<-survey[,c("phylum_worms","Morpho_diff","NoMorphoDiff","StatMorphoDiff","DiagMorphoDiff","Eco_diff","Geo_diff","Geo_dif3","sympara","symp","allop","larv.pktro","larv.leci","larv.dd","larv.pel","DispAdult","Dispers","HKKv2","HKKv3","Hsubstrate","HDifAccess")]
# factors<-survey[,c("genitals","Morpho_diff","NoMorphoDiff","StatMorphoDiff","DiagMorphoDiff","Eco_diff","Geo_diff","Geo_dif3","sympara","symp","allop","larv.pktro","larv.leci","larv.dd","larv.pel","DispAdult","Dispers","HKKv2","HKKv3","Hsubstrate","HDifAccess")]
#create matrix to write pvalues (Fisher tests below, chi above: warnings for chisq)
f<-data.frame(matrix(ncol=length(factors),nrow=length(factors)))
row.names(f)= c("phylum_worms","Morpho_diff","NoMorphoDiff","StatMorphoDiff","DiagMorphoDiff","Eco_diff","Geo_diff","Geo_dif3","sympara","symp","allop","larv.pktro","larv.leci","larv.dd","larv.pel","DispAdult","Dispers","HKKv2","HKKv3","Hsubstrate","HDifAccess")
colnames(f) = c("phylum_worms","Morpho_diff","NoMorphoDiff","StatMorphoDiff","DiagMorphoDiff","Eco_diff","Geo_diff","Geo_dif3","sympara","symp","allop","larv.pktro","larv.leci","larv.dd","larv.pel","DispAdult","Dispers","HKKv2","HKKv3","Hsubstrate","HDifAccess")
# row.names(f)= c("genitals","Morpho_diff","NoMorphoDiff","StatMorphoDiff","DiagMorphoDiff","Eco_diff","Geo_diff","Geo_dif3","sympara","symp","allop","larv.pktro","larv.leci","larv.dd","larv.pel","DispAdult","Dispers","HKKv2","HKKv3","Hsubstrate","HDifAccess")
# colnames(f) = c("genitals","Morpho_diff","NoMorphoDiff","StatMorphoDiff","DiagMorphoDiff","Eco_diff","Geo_diff","Geo_dif3","sympara","symp","allop","larv.pktro","larv.leci","larv.dd","larv.pel","DispAdult","Dispers","HKKv2","HKKv3","Hsubstrate","HDifAccess")
#transform in factor the HKK factor which appeared a integer
factors$HKKv2<-factor(factors$HKKv2)

options(scipen=999) # to prevent scientific notation (exponential) 
# because this notation interfered with addition of the * for signigficant values (below)

for (i in 1:(length(f)-1))
  for (j in (i+1):length(f)){
    f[i,j]<-chisq.test(factors[,i],factors[,j])[3]  #write above diagonal (NB many warnings l'approximation du Chi-2 est peut-?tre incorrecte)
    f[j,i]<-fisher.test(factors[,i],factors[,j],simulate.p.value = TRUE)[1] #write below
    f[i,j] <-round(f[i,j],digits=4)
    f[j,i] <-round(f[j,i],digits=4)
    }
f
for (i in 1:(length(f)))
  for (j in 1:length(f))
      if (is.na(f[i,j])==F) {
      if (f[i,j]< 0.001) {f[i,j]<-paste0(as.character(f[i,j]),"***")}
      else if (f[i,j]< 0.01) {f[i,j]<-paste0(as.character(f[i,j]),"**")}
      else if (f[i,j]< 0.05) {f[i,j]<-paste0(as.character(f[i,j]),"*")}
    }
f    
 write.csv(f,file="977NS_Fisher_chi_p_values_and_HKKv3.csv")
# write.csv2(f,file="670NS_Fisher_chi_p_values_genitals.csv")

## (2) With distinct habitats: to see relationships between geo_dif (or eco_dif or morpho_dif) with habitats  did not use all habitat categories in this round below (see other loop below)
 
 #Create data.frame containing only factors to allow simple indexing on columns
 factors<-survey[,c("phylum_worms","Morpho_diff","NoMorphoDiff","StatMorphoDiff","DiagMorphoDiff","Eco_diff","Geo_diff","Geo_dif3","sympara","symp","allop","Hsubstrate","HDifAccess","Hcave","Hintertidal","Hestuarine","Hcoralreef","Hseagrass","Hcoastal","Hdeepsea","Hpelagic")]
 #create matrix to write pvalues (Fisher tests below, chi above: warnings for chisq)
 f<-data.frame(matrix(ncol=length(factors),nrow=length(factors)))
 row.names(f)= c("phylum_worms","Morpho_diff","NoMorphoDiff","StatMorphoDiff","DiagMorphoDiff","Eco_diff","Geo_diff","Geo_dif3","sympara","symp","allop","Hsubstrate","HDifAccess","Hcave","Hintertidal","Hestuarine","Hcoralreef","Hseagrass","Hcoastal","Hdeepsea","Hpelagic")
 colnames(f)= c("phylum_worms","Morpho_diff","NoMorphoDiff","StatMorphoDiff","DiagMorphoDiff","Eco_diff","Geo_diff","Geo_dif3","sympara","symp","allop","Hsubstrate","HDifAccess","Hcave","Hintertidal","Hestuarine","Hcoralreef","Hseagrass","Hcoastal","Hdeepsea","Hpelagic")
 
 options(scipen=999) # to prevent scientific notation (exponential) 
 # because this notation interfered with addition of the * for signigficant values (below)
 
 for (i in 1:(length(f)-1))
   for (j in (i+1):length(f)){
     f[i,j]<-chisq.test(factors[,i],factors[,j])[3]  #write above diagonal (NB many warnings l'approximation du Chi-2 est peut-?tre incorrecte)
     f[j,i]<-fisher.test(factors[,i],factors[,j],simulate.p.value = TRUE)[1] #write below
     f[i,j] <-round(f[i,j],digits=4)
     f[j,i] <-round(f[j,i],digits=4)
   }
 f
 for (i in 1:(length(f)))
   for (j in 1:length(f))
     if (is.na(f[i,j])==F) {
       if (f[i,j]< 0.001) {f[i,j]<-paste0(as.character(f[i,j]),"***")}
       else if (f[i,j]< 0.01) {f[i,j]<-paste0(as.character(f[i,j]),"**")}
       else if (f[i,j]< 0.05) {f[i,j]<-paste0(as.character(f[i,j]),"*")}
     }
 f    
 write.csv(f,file="977NS_Fisher_chi_p_values_Habitats.csv")
 

# NB : Penser a refaire ci-dessus avec autre calcul de HKKv3 (sans les cas ambigus pour H22, coastal demersal je crois, cf elseif)

# For significant combinations of factors, can print tables to check effect direction : 

# ecological differentiation
table(survey$Eco_diff,survey$allop)      # ** more ecodiff for CS not allopatric *** (competition exclusion ? but not found for symp or sympara)
table(survey$Geo_diff,survey$Eco_diff )  # *
table(survey$Geo_dif3,survey$Eco_diff )  # p=0.11
table(survey$Eco_diff,survey$HDifAccess) # less ecodiff for CS in difficult to access habitats (study bias?) *

# morphological differentiation
table(survey$Geo_dif3,survey$Morpho_diff)     # ** 
table(survey$DiagMorphoDiff,survey$allop)     # *** Less DiagMorpho when allopatry, more DiagMorpho in cases with only sympatry
table(survey$NoMorphoDiff,survey$Hsubstrate)  # ** more cases without morphodiff in sediment than rocky substrate
table(survey$Morpho_diff,survey$Hsubstrate)   # ** and less cases Diag in sediment than rocky
table(survey$StatMorphoDiff,survey$Hsubstrate)   # ** less Stat morphodiff in sediment/rocky
table(survey$NoMorphoDiff,survey$HDifAccess)  # ** more cases without morphodiff (50%) when habitat difficult to access (else: ~20-30%)
table(survey$NoMorphoDiff,survey$Eco_diff)    # p<0.1
table(survey$Morpho_diff,survey$HKKv2)        # ** 
table(survey$NoMorphoDiff,survey$HKKv2)       # ** pas clair surtout du à H25 qui est la catégorie un peu fourre tout à restreindre (cf HKKv3 moins de H25)
table(survey$StatMorphoDiff,survey$HKKv2)     # * plus de cas StatMorphoDiff pour H1000 que pour les petites HKK
table(survey$DiagMorphoDiff,survey$HKKv2)     # * moins de DiagMorphodiff pour H1000,100,25
table(survey$Morpho_diff,survey$HKKv3)        # With HKKv3, Still the same problem as HKKv2  with "25" which interrupts the trend in ratio 'stat/diag'
table(survey$NoMorphoDiff,survey$HKKv3)
table(survey$StatMorphoDiff,survey$HKKv3)
table(survey$DiagMorphoDiff,survey$HKKv3)
table(survey$Morpho_diff,survey$DispAdult)    # Pelagic more often Statistic / Diag or Nod, than demersal

# FAIRE dans sous-dataset avec que des différenciées morphologiquement: proportion de Stat augmente plus régulièrement (sans H25)
# NB : penser a refaire avec autre calcul de HKKv3 (sans les cas ambigus pour H22, coastal demersal je crois, cf elseif)

# other Geo_dif3
table(survey$Geo_dif3,survey$HKKv2) # * Differences mais pas monotones...pas clair 
table(survey$Geo_dif3,survey$HDifAccess) # **  moins de sympatrie pure quand habitat diff accès. ? no comment...

# others HKKv2 (or HKKv3)
table(survey$allop,survey$HKKv2)     # * bcp plus d'allop pour HKK100 et exces (moindre) pour HKK1000, par rapport aux faibles HKK

table(survey$allop)
table(survey$Hsubstrate)
table(survey$hard_skeleton)

allop <-filter(survey, allop==T)
table(allop$Morpho_diff, allop$HKKv3)
table(allop$Morpho_diff, allop$HKKv2)

noallo <-filter(survey, allop==F)
table(noallo$Morpho_diff, noallo$HKKv3)
table(noallo$Morpho_diff, noallo$HKKv2)

sediment <-filter(survey, Hsubstrate=="Sediment") 
table(sediment$Morpho_diff, sediment$HKKv3)
table(sediment$Morpho_diff, sediment$HKKv2)

rock<-filter(survey, Hsubstrate=="Rocky")
table(rock$Morpho_diff, rock$HKKv3)
table(rock$Morpho_diff, rock$HKKv2)

skel <-filter(survey, hard_skeleton=="Y")
table(skel$Morpho_diff, skel$HKKv2)
table(skel$Morpho_diff, skel$HKKv3)

# tables par types précis d'habitat avec Morpho_diff
table(survey$Hcave,survey$Morpho_diff)
table(survey$Hintertidal,survey$Morpho_diff)
table(survey$Hseagrass,survey$Morpho_diff)
table(survey$Hcoralreef,survey$Morpho_diff)
table(survey$Hdeepsea,survey$Morpho_diff)
table(survey$Hpelagic,survey$Morpho_diff)
table(survey$Hcoastal,survey$Morpho_diff)

# tables par types précis d'habitat avec Geo_dif3
table(survey$Hcave,survey$Geo_dif3)
table(survey$Hintertidal,survey$Geo_dif3)
table(survey$Hseagrass,survey$Geo_dif3)
table(survey$Hcoralreef,survey$Geo_dif3)
table(survey$Hdeepsea,survey$Geo_dif3)
table(survey$Hpelagic,survey$Geo_dif3)
table(survey$Hcoastal,survey$Geo_dif3)

# tables par types précis d'habitat avec Eco_diff
table(survey$Hcave,survey$Eco_diff)
table(survey$Hintertidal,survey$Eco_diff)
table(survey$Hseagrass,survey$Eco_diff)
table(survey$Hcoralreef,survey$Eco_diff)
table(survey$Hdeepsea,survey$Eco_diff)
table(survey$Hpelagic,survey$Eco_diff)
table(survey$Hcoastal,survey$Eco_diff)


# arthro <-filter(survey, phylum_worms=="Arthropoda")
# table(arthro$Morpho_diff, arthro$HKKv2)
# table(arthro$Morpho_diff, arthro$HKKv3)
# 
# echi <-filter(survey, phylum_worms=="Echinodermata")
# table(echi$Morpho_diff, echi$HKKv2)
# table(echi$Morpho_diff, echi$HKKv3)
# 
# mollu<-filter(survey, phylum_worms=="Mollusca")
# table(mollu$Morpho_diff, mollu$HKKv2)
# table(mollu$Morpho_diff, mollu$HKKv3)
# 
# cnid<-filter(survey, phylum_worms=="Cnidaria")
# table(cnid$Morpho_diff, cnid$HKKv2)
# table(cnid$Morpho_diff, cnid$HKKv3)
# 

# then check biological traits versus CS differentiation (morpho, geo or eco) to see the direction of effects evidenced or not in p-value table (above)
# Image (voir avec tests plus complets, glm, pour utiliser covariables filtre sur taille echantillons)
table(survey$Geo_dif3,survey$image,useNA="always")  # CS with image vision are less often sympatric (as predicted)
table(survey$symp,survey$image,useNA="always")      # CS with image vision are less often sympatric
table(survey$allop,survey$image,useNA="always")     # CS with image vision are less often sympatric
fisher.test(survey$symp,survey$image)               # p-value = 0.08
fisher.test(survey$sympara,survey$image)            # p-value = 0.08
fisher.test(survey$allop,survey$image)              # p-value = 0.06143
fisher.test(survey$Geo_dif3,survey$image)           # p-value = 0.02651

table(survey$Morpho_diff,survey$image,useNA="always")
table(survey$DiagMorphoDiff,survey$image)
fisher.test(survey$DiagMorphoDiff,survey$image)   # not significant (but the trend was towards more Diagnostic differences when CS see images)

table(survey$Eco_diff,survey$image,useNA="always")  # NS at all but among CS with image less are Eco_diff than among CS not seeing images

# hard_skeleton
table(survey$Morpho_diff,survey$hard_skeleton,useNA="always")  # * CS with hard skeleton have 4x more time statistic morph diff, 3,3x more time diag diff, 2x more time nodiff 
table(survey$NoMorphoDiff,survey$hard_skeleton,useNA="always") # * CS with hard skeleton are more often morphologically differentiated (either Diag or Stat)
table(survey$Geo_dif3,survey$hard_skeleton,useNA="always")
table(survey$Eco_diff,survey$hard_skeleton,useNA="always")

# ferti
table(survey$Morpho_diff,survey$ferti,useNA="always")    #   
table(survey$NoMorphoDiff,survey$ferti,useNA="always")   #  CS with internal fertilization are more often morphologically differentiated !
table(survey$StatMorphoDiff,survey$ferti,useNA="always") #  CS with internal fertilization are more often statistically differentiated
table(survey$DiagMorphoDiff,survey$ferti,useNA="always") #  But surprisingly it was not significant for Diagnostic difference
table(survey$Geo_dif3,survey$ferti,useNA="always")       #  NOTHING at all...
table(survey$Eco_diff,survey$ferti,useNA="always")       #  NOTHING at all...


# genitals
table(survey$Morpho_diff,survey$genitals,useNA="always")    #   
table(survey$NoMorphoDiff,survey$genitals,useNA="always")   # CS with external genitals are more often morphologically differentiated !
table(survey$StatMorphoDiff,survey$genitals,useNA="always") # CS with external genitals are more often statistically differentiated !
table(survey$Geo_dif3,survey$genitals,useNA="always")       # NOTHING at all...
table(survey$Eco_diff,survey$genitals,useNA="always")       # NOTHING at all...


# Here: other significant interactions with biological traits (from file with contingency table tests)

table(survey$HKKv3, survey$image)     # * mais pas de trend montone du tout
table(survey$larv.leci, survey$image)  # ***
table(survey$larv.pktro, survey$image)  # 

table(survey$larv.pktro, survey$hard_skeleton)  # *
table(survey$HKKv2, survey$hard_skeleton)  # ***
table(survey$HKKv3, survey$hard_skeleton)  #
table(survey$Hsubstrate, survey$hard_skeleton)  # **

table(survey$larv.leci, survey$genitals) # *** les esp?ces des classes avec genitals externes n'ont presque JAMAIS de larve lecithotrophes !
table(survey$larv.dd, survey$genitals)   # ***
table(survey$DispAdult, survey$genitals) # *** les esp?ces p?lagiques ont bcp plus souvent des genitals externes (niveau classe)
table(survey$Dispers, survey$genitals) # *** les esp ? dispersion moyenne ont plus svt des genital externes que les extremes (biais phylogenet probable)
table(survey$HKKv2, survey$genitals) # ***  pas monotone
table(survey$HKKv3, survey$genitals) # *** 
table(survey$HDifAccess, survey$genitals) # *** 

table(survey$larv.pktro, survey$ferti) # ** les esp?ces ? larve planktotrophe ont moins souvent f?condation interne ! oui ! !
table(survey$larv.leci, survey$ferti) # *** les esp?ces ? larve lecithotrophe ont moins souvent f?condation interne ! oui ! !
table(survey$larv.dd, survey$ferti)   # *** species with direct ev have more often internal fertilization ! of course
table(survey$DispAdult, survey$ferti) # *** les esp?ces p?lagiques ont plus souvent fecondation interne que les demersales
table(survey$Dispers, survey$ferti) # *** les esp ? dispersion moyenne
table(survey$HKKv2, survey$ferti) # ***  pas monotone
table(survey$HKKv3, survey$ferti) # *** pas monotone
table(survey$HDifAccess, survey$ferti) # *** plus H facile d'acces (3 modes), plus de cas a ferti interne


# E- Range sizes: Do they differ for biological spp. susceptible to have CS ?####
# subset with range size data
da <-filter(survey, is.na(range_CHull_r100)==F)  #292 cases

#Below I removed 2 cases with 29 or 30 CS/cpx seemes outliers and impeeded to see well the other cases with reasonable Nbs of CS
# but not very justified to remove them in the long term (for statistical confirmations)
da<-filter(da, Nb_CS<29) # remove 2 "outliers" with many CS identified (and Nig values)

# selection of variable amount of data based on number of sites genetically analysed 
hist(da$Nsg, breaks=100)

da5<-filter(da, Nsg>4)  # 155 cases with at least 5 sites analysed in genetics
da6<-filter(da, Nsg>5)  # 135 cases .... 6 ....
da7<-filter(da, Nsg>6)  # 124 cases .... 7 ....
da8<-filter(da, Nsg>7)  # 108 cases .... 8 ....
da9<-filter(da, Nsg>8)  # 100 cases .... 9 ....
da10<-filter(da, Nsg>9)  # 90 cases .... 10....

table(da$Geo_dif3)           # 117 Allo + 91 Sympat + 81 both
table(da10$Geo_dif3)         # 32 Allo +  24 Sympat +  31 both
table(da8$Geo_dif3)          # 39 Allo + 28 Sympat + 38 both
table(da7$Geo_dif3)          # 45 Allo + 31 Sympat + 45 both
tapply(da8$range_CHull_r100, da8$Geo_dif3, mean) # 74 M pour sympat, 77 allo,  92 both
tapply(da7$range_CHull_r100, da7$Geo_dif3, mean) # 72.4 M pour sympat, 72 allo,  88 both.

table(da$Morpho_diff)        # 41 Diagnostic + 70 Statistic + 38-40 Not differentiated
table(da$Morpho_diff,da$Geo_dif3)
#                       Allopatric  Sympatric     Sympatric, Allopatric
# Diagnostic                 13        20                     8
# No differentiation         13         9                    15-17
# Statistic                  31        20                    19

plot(da$Nig,da$Nsg)
summary(da$Nig)
summary(da5$Nig)

# PLOTS range size/ sympatry allopatry / Nb_CS...

# da$allop <-str_detect(da$Geo_diff,"Allopatric")
ggplot(da,aes(x=Nb_CS,y=range_CHull_r100,colour=Geo_dif3, size=Nsg))+geom_point()+geom_smooth(method=lm,se=T, fullrange=TRUE)
ggplot(da,aes(x=Nb_CS,y=range_CHull_r100,colour=Geo_dif3, size=Nsg))+geom_point()+geom_smooth(method=lm,se=F, fullrange=TRUE)
tapply(da$range_CHull_r100/1000000,da$Geo_dif3,mean_se)

ggplot(da10,aes(x=Nb_CS,y=range_CHull_r100,colour=Geo_dif3, size=Nsg))+geom_point()+geom_smooth(method=lm,se=T, fullrange=TRUE)
ggplot(da10,aes(x=Nb_CS,y=range_CHull_r100,colour=Geo_dif3, size=Nsg))+geom_point()+geom_smooth(method=lm,se=F, fullrange=TRUE)
tapply(da10$range_CHull_r100/1000000,da10$Geo_dif3,mean_se)

ggplot(da9,aes(x=Nb_CS,y=range_CHull_r100,colour=Geo_dif3, size=Nsg))+geom_point()+geom_smooth(method=lm,se=F, fullrange=TRUE)
tapply(da9$range_CHull_r100/1000000,da9$Geo_dif3,mean_se)

ggplot(da8,aes(x=Nb_CS,y=range_CHull_r100,colour=Geo_dif3, size=Nsg))+geom_point()+geom_smooth(method=lm,se=F, fullrange=TRUE)
tapply(da8$range_CHull_r100/1000000,da8$Geo_dif3,mean_se)

ggplot(da7,aes(x=Nb_CS,y=range_CHull_r100,colour=Geo_dif3, size=Nsg))+geom_point()+geom_smooth(method=lm,se=F, fullrange=TRUE)
tapply(da7$range_CHull_r100/1000000,da7$Geo_dif3,mean_se)

ggplot(da6,aes(x=Nb_CS,y=range_CHull_r100,colour=Geo_dif3, size=Nsg))+geom_point()+geom_smooth(method=lm,se=F, fullrange=TRUE)
tapply(da6$range_CHull_r100/1000000,da6$Geo_dif3,mean_se)

ggplot(da5,aes(x=Nb_CS,y=range_CHull_r100,colour=Geo_dif3, size=Nsg))+geom_point()+geom_smooth(method=lm,se=F, fullrange=TRUE)
tapply(da5$range_CHull_r100/1000000,da5$Geo_dif3,mean_se)

#voir si effet de Nsg fort sur Nb_CS
ggplot(da,aes(x=Nsg,y=Nb_CS,colour=Geo_dif3))+geom_point()+geom_smooth(method=lm,se=F, fullrange=TRUE)

da_bis<-filter(da, Nb_CS<29) # remove 2 "outliers" with many CS identified (and Nig values)
ggplot(da_bis,aes(x=Nb_CS,y=range_CHull_r100,colour=Geo_dif3, size=Nsg))+geom_point()+geom_smooth(method=lm,se=F, fullrange=TRUE)
tapply(da_bis$range_CHull_r100, da_bis$Geo_dif3,mean) # 87 Allo, 67 Symp, 74 both

# Taille moyenne de range size / espece pour Nominalsp without CS, 
# et ensuite comparer a celles avec CS/ Nb_CS (seulement pour Nig>), Nsg>9
dati<-filter(nomsp, is.na(CHull_r100)==F)
datiNocs<-filter(dati, hasCS==F)
mean(datiNocs$CHull_r100) # ~48 millions de km2 chez les NS without CS

# da<-filter(da,Nig>24)
mean(da$range_CHull_r100/da$Nb_CS) # 
mean(da$range_CHull_r100/da$Nb_CS)/mean(datiNocs$CHull_r100)  # avant raréfaction je crois que c'était 2 maintenant c'est 0.63 
dallo<-filter(da, Geo_diff=="Allopatric")
mean(dallo$range_CHull_r100/dallo$Nb_CS)# 37 millions km2 chez les allopatric_CS (par BS!)
dasym<-filter(da, Geo_diff=="Sympatric")
mean(dasym$range_CHull_r100)  # 67 millions km2 chez les sympatric_CS


# F- Number of CS per complex: glm to explain it ####
options(scipen=0, digits=7) # to reset scientific writing of numbers options(scipen=0, digits=7)

# 1- STUDY EFFORT
glm<-glm(Nb_CS~Nsg, data=survey, family="poisson")
summary(glm)
drop1(glm, test="Chisq") # *** !
mean(survey$Nb_CS/survey$Nsg, na.rm=T) # 0.6 CS / site genetically characterized in average but in regression model
glm<-glm(Nb_CS~Nsg+0, data=survey, family="poisson")
summary(glm) # coeff Nsg = 0.036 in log regression means that ... I will do a linear model for this.
summary(lm(Nb_CS-1~Nsg+0, data=survey)) # coeff= 0.15 ==> environ + 1.5 CS pour 10 sites ajout?s (NB constante = il faut soustraite 1 ? la variable ? expliquer en ce cas, car toujours une esp?ce au d?part!)
summary(lm(Nb_CS-1~Nig+0, data=survey)) # coeff= 0.004 ==> environ + 1 CS pour 250 individus ajout?s en g?n?tique
# remark: analysis by individuals less biased by the fact that some CS contain sympatric species
summary(lm(Nb_CS-1~Nig+Nsg+0, data=survey)) # then only Nsg ***, Nig Not significant (too correlated with Nsg)


# 2- TAXONOMY and BIOLOGICAL traits ####

glm1<-glm(Nb_CS~phylum_worms, data=survey, family="poisson")
summary(glm1)
drop1(glm1, test="Chisq") # *
glm1<-glm(Nb_CS~phylum_worms+Nsg, data=survey, family="poisson")
summary(glm1)
drop1(glm1, test="Chisq") # ** and ***

glm2<-glm(Nb_CS~phylum_class, data=survey, family="poisson")
summary(glm2)
drop1(glm2, test="Chisq") # *
glm2<-glm(Nb_CS~phylum_class+Nsg, data=survey, family="poisson")
summary(glm2)
drop1(glm2, test="Chisq") # ** and ***


glm<-glm(Nb_CS~ferti, data=survey, family="poisson")
summary(glm)
drop1(glm, test="Chisq") #
tapply(survey$Nb_CS, survey$ferti,mean, na.rm=T) # more CS/cpx in classes with both ferti types, less in internal fertilization classes but Not significant (or #)
glm<-glm(Nb_CS~ferti+Nsg, data=survey, family="poisson")
summary(glm)
drop1(glm, test="Chisq") # ferti# Nsg***

glm<-glm(Nb_CS~hard_skeleton, data=survey, family="poisson")
summary(glm) # NS
glm<-glm(Nb_CS~hard_skeleton+Nsg, data=survey, family="poisson")
summary(glm) # NS and ***
tapply(survey$Nb_CS, survey$hard_skeleton,mean, na.rm=T) #
glm<-glm(Nb_CS~phylum_worms+ hard_skeleton +Nsg, data=survey, family="poisson")
summary(glm) # now hard_skeleton nearly significant only (when phylum in model)

glm<-glm(Nb_CS~image, data=survey, family="poisson")
summary(glm)# *
glm<-glm(Nb_CS~image + Nsg, data=survey, family="poisson")
summary(glm)# * and *** (thus image less significant than phylum_class so NOT really an effect of the vision itself ?)
tapply(survey$Nb_CS, survey$image,mean, na.rm=T) # 3.07 No image, 3.41 with image vision (contrary to prediction !)
glm<-glm(Nb_CS~phylum_worms + image + Nsg, data=survey, family="poisson") # now image not significant (with phylum)
summary(glm)#


glm<-glm(Nb_CS~genitals, data=survey, family="poisson")
summary(glm)
drop1(glm, test="Chisq") # *** !
glm<-glm(Nb_CS~genitals+Nsg, data=survey, family="poisson")
summary(glm)
drop1(glm, test="Chisq") # *** and ***
tapply(survey$Nb_CS, survey$genitals,mean, na.rm=T) #4.16 (both) > 3.08 (no)=3.09 (yes: genitalia external)

# 3- MORPHO, GEO and ECOlogical differentiation ####

# single factor  (with or without covariable Nsg)

glm3<-glm(Nb_CS~Geo_dif3, data=survey, family="poisson") 
summary(glm3)
drop1(glm3, test="Chisq") #***
tapply(survey$Nb_CS, survey$Geo_dif3,mean, na.rm=T) #

glm3<-glm(Nb_CS~Geo_dif3+Nsg+Nsm, data=survey, family="poisson") 
summary(glm3)
drop1(glm3, test="Chisq") # ***,***,*
tapply(survey$Nb_CS, survey$Geo_dif3,mean, na.rm=T)  # 2.8 Allo,  2.6 Symp <  4.9 both Symp & Allo

glm<-glm(Nb_CS~Morpho_diff, data=survey, family="poisson") 
summary(glm)
drop1(glm, test="Chisq") #***

glm<-glm(Nb_CS~Morpho_diff+Nsg+Nsm, data=survey, family="poisson") 
summary(glm)
drop1(glm, test="Chisq") #***
tapply(survey$Nb_CS, survey$Morpho_diff,mean, na.rm=T) # No_dif=4 > Stat=2.94, Diag=2.8

glm<-glm(Nb_CS~Eco_diff, data=survey, family="poisson") 
summary(glm)
drop1(glm, test="Chisq") # NS

# several factors  (with or without covariable Nsg)
glm<-glm(Nb_CS~Morpho_diff+Geo_dif3, data=survey, family="poisson") 
summary(glm)
drop1(glm, test="Chisq") # ***, ***
glm<-glm(Nb_CS~Morpho_diff*Geo_dif3, data=survey, family="poisson") 
summary(glm)
drop1(glm, test="Chisq") # *
glm<-glm(Nb_CS~Morpho_diff+Geo_dif3+Nsg, data=survey, family="poisson") 
summary(glm)
drop1(glm, test="Chisq") # ***, ***,***
glm<-glm(Nb_CS~Morpho_diff*Geo_dif3+Nsg, data=survey, family="poisson") 
summary(glm)
drop1(glm, test="Chisq") # **, *** (Nsg in model => increased significance for morpho*geo )

survey$geo_morpho <-factor(paste(survey$Geo_dif3,survey$Morpho_diff))
geomor <-filter(survey, is.na(Geo_dif3)==F & is.na(Morpho_diff)==F)
geomor$geo_morpho<-factor(geomor$geo_morpho) # to eliminate obsolete levels  with some NA
tapply(geomor$Nb_CS, geomor$geo_morpho, mean, na.rm=T) # max 6.5 (Sym&Allo, Nodiff), min 2.41 (Symp, statistic)
table(geomor$Morpho_diff, geomor$Geo_dif3)# sample sizes OK for all pairs
# Je ne sais pas comment interpréter ceci donc je ne suis pas inspirée pour tester d'hypothèse...  Je pense que c'est possiblement consequence de choix humains ou autres biais d'efforts 

glm<-glm(Nb_CS~Morpho_diff+Eco_diff, data=survey, family="poisson") 
summary(glm)
drop1(glm, test="Chisq") # NS, NS

glm<-glm(Nb_CS~Geo_dif3+Eco_diff, data=survey, family="poisson") 
summary(glm)
drop1(glm, test="Chisq") # *, NS


# G- Morpho_diff: which variables influence it ? ####
# (Hyp.1: neutral-> Ne | range size | habitat carrying capacity; H2: substrate, class...)

summary(survey$CHull_NotRarefied, na.rm=T) # smaller values because no selection/ rarefaction
summary(survey$range_CHull_r100, na.rm=T)  # 378 NA /670 (292 observations with data rarefied, nearly half)

# Range size en fonction de type de differentiation morpho (abonder dans le sens de l'hypothese neutraliste si confirme range > chez vraies CSss)
tapply(survey$range_CHull_r100/survey$Nb_CS,survey$Morpho_diff, mean,na.rm=T)
# Diagnostic   No differentiation        Statistic 
# 19,894,168           35,445,482         22,580,164 

## NB: BEFORE rarefaction, we had a distinct ranking that supported neutral morphological evolution:
#tapply(survey$CHull_NotRarefied/survey$Nb_CS,survey$Morpho_diff, mean,na.rm=T)
## Diagnostic  No differentiation       Statistic 
## 96.37148           79.86309           94.33095 
sur50sites <-filter(survey, NbSitesObis_beforeRaref>49)
tapply(sur50sites$CHull_NotRarefied/sur50sites$Nb_CS,sur50sites$Morpho_diff, mean,na.rm=T)
# Diagnostic No differentiation          Statistic 
# 117.64867           98.51073          102.67402 
sur30sites <-filter(survey, NbSitesObis_beforeRaref>29)
tapply(sur30sites$CHull_NotRarefied/sur30sites$Nb_CS,sur30sites$Morpho_diff, mean,na.rm=T)
# Diagnostic No differentiation          Statistic 
# 113.1253            93.6819           101.4547 
sur100sites <-filter(survey, NbSitesObis_beforeRaref>99)
tapply(sur100sites$CHull_NotRarefied/sur100sites$Nb_CS,sur100sites$Morpho_diff, mean,na.rm=T)
# Diagnostic No differentiation          Statistic 
# 113.78413           88.26504          106.67381  !!!! even with 100 sites, the ranking does not change and differs from the r100 ! 
tapply(survey$NbSitesObis_beforeRaref, survey$Morpho_diff,mean,na.rm=T)
# Diagnostic No differentiation          Statistic 
# 539.5059           714.8795          1629.6410  # species with statistic diff have much more OBIS sites (effort study or really range?)
tapply(sur100sites$NbSitesObis_beforeRaref, sur100sites$Morpho_diff,mean,na.rm=T)
# Diagnostic No differentiation          Statistic 
# 1067.167           1490.128           2742.449 
tapply(survey$CHull_NotRarefied/survey$NbSitesObis_beforeRaref, survey$Morpho_diff,mean,na.rm=T)
# Diagnostic No differentiation          Statistic 
# 8.340143           5.853580           5.606008 

sur29 <-filter(survey, Nig>29)
tapply(sur29$range_CHull_r100/sur29$Nb_CS,sur29$Morpho_diff, mean,na.rm=T)
# Diagnostic No differentiation          Statistic 
# 19444988           32754875           21026146


#### glm to explain type of morpho differentiation ####

### HKK (proxy of Ne, quantitative) ### It has significant effect on proba of having statistical morphological differentiation

glm<-glm(StatMorphoDiff~HKKv2, survey, family="binomial")
summary(glm) #HKKv3 p=0.018 ** ! HKK variable continue donc d?pend des valeurs choisies a priori j'imagine !

glm<-glm(StatMorphoDiff~HKKv3, survey, family="binomial")
summary(glm) #HKKv3 p=0.02 ** ! HKK variable continue donc d?pend des valeurs choisies a priori j'imagine !

glm<-glm(StatMorphoDiff~HKKv2 + Hsubstrate + allop, survey, family="binomial")
summary(glm) #HKKv2 p=0.0882

glm<-glm(StatMorphoDiff~HKKv3 + Hsubstrate + allop, survey, family="binomial")
summary(glm) #HKKv3 p=0.0742

glm<-glm(StatMorphoDiff~HKKv3 + Hsubstrate + Geo_dif3, survey, family="binomial")
summary(glm) #HKKv3 p=0.0707

glm<-glm(DiagMorphoDiff~HKKv2, survey, family="binomial")
summary(glm) # NS
glm<-glm(DiagMorphoDiff~HKKv3, survey, family="binomial")
summary(glm) # NS

glm<-glm(NoMorphoDiff~HKKv2, survey, family="binomial")
summary(glm) # NS
glm<-glm(NoMorphoDiff~HKKv3, survey, family="binomial")
summary(glm) # NS

glm<-glm(DiagMorphoDiff~ HKKv2 + Hsubstrate + allop, survey, family="binomial")
summary(glm) #HKKv2 p=0.0541
glm<-glm(DiagMorphoDiff~ HKKv3 + Hsubstrate + allop, survey, family="binomial")
summary(glm) #HKKv2 p=0.0675, allop # aussi

#  bestglm ####

survey$rangeperCS<-survey$range_CHull_r100/survey$Nb_CS# existait deja= corrange !
# ci dessous les modeles avec range sont tous selectionnes par besglm mais cette variable reduit le dataset (bcp de NA).
#cependant, meme avant de l'utiliser on avait les memes variables selectionnees sauf range size. ne change rien au reste des modeles...

mat <-select(survey,c(Geo_dif3,Hsubstrate,phylum_worms,Nsg,Nsm,hard_skeleton, image, genitals, 
                      HDifAccess,HKKv3,StatMorphoDiff))
b <-bestglm(mat, IC="BIC")
b$BestModels


mat <-select(survey,c(Geo_dif3,allop,Hsubstrate,phylum_worms,HDifAccess,HKKv2, HKKv3,rangeperCS,StatMorphoDiff))
b <-bestglm(mat, IC="BIC")
b$BestModels
# Geo_dif3 allop Hsubstrate phylum_worms HDifAccess HKKv2 HKKv3 rangeperCS Criterion
# 1    FALSE FALSE       TRUE        FALSE      FALSE FALSE  TRUE       TRUE -2552.709
# 2    FALSE  TRUE       TRUE        FALSE      FALSE FALSE  TRUE       TRUE -2551.426
# 3     TRUE FALSE       TRUE        FALSE      FALSE FALSE  TRUE       TRUE -2550.294
# 4    FALSE FALSE       TRUE        FALSE      FALSE  TRUE  TRUE       TRUE -2546.202
# 5    FALSE  TRUE       TRUE        FALSE      FALSE  TRUE  TRUE       TRUE -2544.919
summary(glm(StatMorphoDiff~Hsubstrate+HKKv3, data= survey, family="binomial")) # Best model but NOt significant variables
a <-bestglm(mat, IC="AIC")
a$BestModels
# Geo_dif3 allop Hsubstrate phylum_worms HDifAccess HKKv2 HKKv3 rangeperCS Criterion
# 1    FALSE  TRUE       TRUE         TRUE       TRUE FALSE  TRUE       TRUE -2818.103
# 2    FALSE  TRUE       TRUE         TRUE      FALSE FALSE  TRUE       TRUE -2817.484
# 3    FALSE  TRUE       TRUE         TRUE       TRUE  TRUE  TRUE       TRUE -2816.103
# 4    FALSE  TRUE       TRUE         TRUE      FALSE  TRUE  TRUE       TRUE -2815.484
# 5     TRUE  TRUE       TRUE         TRUE       TRUE FALSE  TRUE       TRUE -2814.992
# > 
mat <-select(survey,c(Geo_dif3,allop,Hsubstrate,phylum_worms,HDifAccess,HKKv2, HKKv3,rangeperCS,DiagMorphoDiff))
b <-bestglm(mat, IC="BIC")
b$BestModels
# Geo_dif3 allop Hsubstrate phylum_worms HDifAccess HKKv2 HKKv3 rangeperCS Criterion
# 1    FALSE  TRUE       TRUE        FALSE       TRUE FALSE  TRUE       TRUE -2762.321
# 2    FALSE  TRUE       TRUE        FALSE       TRUE  TRUE  TRUE       TRUE -2755.813
# 3    FALSE  TRUE       TRUE        FALSE      FALSE FALSE  TRUE       TRUE -2755.355
# 4     TRUE FALSE       TRUE        FALSE       TRUE FALSE  TRUE       TRUE -2750.396
# 5     TRUE  TRUE       TRUE        FALSE       TRUE FALSE  TRUE       TRUE -2750.344

mat <-select(survey,c(Geo_dif3,allop,Hsubstrate,phylum_worms,HDifAccess,HKKv2, HKKv3,rangeperCS,NoMorphoDiff))
b <-bestglm(mat, IC="BIC")
b$BestModels
# Geo_dif3 allop Hsubstrate phylum_worms HDifAccess HKKv2 HKKv3 rangeperCS Criterion
# 1     TRUE FALSE       TRUE         TRUE      FALSE FALSE  TRUE       TRUE -2945.946
# 2    FALSE FALSE       TRUE         TRUE      FALSE FALSE  TRUE       TRUE -2945.944
# 3    FALSE  TRUE       TRUE         TRUE      FALSE FALSE  TRUE       TRUE -2939.900
# 4     TRUE  TRUE       TRUE         TRUE      FALSE FALSE  TRUE       TRUE -2939.869
# 5     TRUE FALSE       TRUE         TRUE      FALSE  TRUE  TRUE       TRUE -2939.438

survey$rangeperCS<-survey$range_CHull_r100/survey$Nb_CS

morphodif <-filter(survey, NoMorphoDiff==F) # masi l? on perd beaucoup de donn?es !
mat <-select(morphodif,c(Geo_dif3,allop,Hsubstrate,phylum_worms,HDifAccess,HKKv2, HKKv3,rangeperCS,StatMorphoDiff))
b <-bestglm(mat, IC="BIC")
b$BestModels
# Geo_dif3 allop Hsubstrate phylum_worms HDifAccess HKKv2 HKKv3 range_CHull_r100 Criterion
# 1    FALSE  TRUE       TRUE        FALSE      FALSE FALSE  TRUE             TRUE -907.4450
# 2    FALSE  TRUE       TRUE        FALSE      FALSE  TRUE  TRUE             TRUE -901.8283
# 3    FALSE FALSE       TRUE        FALSE      FALSE FALSE  TRUE             TRUE -900.8587
# 4     TRUE FALSE       TRUE        FALSE      FALSE FALSE  TRUE             TRUE -898.6527
# 5    FALSE  TRUE       TRUE        FALSE       TRUE FALSE  TRUE             TRUE -897.7506
a <-bestglm(mat, IC="AIC")
a$BestModels # le meme qu'avec BIC: allop + Hsubstrate + HKKv3 mais presque aussi bon sans allop
# Geo_dif3 allop Hsubstrate phylum_worms HDifAccess HKKv2 HKKv3 range_CHull_r100 CriterionGeo_dif3 allop Hsubstrate phylum_worms HDifAccess HKKv2 HKKv3 range_CHull_r100 Criterion
# 1    FALSE  TRUE       TRUE        FALSE      FALSE FALSE  TRUE             TRUE -921.9121
# 2    FALSE  TRUE       TRUE         TRUE      FALSE FALSE  TRUE             TRUE -920.6706
# 3     TRUE  TRUE       TRUE         TRUE      FALSE FALSE  TRUE             TRUE -920.2925
# 4    FALSE  TRUE       TRUE        FALSE      FALSE  TRUE  TRUE             TRUE -919.9121
# 5    FALSE  TRUE       TRUE        FALSE       TRUE FALSE  TRUE             TRUE -919.4512

glm<-glm(StatMorphoDiff~HKKv3 + Hsubstrate + range_CHull_r100, survey, family="binomial")
summary(glm)  # NS tout ! 
glm<-glm(StatMorphoDiff~HKKv3 +range_CHull_r100, survey, family="binomial")
summary(glm) # range **, HKKv3 * mais coeff de range negatif
glm<-glm(StatMorphoDiff~HKKv3 +(range_CHull_r100/Nb_CS), survey, family="binomial")
summary(glm) # hKK** mais range considere interaction pas division
glm<-glm(StatMorphoDiff~HKKv3 +rangeperCS, survey, family="binomial")
summary(glm) # HKK*, range NS (mais coeff negatif)
glm<-glm(StatMorphoDiff~HKKv3 +rangeperCS + Hsubstrate, survey, family="binomial")
summary(glm) # NS mais substrate p=0.108
glm<-glm(StatMorphoDiff~HKKv3 + Hsubstrate, data=survey, family="binomial")
summary(glm)  # NS tout mais p(HKKv3)=0.10

glm<-glm(StatMorphoDiff~HKKv3 +rangeperCS + Hsubstrate, data=morphodif, family="binomial")
summary(glm) # NS

#rapides tests RANGES/ lm mais modele pas adapte car distribution pas Gaussienne  ####
hist(survey$range_CHull_r100)
summary(sur29$range_CHull_r100)  # 346-176=170 observations
summary(lm(sur29$range_CHull_r100/sur29$Nb_CS~sur29$Morpho_diff))
drop1(lm(sur29$range_CHull_r100/sur29$Nb_CS~sur29$Morpho_diff),test="F")
summary(lm(sur29$range_CHull_r100/sur29$Nb_CS~sur29$StatMorphoDiff))  # NS
summary(lm(sur29$range_CHull_r100/sur29$Nb_CS~sur29$NoMorphoDiff))    # p=0.0524 .
summary(lm(sur29$range_CHull_r100/sur29$Nb_CS~sur29$DiagMorphoDiff))  # NS

summary(lm(survey$range_CHull_r100/survey$Nb_CS~survey$DiagMorphoDiff))  # NS
summary(lm(survey$range_CHull_r100/survey$Nb_CS~survey$StatMorphoDiff))  # NS
summary(lm(survey$range_CHull_r100/survey$Nb_CS~survey$NoMorphoDiff))  # ** larger ranges when NoMorphoDiff

summary(lm(survey$range_CHull_r100/survey$Nb_CS ~ survey$Nsg + survey$NoMorphoDiff))   # ***NoMorphoDiff
summary(lm(survey$range_CHull_r100/survey$Nb_CS ~ survey$Nsg + survey$StatMorphoDiff)) # p=0.0691 .StatMorphoDiff
summary(lm(survey$range_CHull_r100/survey$Nb_CS ~ survey$Nsg + survey$DiagMorphoDiff)) # ns DiagMorphoDiff
summary(lm(survey$range_CHull_r100/survey$Nb_CS ~ survey$Nsg + survey$Morpho_diff))   # **Nodiff>DIag
summary(lm(survey$range_CHull_r100/survey$Nb_CS ~ survey$Nsg + survey$Morpho_diff+survey$allop))  # **Nodiff>DIag
summary(lm(survey$range_CHull_r100/survey$Nb_CS ~ survey$Nsg + survey$allop))  # ns allop !
summary(lm(survey$range_CHull_r100/survey$Nb_CS ~ survey$Nsg + survey$symp))  # p=0.08 symp (smaller range sympTRUE )

allo <-filter(survey, allop==T )
summary(lm(allo$range_CHull_r100/allo$Nb_CS ~ allo$Nsg + allo$Morpho_diff))

#range divided by Nb_CS only when there is some allopatry (even with sympatry)
survey$corrange[survey$allop==T] <-survey$range_CHull_r100[survey$allop==T]/survey$Nb_CS[survey$allop==T]
survey$corrange[survey$allop==F] <-survey$range_CHull_r100[survey$allop==F]
summary(lm(corrange~Morpho_diff+Nsg, survey))  
tapply(survey$corrange, survey$Morpho_diff, mean, na.rm=T)  
# Diagnostic     No differentiation          Statistic 
# 35,351,581           51,498,277           32,921,566 
# mais ceci corrange est justifie si on se fie a l'idee de geo_diff donc si Nsg est ?lev?. 
# Voir donc dans sub-dataset
hist(survey$Nsg, breaks=50)
sur10s <-filter(survey, Nsg>9)  # 181 obs
summary(sur10s$corrange)      #181-91 NA = 90 obervations with reliable corrange 
table(sur10s$Morpho_diff, is.na(sur10s$corrange)) # 10 Diag, 13 nodif, 31 stat (effectifs un peu faibles)
summary(lm(corrange~Morpho_diff+Nsg, sur10s)) # ns 
summary(lm(corrange~Morpho_diff, sur10s))     # ns
tapply(sur10s$corrange, sur10s$Morpho_diff, mean, na.rm=T)  
# Diagnostic  No differentiation     Statistic  # 
#     37 M       67 M                 21 M


table(da$Morpho_diff,useNA="always") # effectifs corrects 41 38 70 140
# reste vrai que au sein de cs avec diff morpho, statistics ont un plus grand range size (prédiction neutraliste /Ne)
# Mais les non différenciées (CS récentes) ont des ranges doubles ! why ? Cette différence est très grande.
boxplot(da$CHullperCS,da$Morpho_diff, na.rm=T)
dm<-filter(da, is.na(Morpho_diff) ==F)
boxplot(da$CHull_r100~da$Morpho_diff)
vioplot(da$CHull_r100~da$Morpho_diff)
vioplot(da$ChullperCS~da$Morpho_diff,na.rm=T)
hist(da$ChullperCS,breaks=30)
hist(da$CHull_r100,breaks=30)

# AVANT RAREFACTION ON AVAIT AUTRE CHOSE ! donc confirme que la selection des especes tres etudiees n'est pas du tout neutre:
# allait dans le bon sens hypothese neutre! JE NE RETROUVE PAS DU TOUT CE PATRON DANS LES NOUVELLES DONNEES MEME AVANT RAREFACTION
# PEUT ETRE UN SOUS DATASET pour Nig>29 ? ou truc du sytle ?
# Diagnostic No differentiation          Statistic 
# 24026066           20055233           29752879 
# en retirant moins d'etudes (moins stringent sur Nig): ne change pas (etait moins bon mais tres net encore)
# Diagnostic No differentiation          Statistic 
# 22778999           20055233           26741108 

# ##########################################################################################################################
# # OLD kept just in case but redone elsewhere (s.e. could not be used so did s.d. but below OK) #####################
# # Nb of CS per complex compared across phyla or classes
# # ...  but see also (more complete but without s.e.) R.scripts computing missed biological species/zone, class or phylum (more complete tables)
# tapply(survey$Nb_CS, survey$class_worms, mean, na.rm=T)
# tapply(survey$Nb_CS, survey$phylum_worms, mean, na.rm=T)
# survey$phylum_class<-paste(survey$phylum_worms,survey$class_worms)
# tapply(survey$Nb_CS, survey$phylum_class, mean, na.rm=T)
# 
# Nb_CS_phylum_class<-aggregate(survey$Nb_CS,by=list(c(survey$phylum_class)),FUN=mean,na.rm=T)
# se<-aggregate(survey$Nb_CS,by=list(c(survey$phylum_class)),FUN=sd,na.rm=T)
# Nb_CS_phylum_class$Nb_CS.se<-se[,2]
# names(Nb_CS_phylum_class)<-c("Phylum_class", "mean Nb_CS","se Nb_CS")
# write.csv2(Nb_CS_phylum_class, "Nb_CS par NS statistics per phylum_class.csv",row.names=F)
# 
# Nb_CS_phylum<-aggregate(survey$Nb_CS,by=list(c(survey$phylum_worms)),FUN=mean,na.rm=T)
# se<-aggregate(survey$Nb_CS,by=list(c(survey$phylum_worms)),FUN=sd,na.rm=T)
# Nb_CS_phylum$Nb_CS.se<-se[,2]
# names(Nb_CS_phylum)<-c("Phylum", "mean Nb_CS","se Nb_CS")
# write.csv2(Nb_CS_phylum, "Nb_CS par NS statistics per phylum.csv",row.names=F)
