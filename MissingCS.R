#Writing a script to do OBIS code for paper 3 (missing species)

library(dplyr)
library(robis)
library(tidyr)
library(ggplot2)
library(cowplot)

setwd("C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs")

##Part 1: pulling all animal species from OBIS

animalia<-checklist(taxonid=2)   #pulls list of all animals from OBIS
write.csv(animalia, "animal_list_June2024.csv") #writing so I don't have to pull the data every time
animalia<-read.csv("animal_list_with_geo.csv") #read back in if needed
animalspp<-animalia[animalia$taxonRank=="Species",]   #ONLY use those that are ID'd to the species level
animals<-as.data.frame(cbind(animalspp$scientificName,animalspp$phylum,animalspp$class))  #create table of names and phyla
colnames(animals)<-c("Species","Phylum","Class")


##Part2: need polygons for all seas - read in Mark's code
Arctic_coor<-read.csv("Arctic_coor.csv")
Atlantic_coor<-read.csv("Atlantic_coor.csv")
Indian_coor<-read.csv("Indian_coor.csv")
Pacific_coor<-read.csv("Pacific_coor.csv")
SouthOcean_coor<-read.csv("SouthOcean_coor.csv")

##Part3: create checklists for each sea
#Arctic Ocean
num_poly_Arctic<-c(1:length(Arctic_coor[,3])) #Number of polygons to search in
Art_check<-data.frame()

for (i in num_poly_Arctic){
  check<-checklist(taxonid=2, geometry = Arctic_coor[i,3])
  extr<-as.data.frame(cbind(check$scientificName,check$phylum,check$class))  #create table of names and phyla
  Art_check<-rbind(Art_check,extr)
}

colnames(Art_check)<-c("Species","Phylum","Class")
Art_check2 <- Art_check %>% distinct(Species, .keep_all = TRUE)  #Remove duplicates (ie spp found in multiple polygons)


#Indian Ocean
num_poly_Indian<-c(1:length(Indian_coor[,3])) #Number of polygons to search in
Ind_check<-data.frame()

for (i in num_poly_Indian){
  check<-checklist(taxonid=2, geometry = Indian_coor[i,3])
  extr<-as.data.frame(cbind(check$scientificName,check$phylum,check$class))  #create table of names and phyla
  Ind_check<-rbind(Ind_check,extr)
}
colnames(Ind_check)<-c("Species","Phylum","Class")
Ind_check2 <- Ind_check %>% distinct(Species, .keep_all = TRUE)  #Remove duplicates (ie spp found in multiple polygons)

#Southern Ocean
num_poly_South<-c(1:length(SouthOcean_coor[,3])) #Number of polygons to search in
South_check<-data.frame()

for (i in num_poly_South){
  check<-checklist(taxonid=2, geometry = SouthOcean_coor[i,3])
  extr<-as.data.frame(cbind(check$scientificName,check$phylum,check$class))  #create table of names and phyla
  South_check<-rbind(South_check,extr)
}
colnames(South_check)<-c("Species","Phylum","Class")
South_check2 <- South_check %>% distinct(Species, .keep_all = TRUE)  #Remove duplicates (ie spp found in multiple polygons)

#Atlantic Ocean
num_poly_Atlantic<-c(1:length(Atlantic_coor[,3])) #Number of polygons to search in
Atl_check<-data.frame()

for (i in num_poly_Atlantic){
  check<-checklist(taxonid=2, geometry = Atlantic_coor[i,3])
  extr<-as.data.frame(cbind(check$scientificName,check$phylum,check$class))  #create table of names and phyla
  Atl_check<-rbind(Atl_check,extr)
}
colnames(Atl_check)<-c("Species","Phylum","Class")
Atl_check2 <- Atl_check %>% distinct(Species, .keep_all = TRUE)  #Remove duplicates (ie spp found in multiple polygons)

#Pacific Ocean
num_poly_Pacific<-c(1:length(Pacific_coor[,3])) #Number of polygons to search in
Pac_check<-data.frame()

for (i in num_poly_Pacific){
  check<-checklist(taxonid=2, geometry = Pacific_coor[i,3])
  extr<-as.data.frame(cbind(check$scientificName,check$phylum,check$class))  #create table of names and phyla
  Pac_check<-rbind(Pac_check,extr)
}
colnames(Pac_check)<-c("Species","Phylum","Class")
Pac_check2 <- Pac_check %>% distinct(Species, .keep_all = TRUE)  #Remove duplicates (ie spp found in multiple polygons)

# Write out the checklist files
write.csv(Art_check2, "Art_checklist.csv") #write it so I don't have to do it again!
write.csv(Atl_check2, "Atl_checklist.csv") #write it so I don't have to do it again!
write.csv(Ind_check2, "Ind_checklist.csv") #write it so I don't have to do it again!
write.csv(Pac_check2, "Pac_checklist.csv") #write it so I don't have to do it again!
write.csv(South_check2, "South_checklist.csv") #write it so I don't have to do it again!

# And read them back in

Art_check2<-read.csv("Art_checklist.csv")
Atl_check2<-read.csv("Atl_checklist.csv")
Ind_check2<-read.csv("Ind_checklist.csv")
Pac_check2<-read.csv("Pac_checklist.csv")
South_check2<-read.csv("South_checklist.csv")

# For all animal species, need following info:
# CS or no
# Present in each ocean, yes or no
# So read in the CS list

survey <- read.csv(file="977_CScpx_100cols_Habitat&Larva_Completed_20230928_comma.csv"   , header=TRUE )
survey2<-survey %>% drop_na(acceptedName_wormsV1) #Get rid of anything without an accepted name in WORMS

# Loop over the animal list:
# Looking to see if a species is present on the CS list, T/F
# Is in each ocean, 5 calls, T/F
# Wind up with table of Species, Phylum, Class, then those columns.


is_cs<-c()
is_Art<-c()
is_Atl<-c()
is_Ind<-c()
is_Pac<-c()
is_South<-c()

for (i in animalspp$acceptedNameUsage){
  cs<-match(i,survey2$acceptedName_wormsV1,nomatch=FALSE) #match spits out the line number of the match; otherwise returns a zero
  is_cs<-rbind(is_cs,cs)
  Art<-match(i,Art_check2$Species,nomatch=FALSE)
  is_Art<-rbind(is_Art,Art)  
  Atl<-match(i,Atl_check2$Species,nomatch=FALSE)
  is_Atl<-rbind(is_Atl,Atl)
  Ind<-match(i,Ind_check2$Species,nomatch=FALSE)
  is_Ind<-rbind(is_Ind,Ind)
  Pac<-match(i,Pac_check2$Species,nomatch=FALSE)
  is_Pac<-rbind(is_Pac,Pac)
  South<-match(i,South_check2$Species,nomatch=FALSE)
  is_South<-rbind(is_South,South)
  
}

animalspp<-cbind(animalspp,is_cs,is_Art,is_Atl,is_Ind,is_Pac,is_South)
animalspp$is_cs<-animalspp$is_cs>0  #This line turns the numbers (which don't mean anything) into TRUE - means species is present in the zone.
animalspp$is_Art<-animalspp$is_Art>0  #This line turns the numbers (which don't mean anything) into TRUE - means species is present in the zone.
animalspp$is_Atl<-animalspp$is_Atl>0  #This line turns the numbers (which don't mean anything) into TRUE - means species is present in the zone.
animalspp$is_Ind<-animalspp$is_Ind>0  #This line turns the numbers (which don't mean anything) into TRUE - means species is present in the zone.
animalspp$is_Pac<-animalspp$is_Pac>0  #This line turns the numbers (which don't mean anything) into TRUE - means species is present in the zone.
animalspp$is_South<-animalspp$is_South>0  #This line turns the numbers (which don't mean anything) into TRUE - means species is present in the zone.

write.csv(animalspp, "animal_list_with_geo.csv") #write it so I don't have to do it again!

# The numbers in each column don't match up to the checklisted numbers - ie fewer species in an ocean than there are on an ocean's checklist. Why?
# Maybe because I didn't use the step to filter the checklists to only things ID'd as a species
# YES THIS SEEMS TO BE IT (10/26)

# Now I need to know zone info IN the survey table bc I need that Nb_CS column, so we'll repeat the code.
# Same loop as above, just a different input?
# Removing the 'is_cs' bc the answer will always be yes

is_Art<-c()
is_Atl<-c()
is_Ind<-c()
is_Pac<-c()
is_South<-c()

for (i in survey2$acceptedName_wormsV1){
  Art<-match(i,Art_check2$Species,nomatch=FALSE)
  is_Art<-rbind(is_Art,Art)  
  Atl<-match(i,Atl_check2$Species,nomatch=FALSE)
  is_Atl<-rbind(is_Atl,Atl)
  Ind<-match(i,Ind_check2$Species,nomatch=FALSE)
  is_Ind<-rbind(is_Ind,Ind)
  Pac<-match(i,Pac_check2$Species,nomatch=FALSE)
  is_Pac<-rbind(is_Pac,Pac)
  South<-match(i,South_check2$Species,nomatch=FALSE)
  is_South<-rbind(is_South,South)
  
}

survey3<-cbind(survey2,is_Art,is_Atl,is_Ind,is_Pac,is_South)
survey3$is_Art<-survey3$is_Art>0  #This line turns the numbers (which don't mean anything) into TRUE - means species is present in the zone.
survey3$is_Atl<-survey3$is_Atl>0  #This line turns the numbers (which don't mean anything) into TRUE - means species is present in the zone.
survey3$is_Ind<-survey3$is_Ind>0  #This line turns the numbers (which don't mean anything) into TRUE - means species is present in the zone.
survey3$is_Pac<-survey3$is_Pac>0  #This line turns the numbers (which don't mean anything) into TRUE - means species is present in the zone.
survey3$is_South<-survey3$is_South>0  #This line turns the numbers (which don't mean anything) into TRUE - means species is present in the zone.

write.csv(survey3, "survey_with_geo.csv") #write it so I don't have to do it again!


# Read in files

animalgeo<-read.csv("animal_list_with_geo.csv")
survey3<-read.csv("survey_with_geo.csv")

#### For Arctic Ocean (adopted from Anne's code for latitudinal zones)

# nb of nominal species in ocean
table(animalgeo$is_Art, useNA="always") # to check there are several cases and their numbers
a <- nrow(filter(animalgeo,is_Art==TRUE))
# nb of these which have CS in this zone
b <- nrow(filter(animalgeo, (is_Art==T)&(is_cs==T)))
# mean nb of CS per nominal sp with CS (based on survey dataset)
c <- mean((filter(survey3, is_Art==T))[,"Nb_CS"], na.rm=T)
sd <- sd((filter(survey3, is_Art==T))[,"Nb_CS"], na.rm=T)
# number of biological species MISSED due to CS when counting only nominal species (indirect estimation and direct count, differences unexplained)
m <- b*(c-1)                        
m2 <- sum(filter(survey3, is_Art==T)[,"Nb_CS"], na.rm=T) - nrow(filter(survey3, is_Art==T))   # direct count, quite distinct (but error since a few NS appear several times in survey)
#proportions of biological species missed
prop.m<-m/(a+m)
prop.m2<-m2/(a+m2)

missArt<-c(a,b,c,sd,m,m2,prop.m,prop.m2)

#### For Atlantic Ocean 

# nb of nominal species in ocean
table(animalgeo$is_Atl, useNA="always") # to check there are several cases and their numbers
a <- nrow(filter(animalgeo,is_Atl==TRUE))
# nb of these which have CS in this zone
b <- nrow(filter(animalgeo, (is_Atl==T)&(is_cs==T)))
# mean nb of CS per nominal sp with CS (based on survey dataset)
c <- mean((filter(survey3, is_Atl==T))[,"Nb_CS"], na.rm=T)
sd <- sd((filter(survey3, is_Atl==T))[,"Nb_CS"], na.rm=T)
# number of biological species MISSED due to CS when counting only nominal species (indirect estimation and direct count, differences unexplained)
m <- b*(c-1)                        
m2 <- sum(filter(survey3, is_Atl==T)[,"Nb_CS"], na.rm=T) - nrow(filter(survey3, is_Atl==T))   # direct count, quite distinct (but error since a few NS appear several times in survey)
#proportions of biological species missed
prop.m<-m/(a+m)
prop.m2<-m2/(a+m2)

missAtl<-c(a,b,c,sd,m,m2,prop.m,prop.m2)

#### For Indian Ocean 

# nb of nominal species in ocean
table(animalgeo$is_Ind, useNA="always") # to check there are several cases and their numbers
a <- nrow(filter(animalgeo,is_Ind==TRUE))
# nb of these which have CS in this zone
b <- nrow(filter(animalgeo, (is_Ind==T)&(is_cs==T)))
# mean nb of CS per nominal sp with CS (based on survey dataset)
c <- mean((filter(survey3, is_Ind==T))[,"Nb_CS"], na.rm=T)
sd <- sd((filter(survey3, is_Ind==T))[,"Nb_CS"], na.rm=T)
# number of biological species MISSED due to CS when counting only nominal species (indirect estimation and direct count, differences unexplained)
m <- b*(c-1)                        
m2 <- sum(filter(survey3, is_Ind==T)[,"Nb_CS"], na.rm=T) - nrow(filter(survey3, is_Ind==T))   # direct count, quite distinct (but error since a few NS appear several times in survey)
#proportions of biological species missed
prop.m<-m/(a+m)
prop.m2<-m2/(a+m2)

missInd<-c(a,b,c,sd,m,m2,prop.m,prop.m2)

#### For Pacific Ocean 

# nb of nominal species in ocean
table(animalgeo$is_Pac, useNA="always") # to check there are several cases and their numbers
a <- nrow(filter(animalgeo,is_Pac==TRUE))
# nb of these which have CS in this zone
b <- nrow(filter(animalgeo, (is_Pac==T)&(is_cs==T)))
# mean nb of CS per nominal sp with CS (based on survey dataset)
c <- mean((filter(survey3, is_Pac==T))[,"Nb_CS"], na.rm=T)
sd <- sd((filter(survey3, is_Pac==T))[,"Nb_CS"], na.rm=T)
# number of biological species MISSED due to CS when counting only nominal species (indirect estimation and direct count, differences unexplained)
m <- b*(c-1)                        
m2 <- sum(filter(survey3, is_Pac==T)[,"Nb_CS"], na.rm=T) - nrow(filter(survey3, is_Pac==T))   # direct count, quite distinct (but error since a few NS appear several times in survey)
#proportions of biological species missed
prop.m<-m/(a+m)
prop.m2<-m2/(a+m2)

missPac<-c(a,b,c,sd,m,m2,prop.m,prop.m2)

#### For Southern Ocean 

# nb of nominal species in ocean
table(animalgeo$is_South, useNA="always") # to check there are several cases and their numbers
a <- nrow(filter(animalgeo,is_South==TRUE))
# nb of these which have CS in this zone
b <- nrow(filter(animalgeo, (is_South==T)&(is_cs==T)))
# mean nb of CS per nominal sp with CS (based on survey dataset)
c <- mean((filter(survey3, is_South==T))[,"Nb_CS"], na.rm=T)
sd <- sd((filter(survey3, is_South==T))[,"Nb_CS"], na.rm=T)
# number of biological species MISSED due to CS when counting only nominal species (indirect estimation and direct count, differences unexplained)
m <- b*(c-1)                        
m2 <- sum(filter(survey3, is_South==T)[,"Nb_CS"], na.rm=T) - nrow(filter(survey3, is_South==T))   # direct count, quite distinct (but error since a few NS appear several times in survey)
#proportions of biological species missed
prop.m<-m/(a+m)
prop.m2<-m2/(a+m2)

missSouth<-c(a,b,c,sd,m,m2,prop.m,prop.m2)

# Joining all zones together
MissPerOcean<-data.frame(missArt,missAtl, missInd, missPac, missSouth)
rownames(MissPerOcean)<-c("nb nominal spp","nb NS having CS","mean nb CS/sp.with","sd_nb CS/sp.with", "estim. missed Biol sp", "count missed Biol sp","prop.m","prop.m2")

# WRITE TABLE in a file: choose name carefully (either restricted to NCBI species or not, below)
# write.csv2(MissPerZone, file="Missed biological species per zone.csv")
write.csv(MissPerOcean, file="Missed_biological_spp_per_Ocean.csv")

# These numbers look pretty different than the zone numbers. 
# Maybe this is because I pulled the original animal list from OBIS and the zone analysis (Anne) pulled from WORMS
# THEREFORE I am only including species that are in OBIS to begin wtih?
# But if they aren't in OBIS, how can we know where they exist?
# My total species count is higher in all oceans than the zone numbers
# My CS count is higher in all oceans than the zone numbers


# Two more things to do
# 1: Expected vs observed chi square tests per ocean (like Mark did)
# Based on Mark's code

#Chi-square test
nspecies<-c(nrow(filter(animalgeo,is_Art==TRUE)),nrow(filter(animalgeo,is_Atl==TRUE)),nrow(filter(animalgeo,is_Ind==TRUE)),
            nrow(filter(animalgeo,is_Pac==TRUE)),nrow(filter(animalgeo,is_South==TRUE)))

nspecies_cryptic<-c(nrow(filter(survey3,is_Art==TRUE)),nrow(filter(survey3,is_Atl==TRUE)),nrow(filter(survey3,is_Ind==TRUE)),
                    nrow(filter(survey3,is_Pac==TRUE)),nrow(filter(survey3,is_South==TRUE)))

table_nspecies<-t(data.frame(nspecies,nspecies_cryptic))
collumnname<-c("Arctic","Atlantic","Indian","Pacific","Southern")
colnames(table_nspecies)<-collumnname

chisq.test(table_nspecies)

# Significant but I am not convinced -- let's redo this by hand.
# Total NS = 168999
# Total CS = 1632 (note that both these numbers include double counting and idk what to do about that
# Expected Arctic = 61.85 -- I DID THESE BY HAND, FIX
# Expected Atlantic = 451,21
# Expected Indian = 383
# Expected Pacific = 668
# Expected Southern = 67.46
# So lots more than expected in Arctic, Indian
# About as expected in Atlantic, Southern
# Lots fewer than expected in Pacific
# More or less what Mark found

#Chisq with those numbers
expected<-c(61.85,451.21,383,668,67.46)
observed<-c(130,453,430,562,59)
chisq.test(rbind(expected,observed))

# 2: Calculate the number of CS we would expect to find if we had NCBI data for all species in an ocean
# I think this will be (CS/NCBI)*Total_Obis for each ocean
# we have a column in animalspp that is NCBI id - if >0, NCBI data exist.

#Arctic Ocean

d<-nrow(filter(animalgeo, (is_Art==T)&(ncbi_id>0))) #total in ocean with NCBI
e<-nrow(filter(animalgeo, (is_Art==T))) #total in ocean
f<-nrow(filter(animalgeo, (is_Art==T)&(is_cs==T))) #total CS
g<-(f/d)*e #total in ocean IF everyone had NCBI
h<-g-f  #total MISSING CS
j<-h/f   # proportion missing CS -- compared to total CS already found

missingArt<-cbind(e,f,g,h,j)

#Atlantic Ocean

d<-nrow(filter(animalgeo, (is_Atl==T)&(ncbi_id>0))) #total in ocean with NCBI
e<-nrow(filter(animalgeo, (is_Atl==T))) #total in ocean
f<-nrow(filter(animalgeo, (is_Atl==T)&(is_cs==T))) #total CS
g<-(f/d)*e #total in ocean IF everyone had NCBI
h<-g-f  #total MISSING
j<-h/f  # proportion MISSING

missingAtl<-cbind(e,f,g,h,j)

#Indian Ocean

d<-nrow(filter(animalgeo, (is_Ind==T)&(ncbi_id>0))) #total in ocean with NCBI
e<-nrow(filter(animalgeo, (is_Ind==T))) #total in ocean
f<-nrow(filter(animalgeo, (is_Ind==T)&(is_cs==T))) #total CS
g<-(f/d)*e #total in ocean IF everyone had NCBI
h<-g-f  #total MISSING
j<-h/f   # proportion MISSING

missingInd<-cbind(e,f,g,h,j)

#Pacific Ocean

d<-nrow(filter(animalgeo, (is_Pac==T)&(ncbi_id>0))) #total in ocean with NCBI
e<-nrow(filter(animalgeo, (is_Pac==T))) #total in ocean
f<-nrow(filter(animalgeo, (is_Pac==T)&(is_cs==T))) #total CS
g<-(f/d)*e #total in ocean IF everyone had NCBI
h<-g-f  #total MISSING
j<-h/f   # proportion MISSING

missingPac<-cbind(e,f,g,h,j)

#Southern Ocean

d<-nrow(filter(animalgeo, (is_South==T)&(ncbi_id>0))) #total in ocean with NCBI
e<-nrow(filter(animalgeo, (is_South==T))) #total in ocean
f<-nrow(filter(animalgeo, (is_South==T)&(is_cs==T))) #total CS
g<-(f/d)*e #total in ocean IF everyone had NCBI
h<-g-f  #total MISSING
j<-h/f   # proportion MISSING

missingSouth<-cbind(e,f,g,h,j)

MissingPerOcean<-rbind(missingArt,missingAtl, missingInd, missingPac, missingSouth)
colnames(MissingPerOcean)<-c("Total in ocean","Total CS observed","Total CS expected","Total not yet found","Proportion not yet found")
rownames(MissingPerOcean)<-c("Arctic","Atlantic","Indian","Pacific","Southern")

write.csv(MissingPerOcean,"Missing_CS_per_Ocean.csv")

# Results say that in all oceans, there are more CS out there than have already been discovered
# But that proportion is highest in the Southern ocean (3.5X expected out there)


# WILL PICK UP HERE - this is Anne's code for the phyla, copy-pasted
# 2- By PHYLUM ####
# Here I am going to use the WORMS data and not the OBIS data, so that we have a larger sample size.
# I think that's ok but we need to remember it.

nomsp <-read.csv2(file="file_S2_oct.csv" ,header=T)# need to load all nominal spp data for some comparisons 


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

write.csv(info, file="Missed biological species per phylum Oct27.csv")

# BY CLASS instead of phylum

class <- factor(unique(nomsp$class))
columns <- c("nb_NS","nb_NS_ncbi","nb_NS_withCS","nb_NS_withCS_ncbi","mean_Nb_CS_perCpx")
info    <- data.frame(matrix(nrow = length(class), ncol = length(columns))) 
colnames(info) = columns
row.names(info)<- class

survey$class_worms<-as.character(survey$class_worms) # required for the loop below to work

for (i in 1:length(class)){ 
  info$nb_NS[i] <-nrow(filter(nomsp,class==class[i]))
  info$nb_NS_ncbi[i] <-nrow(filter(nomsp,(class==class[i]) & (ncbi==T)))
  info$nb_NS_withCS[i] <-nrow(filter(nomsp,(class==class[i]) & (hasCS==T)))
  info$nb_NS_withCS_ncbi[i] <-nrow(filter(nomsp,(class==class[i])& (ncbi==T) & (hasCS==T)))
  info$mean_Nb_CS_perCpx[i] <-mean(filter(survey,(class_worms==class[i]))[,"Nb_CS"], na.rm=T)
  info$sd_Nb_CS_perCpx[i] <-sd(filter(survey,(class_worms==class[i]))[,"Nb_CS"], na.rm=T)
}  

info$missed_BS_estim <- info$nb_NS_withCS * (info$mean_Nb_CS_perCpx-1)
info$prop.missed_BS_estim <- info$missed_BS_estim / (info$missed_BS_estim + info$nb_NS )
info$prop.missed_BS_estim_ncbi <- info$missed_BS_estim / (info$missed_BS_estim + info$nb_NS_ncbi )  

write.csv(info, file="Missed biological species per class Oct30.csv")


#Ok, now let's figure out how to do the MISSING like I did yesterday for the oceans
# 2: Calculate the number of CS we would expect to find if we had NCBI data for all species in an ocean

#Loop over phyla
missing_phyla<-data.frame()

for (i in phyla){

d<-nrow(filter(nomsp, (phylum==i)&(ncbi==T))) #total in phylum with NCBI
e<-nrow(filter(nomsp, (phylum==i))) #total in phylum
f<-nrow(filter(nomsp, (phylum==i)&(hasCS==T))) #total CS
g<-(f/d)*e #total in phylum IF everyone had NCBI
h<-g-f  #total MISSING CS, yet to be found
j<-h/f   # proportion missing CS -- compared to total CS already found, so often greater than 1

missing<-cbind(i,e,f,g,h,j)
missing_phyla<-rbind(missing_phyla,missing)

}

colnames(missing_phyla)<-c("Phylum","Total in phylum","Total CS","Total CS IF NCBI","Total CS yet to be found","Proportion CS yet to be found")

write.csv(missing_phyla,"missing_per_phylum.csv")

#Ok, now let's figure out how to do the MISSING by latitudinal zones
# 2: Calculate the number of CS we would expect to find if we had NCBI data for all species in an ocean


  d<-nrow(filter(nomsp, (Npol==T)&(ncbi==T))) #total in phylum with NCBI
  e<-nrow(filter(nomsp, (Npol==T))) #total in phylum
  f<-nrow(filter(nomsp, (Npol==T)&(hasCS==T))) #total CS
  g<-(f/d)*e #total in phylum IF everyone had NCBI
  h<-g-f  #total MISSING CS, yet to be found
  j<-h/f   # proportion missing CS -- compared to total CS already found, so often greater than 1
  missing_Npol<-cbind(i,g,h,j)
  
  d<-nrow(filter(nomsp, (Ntemp==T)&(ncbi==T))) #total in phylum with NCBI
  e<-nrow(filter(nomsp, (Ntemp==T))) #total in phylum
  f<-nrow(filter(nomsp, (Ntemp==T)&(hasCS==T))) #total CS
  g<-(f/d)*e #total in phylum IF everyone had NCBI
  h<-g-f  #total MISSING CS, yet to be found
  j<-h/f   # proportion missing CS -- compared to total CS already found, so often greater than 1
  missing_Ntemp<-cbind(i,g,h,j)
  
  d<-nrow(filter(nomsp, (Trop==T)&(ncbi==T))) #total in phylum with NCBI
  e<-nrow(filter(nomsp, (Trop==T))) #total in phylum
  f<-nrow(filter(nomsp, (Trop==T)&(hasCS==T))) #total CS
  g<-(f/d)*e #total in phylum IF everyone had NCBI
  h<-g-f  #total MISSING CS, yet to be found
  j<-h/f   # proportion missing CS -- compared to total CS already found, so often greater than 1
  missing_Trop<-cbind(i,g,h,j)
  
  d<-nrow(filter(nomsp, (Stemp==T)&(ncbi==T))) #total in phylum with NCBI
  e<-nrow(filter(nomsp, (Stemp==T))) #total in phylum
  f<-nrow(filter(nomsp, (Stemp==T)&(hasCS==T))) #total CS
  g<-(f/d)*e #total in phylum IF everyone had NCBI
  h<-g-f  #total MISSING CS, yet to be found
  j<-h/f   # proportion missing CS -- compared to total CS already found, so often greater than 1
  missing_Stemp<-cbind(i,g,h,j)
  
  d<-nrow(filter(nomsp, (Spol==T)&(ncbi==T))) #total in phylum with NCBI
  e<-nrow(filter(nomsp, (Spol==T))) #total in phylum
  f<-nrow(filter(nomsp, (Spol==T)&(hasCS==T))) #total CS
  g<-(f/d)*e #total in phylum IF everyone had NCBI
  h<-g-f  #total MISSING CS, yet to be found
  j<-h/f   # proportion missing CS -- compared to total CS already found, so often greater than 1
  missing_Spol<-cbind(i,g,h,j)
  

  missing_zones<-rbind(missing_Npol,missing_Ntemp,missing_Trop,missing_Stemp,missing_Spol)
  
colnames(missing_zones)<-c("Zone","Total CS IF NCBI","Total CS yet to be found","Proportion CS yet to be found")
rownames(missing_zones)<-c("Npol","Ntemp","Trop","Stemp","Spol")

write.csv(missing_zones,"missing_per_zone.csv")


# 2: Calculate the number of CS we would expect to find if we had NCBI data for all species in an ocean
#BY CLASS
#Loop over class
missing_class<-data.frame()

for (i in class){
  
  d<-nrow(filter(nomsp, (class==i)&(ncbi==T))) #total in class with NCBI
  e<-nrow(filter(nomsp, (class==i))) #total in class
  f<-nrow(filter(nomsp, (class==i)&(hasCS==T))) #total CS
  g<-(f/d)*e #total in class IF everyone had NCBI
  h<-g-f  #total MISSING CS, yet to be found
  j<-h/f   # proportion missing CS -- compared to total CS already found, so often greater than 1
  
  missing<-cbind(i,g,h,j)
  missing_class<-rbind(missing_class,missing)
  
}

colnames(missing_class)<-c("Class","Total CS IF NCBI","Total CS yet to be found","Proportion CS yet to be found")

write.csv(missing_class,"missing_per_class.csv")


## Pickign up again April 12, Montpellier

missing_phyla<-data.frame()
phyla <- factor(unique(nomsp$phylum))

for (i in phyla){
  
  d<-nrow(filter(nomsp, (phylum==i)&(ncbi==T))) #total in phylum with NCBI
  e<-nrow(filter(nomsp, (phylum==i))) #total in phylum
  f<-nrow(filter(nomsp, (phylum==i)&(hasCS==T))) #total CS
  g<-(f/d)*e #total CS in phylum IF everyone had NCBI
  h<-g-f  #total MISSING CS, yet to be found
  j<-h/g  #proportion of MISSING CS
  
  missing<-cbind(i,e,f,g,h,j)
  missing_phyla<-rbind(missing_phyla,missing)
  
}

colnames(missing_phyla)<-c("Phylum","Total_phylum","Total_CS","Total_IF_NCBI","Not_yet_found","Proportion_missing")

# Now graphs (large phyla only, >= 8 CS found)

largephyla<-missing_phyla %>%                 # Order table with dplyr
  as.data.frame() %>% 
  arrange(desc(as.numeric(Total_CS)))

largephyla<-largephyla[1:10,]

ggplot(largephyla,aes(x=Phylum,y=as.numeric(Proportion_missing)))+
  geom_bar(stat="identity")+
  ylab("Proportion CS missing")+
  xlab("Phylum")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())

# Now redo again for the geography

d<-nrow(filter(animalgeo, (is_Art==T)&(ncbi_id>0))) #total in ocean with NCBI
e<-nrow(filter(animalgeo, (is_Art==T))) #total in ocean
f<-nrow(filter(animalgeo, (is_Art==T)&(is_cs==T))) #total CS
g<-(f/d)*e #total in ocean IF everyone had NCBI
h<-g-f  #total MISSING CS
j<-h/g   # proportion missing CS -- compared to total CS already found

missingArt<-cbind(e,f,g,h,j)

#Atlantic Ocean

d<-nrow(filter(animalgeo, (is_Atl==T)&(ncbi_id>0))) #total in ocean with NCBI
e<-nrow(filter(animalgeo, (is_Atl==T))) #total in ocean
f<-nrow(filter(animalgeo, (is_Atl==T)&(is_cs==T))) #total CS
g<-(f/d)*e #total in ocean IF everyone had NCBI
h<-g-f  #total MISSING
j<-h/g  # proportion MISSING

missingAtl<-cbind(e,f,g,h,j)

#Indian Ocean

d<-nrow(filter(animalgeo, (is_Ind==T)&(ncbi_id>0))) #total in ocean with NCBI
e<-nrow(filter(animalgeo, (is_Ind==T))) #total in ocean
f<-nrow(filter(animalgeo, (is_Ind==T)&(is_cs==T))) #total CS
g<-(f/d)*e #total in ocean IF everyone had NCBI
h<-g-f  #total MISSING
j<-h/g   # proportion MISSING

missingInd<-cbind(e,f,g,h,j)

#Pacific Ocean

d<-nrow(filter(animalgeo, (is_Pac==T)&(ncbi_id>0))) #total in ocean with NCBI
e<-nrow(filter(animalgeo, (is_Pac==T))) #total in ocean
f<-nrow(filter(animalgeo, (is_Pac==T)&(is_cs==T))) #total CS
g<-(f/d)*e #total in ocean IF everyone had NCBI
h<-g-f  #total MISSING
j<-h/g   # proportion MISSING

missingPac<-cbind(e,f,g,h,j)

#Southern Ocean

d<-nrow(filter(animalgeo, (is_South==T)&(ncbi_id>0))) #total in ocean with NCBI
e<-nrow(filter(animalgeo, (is_South==T))) #total in ocean
f<-nrow(filter(animalgeo, (is_South==T)&(is_cs==T))) #total CS
g<-(f/d)*e #total in ocean IF everyone had NCBI
h<-g-f  #total MISSING
j<-h/g   # proportion MISSING

missingSouth<-cbind(e,f,g,h,j)


MissingPerOcean<-rbind(missingArt,missingAtl, missingInd, missingPac, missingSouth)
MissingPerOcean<-cbind(MissingPerOcean,c("Arctic","Atlantic","Indian","Pacific","Southern"))
colnames(MissingPerOcean)<-c("Total_ocean","Total_CS","CS_expected","Missing","Proportion_Missing","Ocean")
rownames(MissingPerOcean)<-c("Arctic","Atlantic","Indian","Pacific","Southern")


missingoce<-ggplot(as.data.frame(MissingPerOcean),aes(x=as.character(Ocean),y=as.numeric(Proportion_Missing)))+
  geom_bar(stat="identity")+
  ylab("Proportion CS missing")+
  xlab("Ocean")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())

oce<-ggplot(as.data.frame(MissingPerOcean),aes(x=as.character(Ocean),y=as.numeric(Total_CS)))+
  geom_bar(stat="identity")+
  ylab("Total CS Found")+
  xlab("Ocean")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())

plot_grid(oce,missingoce,ncol=2)