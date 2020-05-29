library(dplyr)
library(robis)

#read in list of nominal species. 
#Make sure there are no spaces after species names!
specieslist<-read.csv("C:/Users/aecsk/Desktop/specieslist.csv")

#initate blank dataframe
occ = NULL

nums = NULL

for (i in specieslist$SpeciesName) { 
  sp <- try(occurrence(i), silent=TRUE) #looking for species in OBIS, pull occurrence data
 
  trop<-try(between(sp$decimalLatitude,-23.5,23.5),silent=TRUE) #check if each obs is in the tropics
  istropocc<-length(trop[trop== TRUE])>0 #see if there are any obs in the tropics
  istropnums<-length(trop[trop== TRUE])
  
  #same for northern temp zone
  ntemp<-try(between(sp$decimalLatitude,23.5,66.5),silent=TRUE)
  isntempocc<-length(ntemp[ntemp== TRUE])>0
  isntempnums<-length(ntemp[ntemp== TRUE])
  
  #same for northern polar zone
  npol<-try(between(sp$decimalLatitude,66.5,90),silent=TRUE)
  isnpolocc<-length(npol[npol== TRUE])>0
  isnpolnums<-length(npol[npol== TRUE])
  
  #same for southern temperate zone
  stemp<-try(between(sp$decimalLatitude,-66.5,-23.5),silent=TRUE)
  isstempocc<-length(stemp[stemp == TRUE])>0
  isstempnums<-length(stemp[stemp == TRUE])
  
  #same for southern polar zone
  spol<-try(between(sp$decimalLatitude,-90,-66.5),silent=TRUE)
  isspolocc<-length(spol[spol == TRUE])>0
  isspolnums<-length(spol[spol == TRUE])
  
  sprowocc<-c(i,istropocc,isntempocc,isnpolocc,isstempocc,isspolocc) #bind all T/F values for a species together
  sprownums<-c(i,istropnums,isntempnums,isnpolnums,isstempnums,isspolnums) 
  
  occ<-rbind(occ,sprowocc) #add to dataset
  nums<-rbind(nums,sprownums)
}

colnames(occ)<-c("Name","Trop","NTemp","NPol","STemp","SPol")
colnames(nums)<-c("Name","Trop","NTemp","NPol","STemp","SPol")

occ
nums