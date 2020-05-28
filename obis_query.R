library(dplyr)
library(robis)

#read in list of nominal species. 
#Make sure there are no spaces after species names!
specieslist<-read.csv("C:/Users/aecsk/Desktop/specieslist.csv")

#initate blank dataframe
outdata = NULL

for (i in specieslist$SpeciesName[1:10]) { #currently set to only do first 10 in the list
  sp <- try(occurrence(i), silent=FALSE) #looking for species in OBIS, pull occurrence data
 
  trop<-try(between(sp$decimalLatitude,-23.5,23.5)) #check if each obs is in the tropics
  istrop<-length(trop[trop== TRUE])>0 #see if there are any obs in the tropics
  
  #same for northern temp zone
  ntemp<-try(between(sp$decimalLatitude,23.5,66.5))
  isntemp<-length(ntemp[ntemp== TRUE])>0
  
  #same for northern polar zone
  npol<-try(between(sp$decimalLatitude,66.5,90))
  isnpol<-length(npol[npol== TRUE])>0
  
  #same for southern temperate zone
  stemp<-try(between(sp$decimalLatitude,-66.5,-23.5))
  isstemp<-length(stemp[stemp == TRUE])>0
  
  #same for southern polar zone
  spol<-try(between(sp$decimalLatitude,-90,-66.5))
  isspol<-length(spol[spol == TRUE])>0
  
  sprow<-c(i,istrop,isntemp,isnpol,isstemp,isspol) #bind all T/F values for a species together
  
  outdata<-rbind(outdata,sprow) #add to dataset
}

colnames(outdata)<-c("Name","Trop","NTemp","NPol","STemp","SPol")

outdata