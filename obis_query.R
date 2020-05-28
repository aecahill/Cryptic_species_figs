library(dplyr)
library(robis)

#read in list of nominal species. 
#Make sure there are no spaces after species names!
specieslist<-read.csv("C:/Users/aecsk/Desktop/specieslist.csv")

#initate blank dataframe
outdata = NULL

for (i in specieslist$SpeciesName[1:10]) { #
  sp <- try(occurrence(i), silent=FALSE)
  
  #data<-cbind(sp$scientificName,sp$decimalLatitude)
  
  #data1<-rbind(data1,data)
  
#}
  
  trop<-try(between(sp$decimalLatitude,-23.5,23.5))
  istrop<-length(trop[trop== TRUE])>0
  
  ntemp<-try(between(sp$decimalLatitude,23.5,66.5))
  isntemp<-length(ntemp[ntemp== TRUE])>0
  
  npol<-try(between(sp$decimalLatitude,66.5,90))
  isnpol<-length(npol[npol== TRUE])>0
  
  stemp<-try(between(sp$decimalLatitude,-66.5,-23.5))
  isstemp<-length(stemp[stemp == TRUE])>0
  
  spol<-try(between(sp$decimalLatitude,-90,-66.5))
  isspol<-length(spol[spol == TRUE])>0
  
  sprow<-c(i,istrop,isntemp,isnpol,isstemp,isspol)
  
  outdata<-rbind(outdata,sprow)
}

colnames(outdata)<-c("Name","Trop","NTemp","NPol","STemp","SPol")

outdata