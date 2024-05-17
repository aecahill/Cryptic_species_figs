# script to join the shallow species into a column of the survey

# First read in current last version of data

surveysp <- read.csv(file="978_species_clean_may17.csv", header=TRUE ) 

# And the shallow water

shallowsp<-read.csv("shallow_rock_sed.csv",header=T)

shallist<-c()
numsp<-c(1:length(surveysp$Article_authors))

for (i in numsp){
  if (sum(grepl(surveysp$NameBeforeCor[i],shallowsp$NameBeforeCor))>0)
    b<- TRUE
  else
    b<- FALSE
  shallist<-rbind(shallist,b)
}

surveysp2<-cbind(surveysp,shallist)