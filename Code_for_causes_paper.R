#Script to work on analyses for causes paper
#Starting on October 9, after working through the table.
# NOTE: statistical significance is part of CSss -- if I use it otherwise, need to note

library(dplyr)
library(stringr)
library(ggplot2)
library(vioplot)
library(bestglm)
library(tidyr)
library(ggmosaic)

survey <- read.csv(file="977_CScpx_100cols_Habitat&Larva_Completed_20230928_comma.csv"   , header=TRUE ) # file ready for analyses with new variables ,# data with range sizes rarefied for 100 sites, and RIOK
nomsp <-read.csv2(file="file_S2_oct.csv" ,header=T)# need to load all nominal spp data for some comparisons 
bio<-read.csv2(file="input_BIOLOGY_classes.csv")

# cleaned from some original variables and temporary variable (CAUTION: CHECK COLUMN nb!):
# reintroduce genus but several possibilities: solution below not OK for CS at genus level (no data), can also use NameBoforeCor instead of acceptedName
survey$genus <-word(survey$acceptedName_wormsV1,1)
survey$genusBC <-word(survey$NameBeforeCor,1) # see if this genusBC changes results / genus.
survey <-as.data.frame(unclass(survey),stringsAsFactors=TRUE)
survey$Geo_dif3 <-survey$Geo_diff
levels(survey$Geo_dif3)<-c("","Allopatric", "Allopatric", "Allopatric","Sympatric","Sympatric, Allopatric", "Sympatric, Allopatric", "Sympatric, Allopatric")
levels(survey$Geo_dif3)<-c(NA, "Allopatric","Sympatric","Sympatric, Allopatric")
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
survey$CSss     <-str_detect(survey$Morpho_diff,"Statistic") | str_detect(survey$Morpho_diff,"No differentiation")

# Morpho differentiation types: More CS sl than CS ss overall?
#First, need to combine stat + no_diff to make CSss
survey$CSss     <-str_detect(survey$Morpho_diff,"Statistic") | str_detect(survey$Morpho_diff,"No differentiation")

#Now need to find expectations and test against 50:50 mark
expected<-c((length(na.omit(survey$Morpho_diff)))/2,(length(na.omit(survey$Morpho_diff)))/2)
observed<-c(length(which(survey$CSss=="TRUE")),length(which(survey$CSss=="FALSE")))
chisq.test(cbind(expected,observed))

# Next we will make a graph