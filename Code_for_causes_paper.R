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
survey$CSss     <-str_detect(survey$Morpho_diff,"Statistic") | str_detect(survey$Morpho_diff,"No differentiation") #need to combine stat + no_diff to make CSss
survey$Sympatric <-str_detect(survey$Geo_dif3,"Sympatric") | str_detect(survey$Geo_dif3,"Sympatric, Allopatric") #This creates a vector that counts sym/allo as sympatric - that is, do spp exist in sympatry?

# Morpho differentiation types: More CS sl than CS ss overall?
#Need to find expectations and test against 50:50 mark
expected<-c((length(na.omit(survey$Morpho_diff)))/2,(length(na.omit(survey$Morpho_diff)))/2)
observed<-c(length(which(survey$CSss=="TRUE")),length(which(survey$CSss=="FALSE")))
chisq.test(cbind(expected,observed))

# Next we will make a graph
CSssGraph<-as.data.frame(cbind(observed,c("CSss","CSsl")))
colnames(CSssGraph)<-c("Number","CS_Type")
ggplot(data=CSssGraph,aes(x=CS_Type, y=Number))+geom_bar(stat="identity")+
  theme_bw()+
  #theme(legend.position="none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  theme(axis.text.x = element_text(angle = 65,hjust=1))

#Morphological characters
#First, more diagnostic traits with hard skeleton?
#First, start with cases where there is morphodiff
surveymorpho<-filter(survey, Morpho_diff != "NA") #make matrix where only the cases where morpho was studied is here
table(surveymorpho$Morpho_diff,surveymorpho$hard_skeleton)
chisq.test(surveymorpho$Morpho_diff,surveymorpho$hard_skeleton)

#graph
morphoskel_count<-table(surveymorpho$hard_skeleton)
morphodiff<-table(surveymorpho$Morpho_diff,surveymorpho$hard_skeleton) 
stat_diag<-morphodiff[3,]/morphodiff[1,]
stat_anydiff<-morphodiff[3,]/(morphodiff[3,]+morphodiff[1,])
stat_total<-morphodiff[3,]/morphoskel_count
diag_total<-morphodiff[1,]/morphoskel_count
nod_total<-morphodiff[2,]/morphoskel_count
morphoskeltable<-rbind(stat_total,diag_total,nod_total)
morphoskeltable #this should be the percentages for each, 
#columns sum to 1

surveymorphoskeltable<-pivot_longer(as.data.frame(morphoskeltable), cols=1:2, names_to = "Hard_Skeleton",
                                    values_to = "Percent", values_drop_na = FALSE)
surveymorphoskeltable$Diff_type<-c("Statistical","Statistical","Diagnostic","Diagnostic","No_Difference","No_Difference")

ggplot(as.data.frame(surveymorphoskeltable),aes(x=Hard_Skeleton,y=Percent,fill=Diff_type,))+geom_bar(stat="identity")

#NOTE: Do not get same result if I just consider CSss vs CSsl; it really makes a difference to have the three categories. 
# I think this is because "statistical" diff is lumped with no_diff into CSss, but for this trait that does not make sense
# If you have hard parts, there's more to measure and therefore more chance of getting statistical differences

#Next: image vision
table(surveymorpho$Morpho_diff,surveymorpho$image)
chisq.test(surveymorpho$Morpho_diff,surveymorpho$image)
#Not making a graph bc not significant; result does not change if I just consider CSss vs CSsl

#Next: fertilization
table(surveymorpho$Morpho_diff,surveymorpho$ferti)
chisq.test(surveymorpho$Morpho_diff,surveymorpho$ferti)

#Note: Still significant if we just do ss vs sl. Graphing whole thing.
morphoferti_count<-table(surveymorpho$ferti)
morphodiff<-table(surveymorpho$Morpho_diff,surveymorpho$ferti) 
stat_diag<-morphodiff[3,]/morphodiff[1,]
stat_anydiff<-morphodiff[3,]/(morphodiff[3,]+morphodiff[1,])
stat_total<-morphodiff[3,]/morphoferti_count
diag_total<-morphodiff[1,]/morphoferti_count
nod_total<-morphodiff[2,]/morphoferti_count
morphofertitable<-rbind(stat_total,diag_total,nod_total)
morphofertitable #this should be the percentages for each, eco and not
#columns sum to 1
surveymorphofertitable<-pivot_longer(as.data.frame(morphofertitable), cols=1:3, names_to = "Fertilization",
                                    values_to = "Percent", values_drop_na = FALSE)
surveymorphofertitable$Diff_type<-c("Statistical","Statistical","Statistical","Diagnostic","Diagnostic","Diagnostic","No_Difference","No_Difference","No_Difference")

ggplot(as.data.frame(surveymorphofertitable),aes(x=Fertilization,y=Percent,fill=Diff_type,))+geom_bar(stat="identity")

#What if I remove both
ferti_nob<-filter(surveymorpho, ferti != "B")
ferti_nob<-droplevels(ferti_nob)
table(ferti_nob$Morpho_diff,ferti_nob$ferti)
chisq.test(ferti_nob$Morpho_diff,ferti_nob$ferti)
#Still significant
#And if I do this for ss vs sl?
table(ferti_nob$CSss,ferti_nob$ferti)
chisq.test(ferti_nob$CSss,ferti_nob$ferti)
#Yep, still significant. Species with Internal fertilization are more likely to have diagnostic differences (i.e. CSsl)

# Next: genitalia
table(surveymorpho$Morpho_diff,surveymorpho$genitals)
chisq.test(surveymorpho$Morpho_diff,surveymorpho$genitals)

#and a graph

morphogen_count<-table(surveymorpho$genitals)
morphodiff<-table(surveymorpho$Morpho_diff,surveymorpho$genitals) 
stat_diag<-morphodiff[3,]/morphodiff[1,]
stat_anydiff<-morphodiff[3,]/(morphodiff[3,]+morphodiff[1,])
stat_total<-morphodiff[3,]/morphogen_count
diag_total<-morphodiff[1,]/morphogen_count
nod_total<-morphodiff[2,]/morphogen_count
morphogentable<-rbind(stat_total,diag_total,nod_total)
morphogentable #this should be the percentages for each, eco and not
#columns sum to 1

surveymorphogentable<-pivot_longer(as.data.frame(morphogentable), cols=1:3, names_to = "External_genitals",
                                   values_to = "Percent", values_drop_na = FALSE)
surveymorphogentable$Diff_type<-c("Statistical","Statistical","Statistical","Diagnostic","Diagnostic","Diagnostic","No_Difference","No_Difference","No_Difference")

ggplot(as.data.frame(surveymorphogentable),aes(x=External_genitals,y=Percent,fill=Diff_type,))+geom_bar(stat="identity")

#checking to see if this holds with just ss vs sl
table(surveymorpho$CSss,surveymorpho$genitals)
chisq.test(surveymorpho$CSss,surveymorpho$genitals)

#Turns out that only gastropods are "both," so what happens if we remove them?
surveymorphonob<-filter(surveymorpho, genitals != "B")
surveymorphonob<-droplevels(surveymorphonob)


table(surveymorphonob$Morpho_diff,surveymorphonob$genitals)
chisq.test(surveymorphonob$Morpho_diff,surveymorphonob$genitals)
#Still significant. 

#Checking for selection against hybrids
#Hypothesis: more diag differences (=CSsl) in sympatry than allopatry

table(surveymorpho$CSss,surveymorpho$Sympatric)
chisq.test(surveymorpho$CSss,surveymorpho$Sympatric)

CSsym_count<-table(surveymorpho$Sympatric)
CSssdiff<-table(surveymorpho$CSss,surveymorpho$Sympatric) 
CSss_total<-CSssdiff[2,]/CSsym_count
CSsl_total<-CSssdiff[1,]/CSsym_count
CSsymtable<-rbind(CSss_total,CSsl_total)
CSsymtable #this should be the percentages for each, sym and not, columns sum to 1

surveyCSsymtable<-pivot_longer(as.data.frame(CSsymtable), cols=1:2, names_to = "Sympatry",
                                   values_to = "Percent", values_drop_na = FALSE)
surveyCSsymtable$CSss_type<-c("CSsl","CSsl","CSss","CSss")

ggplot(as.data.frame(surveyCSsymtable),aes(x=Sympatry,y=Percent,fill=CSss_type,))+geom_bar(stat="identity")


#Hypothesis: High HKK leads to more CS ss / statistical divergence
#Prediction: More cases of diagnostic diffs in low HKK values

surveymorpho<-filter(survey, Morpho_diff != "NA") #make matrix where only the cases where morpho was studied is here
Hkk_count<-table(surveymorpho$HKKv3)
morphodiff<-table(surveymorpho$Morpho_diff,surveymorpho$HKKv3) 
stat_diag<-morphodiff[3,]/morphodiff[1,]
stat_anydiff<-morphodiff[3,]/(morphodiff[3,]+morphodiff[1,])
stat_total<-morphodiff[3,]/Hkk_count
diag_total<-morphodiff[1,]/Hkk_count
nod_total<-morphodiff[2,]/Hkk_count
HKKtable<-rbind(stat_total,diag_total,nod_total)
morphodiff 
HKKtable
Hkk_count
chisq.test(surveymorpho$Morpho_diff,surveymorpho$HKKv3)

#Graph
HKKtable2<-pivot_longer(as.data.frame(HKKtable), cols=1:6, names_to = "HKK",
                        values_to = "Percent", values_drop_na = FALSE)
HKKtable2$Diff_type<-c("Statistical","Statistical","Statistical","Statistical","Statistical","Statistical","Diagnostic","Diagnostic","Diagnostic","Diagnostic","Diagnostic","Diagnostic","No_Difference","No_Difference","No_Difference","No_Difference","No_Difference","No_Difference")
HKKtable2["HKK"][HKKtable2["HKK"] == "1"] <- "A1"
HKKtable2["HKK"][HKKtable2["HKK"] == "5"] <- "B2"
HKKtable2["HKK"][HKKtable2["HKK"] == "10"] <- "C10"
HKKtable2["HKK"][HKKtable2["HKK"] == "25"] <- "D25"
HKKtable2["HKK"][HKKtable2["HKK"] == "100"] <- "E100"
HKKtable2["HKK"][HKKtable2["HKK"] == "1000"] <- "F1000"

ggplot(as.data.frame(HKKtable2),aes(x=HKK,y=Percent,fill=Diff_type,))+geom_bar(stat="identity")

#Ok, what if we just consider CSss vs CSsl?

table(surveymorpho$HKKv3,surveymorpho$CSss)
chisq.test(surveymorpho$HKKv3,surveymorpho$CSss)

CSssdiff<-table(surveymorpho$CSss,surveymorpho$HKKv3) 
CSss_total<-CSssdiff[2,]/Hkk_count
CSsl_total<-CSssdiff[1,]/Hkk_count
HKKtable3<-rbind(CSss_total,CSsl_total)
HKKtable3

#Graph
HKKtable4<-pivot_longer(as.data.frame(HKKtable3), cols=1:6, names_to = "HKK",
                        values_to = "Percent", values_drop_na = FALSE)
HKKtable4$Diff_type<-c("CSss","CSss","CSss","CSss","CSss","CSss","CSsl","CSsl","CSsl","CSsl","CSsl","CSsl")
HKKtable4["HKK"][HKKtable4["HKK"] == "1"] <- "A1"
HKKtable4["HKK"][HKKtable4["HKK"] == "5"] <- "B2"
HKKtable4["HKK"][HKKtable4["HKK"] == "10"] <- "C10"
HKKtable4["HKK"][HKKtable4["HKK"] == "25"] <- "D25"
HKKtable4["HKK"][HKKtable4["HKK"] == "100"] <- "E100"
HKKtable4["HKK"][HKKtable4["HKK"] == "1000"] <- "F1000"

ggplot(as.data.frame(HKKtable4),aes(x=HKK,y=Percent,fill=Diff_type,))+geom_bar(stat="identity")

#More diagnostic differences (=CSsl) at lower HKK values, both when considering 3 morpho classes AND just ss vs sl

