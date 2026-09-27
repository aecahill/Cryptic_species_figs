#Need to make forest plot with factors and odds ratios

library(dplyr)
library(stringr)
library(ggplot2)
library(vioplot)
library(bestglm)
library(tidyr)
library(ggmosaic)
library(lme4)
library(cowplot)
library(GLMMselect)
library(cAIC4)
library(tidyverse)
library(wesanderson)

survey <- read.csv(file="978_species_clean_may17.csv" , header=TRUE ) 
survey <-as.data.frame(unclass(survey),stringsAsFactors=TRUE)

# Need to generate models, odds ratios, CIs

surveymorpho<-filter(survey, Morpho_diff != "NA") #make matrix where only the cases where 

#Year model
yearbmod<-(glmer(CSss~yearb+(1|phylum_class),data=surveymorpho,family=binomial))
log_ci <- confint(yearbmod, parm = "beta_", method = "Wald") # "beta_" selects only fixed effects
odds_table_year <- exp(cbind(OR = fixef(yearbmod), log_ci))
year<-odds_table_year[2,]

#Bio traits
skelemod<-(glmer(CSss~hard_skeleton+(1|phylum_class),data=surveymorpho,family=binomial))
fertimod<-(glmer(CSss~ferti+(1|phylum_class),data=surveymorpho,family=binomial))
genimod<-(glmer(CSss~genitals+(1|phylum_class),data=surveymorpho,family=binomial))
visionmod<-(glmer(CSss~image+(1|phylum_class),data=surveymorpho,family=binomial))

log_ci_skel <- confint(skelemod, parm = "beta_", method = "Wald") # "beta_" selects only fixed effects
log_ci_ferti <- confint(fertimod, parm = "beta_", method = "Wald") # "beta_" selects only fixed effects
log_ci_geni <- confint(genimod, parm = "beta_", method = "Wald") # "beta_" selects only fixed effects
log_ci_vision <- confint(visionmod, parm = "beta_", method = "Wald") # "beta_" selects only fixed effects

odds_table_skele <- exp(cbind(OR = fixef(skelemod), log_ci_skel))
odds_table_ferti <- exp(cbind(OR = fixef(fertimod), log_ci_ferti))
odds_table_geni <- exp(cbind(OR = fixef(genimod), log_ci_geni))
odds_table_vision <- exp(cbind(OR = fixef(visionmod), log_ci_vision))

skeleton<-odds_table_skele[2,]
fertilization<-odds_table_ferti[2,]  # THIS IS ALSO A PROBLEM
genitalia<-odds_table_geni[2,]
vision<-odds_table_vision[2,]

#HKK
HKKmod<-(glmer(CSss~HKKv3+(1|phylum_class),data=surveymorpho,family=binomial))
log_ci <- confint(HKKmod, parm = "beta_", method = "Wald") # "beta_" selects only fixed effects
odds_table_HKK <- exp(cbind(OR = fixef(HKKmod), log_ci))
HKK<-odds_table_HKK[2,]

#Sympatry
sympmod<-(glmer(CSss~Sympatric+(1|phylum_class),data=surveymorpho,family=binomial))
log_ci <- confint(sympmod, parm = "beta_", method = "Wald") # "beta_" selects only fixed effects
odds_table_symp <- exp(cbind(OR = fixef(sympmod), log_ci))
sympatry<-odds_table_symp[2,]

# Eco diff
ecomod<-(glmer(CSss~Eco_diff+(1|phylum_class),data=surveymorpho,family=binomial))
log_ci <- confint(ecomod, parm = "beta_", method = "Wald") # "beta_" selects only fixed effects
odds_table_eco <- exp(cbind(OR = fixef(ecomod), log_ci))
eco_diff<-odds_table_eco[2,]

# Larval type (note - diff dataset)
surveylarvmorpho<-filter(surveymorpho, Larv_type != "NA")
larvmod<-(glmer(CSss~Larv_type+(1|phylum_class),data=surveylarvmorpho,family=binomial))
log_ci <- confint(larvmod, parm = "beta_", method = "Wald") # "beta_" selects only fixed effects
odds_table_larv <- exp(cbind(OR = fixef(larvmod), log_ci))
#larva<-odds_table_larv[2,]

# Sediment type (note - diff dataset)
surveysed<-filter(surveymorpho, Hsubstrate != "NA") 
surveysed<-filter(surveysed, Hsubstrate != "Sediment and Rocky") 
sedmod<-(glmer(CSss~Hsubstrate+(1|phylum_class),data=surveysed,family=binomial))
log_ci <- confint(sedmod, parm = "beta_", method = "Wald") # "beta_" selects only fixed effects
odds_table_sed <- exp(cbind(OR = fixef(sedmod), log_ci))
sediment<-odds_table_sed[2,]

OR_table<-cbind(year,skeleton,fertilization,genitalia,vision,HKK,sympatry,eco_diff,larva,sediment)

OR_table<-as.data.frame(t(rbind(colnames(OR_table),OR_table)))
colnames(OR_table)<-c("model","OR","low","high")

p<-  ggplot(OR_table,aes(y = fct_rev(model))) + 
 theme_classic()

p +
  geom_point(aes(x=as.numeric(OR)), shape=15, size=2) +
  geom_linerange(aes(xmin=as.numeric(low), xmax=as.numeric(high))) +
  geom_vline(xintercept = 1, linetype="dashed") +
  labs(x="Log Odds Ratio", y="")



# Reassigning HKK to be categorical

surveymorpho$HKKv3[surveymorpho$HKKv3==1] <- "cave"
surveymorpho$HKKv3[surveymorpho$HKKv3==5] <- "intertidal"
surveymorpho$HKKv3[surveymorpho$HKKv3==10] <- "estuaries"
surveymorpho$HKKv3[surveymorpho$HKKv3==25] <- "coastal"
surveymorpho$HKKv3[surveymorpho$HKKv3==100] <- "deep_sea"
surveymorpho$HKKv3[surveymorpho$HKKv3==1000] <- "pelagic"

#Trying the model again
summary(glmer(CSss~HKKv3+(1|phylum_class),data=surveymorpho,family=binomial))

HKKmod2<-(glmer(CSss~HKKv3+(1|phylum_class),data=surveymorpho,family=binomial))
log_ci <- confint(HKKmod2, parm = "beta_", method = "Wald") # "beta_" selects only fixed effects
odds_table_HKK2 <- exp(cbind(OR = fixef(HKKmod2), log_ci))


# Does this need its own figure? I sort of think so - maybe it replaces part of the current fig?
# In fact, HKK, Fertilization, and Larval Type all have multiple categories and this might need to be shown?