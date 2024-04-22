# This script helps to obtain, for each species of a list, ...
# ... the bibliographical references citing the species name OR ANY SYNONYM, 
# ... [AND other keywords (corresponding to the traits, for instance to fill in a database)]
# ... and to write in a table the reference numbers for each species.
# if the species list of synonyms is ready and has been used already, and 1wordspecies list done , can start at PART C.

# install.packages("tidyverse")
library(tidyverse)
# install.packages("readxl")
library(readxl)
# install.packages("writexl")
library(writexl)


# You need to set the working directory = the directory containing the script= source file location (Session menu in R studio).
list.files() # check that your input files are listed in the working directory

#############################################################################################################

### INPUT FILES NEEDED ####

# Caution: The 3rd input file can be obtained only AFTER RUNNING STEP A of this script

# 1) " scientificName.csv " : The list of species names for which you seek references (to obtain trait values)
# 2) " taxon.txt "          : The WoRMS (World Register of Marine Species) database main file : can be obtained by registering and asking the staff of WoRMS.
# 3) " savedrecs().xls "    : The output file from the Web of Science search in Excel format (saved records).

#############################################################################################################
### STEP A : CREATE A LIST OF ALL SYNONYMS OF EACH SPECIES NAME ####
################################################################################################################

# Load our initial species name list is Species_List (loaded as a .csv from an Excel file for instance, the column being named "scientificName")
spp<-read.csv2("scientificName.csv")

# Load the WoRMS database (can be obtained from WoRMS but it requires a particular request/registering, etc)
worms <- read_tsv('taxon.txt',show_col_types = FALSE) #

# keep only names of animal species 
worms<-filter(worms,(kingdom=="Animalia" & taxonRank=="Species")) 

# Find the "accepted Names" corresponding to this name which may not be "accepted" (WoRMS lists all scientificNames, even obsolete ones, and the "Accepted" names are in the acceptedNameUsage column)
spp$acceptedNameUsage <-worms$acceptedNameUsage[match(spp$scientificName,worms$scientificName)] # in the present case (Medit_spp.csv) the variable already exists
# may also add taxonomy information (phylum, class, order, family...) :


# Create the list of synonyms for each name of our species list

for (i in 1:nrow(spp)){
  name <- spp$acceptedNameUsage[i]
  w    <- which(worms$acceptedNameUsage==name)    
  list <- worms$scientificName[w]
  n    <- length(list)
  spp$synonymList[i]  <- paste0(list, collapse=", ")
  spp$Nb_synonyms[i]  <- n
} #  
summary(spp$Nb_synonyms) 
hist(spp$Nb_synonyms, breaks=67) 

# Below are some checks (were done for very big lists) 
nrow(filter(spp,Nb_synonyms==0)) #  had 0 species in list of synonyms 
# SOME of the scientificNames in spp. ARE NOT CLEAN SPECIES NAMES: they contain either 'spp.'  or 'cf.' or are family names, for instance.
# These should be corrected manually, or removed. You can then re-run the script with the corrected input file (no problem to follow running the script however)

# If you do not correct the initial list NOW but want to run the code, you can, provided you...
# ...RUN AT LEAST 1 OF THE 2 ALTERNATIVES BELOW (you can do both: just run all the code lines)
# They serve to avoid NA when no synonyms were found, in the final list or "species names" separated by OR, which will be used in bibliographic search. Because searches containing " NA OR NA" cannot run in WoS (logically).
# I recommend just running ALTERNATIVE 2 and skipping ALTERNATIVE 1.

# ALTERNATIVE 1 (I used this on a very large initial list, when not an updated worms version, to separate cases): 
spp$synonymList[spp$Nb_synonyms==0]<- spp$acceptedNameUsage[spp$Nb_synonyms==0] # can help in some cases (may be not in this example)
spp$Nb_synonyms[spp$Nb_synonyms==0 & is.na(spp$acceptedNameUsage)==F]<- 1
nrow(filter(spp,Nb_synonyms==0)) # nb sp without an acceptedNameUsage: then should use their speciesName (initial) in search WoS
spp$synonymList[spp$Nb_synonyms==0]<- spp$scientificName[spp$Nb_synonyms==0]
spp$Nb_synonyms[spp$Nb_synonyms==0]<- 1

# ALTERNATIVE 2  (simpler, has no effect if ALTERNATIVE 1 has been run previously)
spp <-filter(spp, is.na(synonymList)==F) # This step is necessary if you did not run code replacing missing synonymLists by scientificNames 

# Print the Total number of species names, including synonyms, just for your information :
sum(spp$Nb_synonyms) # in this example there are 516 species names (after adding synonyms to the 79 initial names)

# WRITE QUERY FOR SPECIES NAMES WITH SYNONYMS FOR BIBLIOGRAPHIC SEARCH

# Important preliminary step:  remove species names with NA in the synonymList:
spp <-filter(spp, is.na(synonymList)==F) # This step is necessary if you did not run code replacing missing synonymLists by scientificNames 
spp <-filter(spp, (synonymList)!="") # This step is necessary if you did not run code replacing missing synonymLists by scientificNames 

# initialize list by the first true element to avoid manual correction of the beginning of 1word_SpeciesList
list <-spp$synonymList[1] # then loop over all (initial) species while taking synonyms
for (i in 2:nrow(spp)){
  liste <- spp$synonymList[i]
  list <-paste (list, liste, sep=',')
}
list <-gsub (", ",",",list) # global substitution of ", " by "," (removes space after comma)
list <-gsub(",", " OR ", list) 
# list<-gsub("OR OR", "OR", list) # no longer useful, but does not harm (it is a security)
list <-gsub(' OR ','" OR "' ,list) # add quotes surrounding names (except first and last species)
list<-paste0('"',list,'"')  # add double quote at very beginning and very end of the string of characters 'list'.

writeChar(list, "1word_speciesListwithSyn")  # this writes the list in a file ...
# ... so you can open it, paste it in the field 'topic' to do the bibliographical search)

rm(list, liste,n,name,w,worms)

#############################################################################################################
### STEP B : (OUTSIDE R) FIND THE BIBLIOGRAPHICAL REFERENCES IN Web of Science : ####
################################################################################################################

# This step must be done on the web (not within the R environment):
# Use the list written in the file above to do Web of Science (or Scopus...) searches. Add in the search equation the keyword terms relating to the trait
# TS= ("Callinectes sapidus" OR "Crassostrea virginica" OR .......)
# AND
# TS=("body size" OR "body length" OR "body shape" OR "body volume" OR "body surface")
# The search equation provides ~420 references  which can be downloaded in Excel format ...
# ...(if more than 1000 references you can download several Excel files via the WoS interface)
# Save the reference file(s) in the same  directory of this script 
# Go back to this R code (STEP C)

# One bibliographical search for each main trait: save results in Excel format with abstracts (1-1000 ref maximum)
# if there are more than 1000 ref, several files are needed
# For instance, for Duration rename the Excel files: Duration1, Duration2
# 
# In the end we have, at least one exdcel file per main trait:
# Duration1.xlsx,
# Duration2.xlsx,
# Length1.xlsx, 
# Shape1.xlsx, 
# Fecundity1.xlsx
# ... 
# and so on for all main traits (11 for benthic invertebrates).
# PLACE ALL FILES INTO THE SAME DIRECTORY WITHIN THE WORKING DIRECTORY, named RefFolder

write_xlsx(spp,path="Species_synonym.xlsx", col_names = TRUE, format_headers = TRUE,use_zip64 = FALSE)


#############################################################################################################
### STEP C : REPORT THE BIBLIOGRAPHICAL REFERENCES for each species and each trait ####
################################################################################################################

# We can start from here then reload species and synonyms:
spp <-read_xlsx("Species_synonym.xlsx")

## C-1 ## LOAD, CUSTOMIZE & MERGE ALL REFERENCE FILES INTO A SINGLE BIG EXCEL FILE wITH NEW COLUMNs FOR TRAIT SEARCHED

filenames <- list.files("RefFolder") #RefFolder contains reference files (Excel) from Web of Science
traitnames <-gsub(".xls","",filenames)
traitnames <-unique(gsub('[0-9]+', '', traitnames))

# Initialising the reference file (to be completed in the for loop with all ref Excel files)
allreferences <-read_xls(paste0("RefFolder/",filenames[1]))
colstokeep <- c("Authors","Publication Year","Article Title","Abstract","Source Title","Volume","Start Page","End Page")
allreferences <-select(allreferences, all_of(colstokeep)) # cette expression fonctionne !

# keep only first line and col names (so we can  increment then in all 'filenames, avoid using brackets [i] in loop code)
allreferences <-allreferences[1,] # can then check how duplicates are removed
for (j in traitnames) { # create columns for each traits
  allreferences[,j] <- NA#
}

# looping to all individual reference files
for (i in filenames) {
  newdata <- read_xls(paste0("RefFolder/",i))  #load
  newdata <- select(newdata, all_of(colstokeep))
  #add columns for each trait:
  for (j in traitnames) {
    newdata[,j] <- grepl(j, i, fixed = TRUE)# 
  }
  allreferences <-rbind(newdata,allreferences)
}

# NEED TO find duplicate references (for biblio columns) and complete TRAITS replacing the FALSE corresponding to the Other trait for which the reference was also selected with TRUE
# Just to know how many duplicates in this dataset:
table(duplicated(allreferences[,c(1:3,5:8)])) # I removed abstracts to save the planet
allreferences$concat <-paste(allreferences$Authors,allreferences$`Publication Year`,allreferences$`Article Title`,allreferences$`Source Title`,allreferences$Volume,allreferences$`Start Page`)  

# 1st Solution ChatGPT (base R)
colnames(allreferences)[length(allreferences)]


for (i in 1:nrow(allreferences)) {
  A <- as.character(allreferences[i,"concat"])  # a character string
  w <- which(allreferences$concat == A) # w contains matching rows indices
  for (j in traitnames){
    if (str_detect(as.character(allreferences[w,j]),"TRUE")) {
      allreferences[i, j] <- TRUE
    }
  }
}

allreferences[1:12,8:(8+length(traitnames))] # to check that there are no TRUE everywhere 
# add column nb of TRUE columns
allreferences <- allreferences %>%
  mutate(nbT = rowSums(across(9:(8 + length(traitnames))), na.rm = TRUE))

table(allreferences$nbT) # to check, some should be more than 1 and 2...

colnames(allreferences)
# remove duplicates:
allreferences <-distinct(allreferences) 
allreferences <-select(allreferences, -"concat") # remove cols at the endnee to stop at last trait

# Then I renamed allreference -> dat, for simplicity (not to change all code below)
data <-allreferences
rm(allreferences) # and cleaned the environment removing 'allreferences'
rm(newdata)
####################################################
names(data)[names(data) == "Article Title"] <- "Article.Title" # R does not handle variable names with space (in general)
names(data)[names(data) == "Source Title"] <- "Source.Title" 
names(data)[names(data) == "Publication Year"] <- "Publication.Year" 
names(data)[names(data) == "Start Page"] <- "Start.Page" 
names(data)[names(data) == "End Page"] <- "End.Page" 

# create a variable 'referenceID' (to be sure things do not change between input and output):
data$referenceID <-rownames(data) 
# reorder columns in 'dat' (references):
data <-select(data, c(referenceID,Authors,Publication.Year, Article.Title, Abstract,Source.Title, everything()))
# data <-select(data, -c(dup,concat,B,t)) 

## C-2 ## OBTAIN DETAILED OUTPUT FOR EACH TRAIT 

# MAIN LOOP OVER TRAITS (k)

for (k in traitnames){ #1st trait column to last trait column
  dat <-data[data[[k]] == TRUE, ]
  assign(k,dat) #create dataframe of references, named according to trait"
  
  # SECONDARY LOOP TO FILL IN SPP (the loop for synonyms is nested within) #
  
  for (i in 1:nrow(spp)){    # loop through species names (initial unique names)
    
    list  <- as.list(strsplit(spp$synonymList[i], ",")[[1]])     # list of synonyms for species i
    
    # INITIALISE ALL VECTORS USED FOR EACH SPECIES
    
    dat$occ_titles    <- rep(FALSE, length(dat$Article.Title)) # (re-)initialise avec des valeurs = FALSE (before loop on synonyms, to append all TRUE references)
    dat$occ_abstracts <- rep(FALSE, length(dat$Article.Title)) # (re-)initialise avec des valeurs = FALSE
    dat$occ_titles_short    <- rep(FALSE, length(dat$Article.Title)) # (re-)initialise avec des valeurs = FALSE
    dat$occ_abstracts_short <- rep(FALSE, length(dat$Article.Title)) # (re-)initialise avec des valeurs = FALSE
    # 2 lignes dessous : new essai simplifier
    dat$occ            <- rep(FALSE, length(dat$Article.Title))
    dat$occ_short      <- rep(FALSE, length(dat$Article.Title))
    dat$occ_any        <- rep(FALSE, length(dat$Article.Title))
    temporary <-c("occ_titles","occ_abstracts","occ_titles_short","occ_abstracts_short","occ", "occ_short","occ_any")
    
    #####ABERRATION SORTIAT UNE VALEUR POUR SHORT QUAND AVAIT le crochait ci-dessous  pas mis pour spp$refs_fullName
    # spp$refs_fullName    <- character(nrow(spp))
    # spp$refs_shortName[i]   <- character(nrow(spp))
    # spp$refs_any[i]         <- character(nrow(spp))
    # 
    # spp$Count_refs_full  <- numeric(nrow(spp))
    # spp$Count_refs_short <- numeric(nrow(spp))
    # spp$Count_refs_any   <- numeric(nrow(spp))
    
    
    #  TERTIARY LOOP THROUGH SYNONYMS (SHOULD NOT reinitialize the above variables for code below)
    
    for (j in 1:length(list)){  # loop through synonyms
      
      taxon <- list[[j]]        # one name= synonym j of species i
      sp_split <- strsplit(taxon, " ")[[1]]   # to find abbreviated name (Genus species -> G. species)
      shorttaxon<-paste0(substr(sp_split[1], 1, 1), ". ", sp_split[2])
      
      pos_T <- grep(taxon, dat$Article.Title, ignore.case = FALSE) # a collection of indices of ref containing 'taxon' in title
      dat$occ_titles[pos_T] <- TRUE # indicate in the reference data.frame which cite the taxon
      pos_A <- grep(taxon, dat$Abstract, ignore.case = FALSE)
      dat$occ_abstracts[pos_A] <- TRUE
      dat$occ <- (dat$occ_titles)|(dat$occ_abstracts)
      
      pos_Ts <- grep(shorttaxon, dat$Article.Title, ignore.case = FALSE) 
      dat$occ_titles_short[pos_Ts] <- TRUE
      pos_As <- grep(shorttaxon, dat$Abstract, ignore.case = FALSE)
      dat$occ_abstracts_short[pos_As] <- TRUE
      dat$occ_short <- (dat$occ_titles_short)|(dat$occ_abstracts_short)
      
      dat$occ_any<- (dat$occ)|(dat$occ_short) # ref tagged as TRUE if found in full or abbreviated in title or abstract
    }
    
    # now combine all TRUE references and list their numbers in species dataframe
    Pos_T    <- grep(TRUE, dat$occ_titles, ignore.case = FALSE) 
    Pos_A    <- grep(TRUE, dat$occ_abstracts, ignore.case = FALSE) 
    Pos_Ts   <- grep(TRUE, dat$occ_titles_short, ignore.case = FALSE) 
    Pos_As   <- grep(TRUE, dat$occ_abstracts_short, ignore.case = FALSE) 
    Pos      <- grep(TRUE, dat$occ, ignore.case = FALSE)       # full name  (anywhere: title or abstract)
    Pos_short<- grep(TRUE, dat$occ_short, ignore.case = FALSE) # short name  (anywhere: title or abstract) 
    Pos_any  <- grep(TRUE, dat$occ_any, ignore.case = FALSE)   # any name  (anywhere: title or abstract) 
    
    # spp$ref_list_Title[i]              <- paste(Pos_T, collapse=", ")   # too detailed, better be omitted AND should replace Pos... by dat$referenceID[Pos...]
    # spp$ref_list_Abstract[i]           <- paste(Pos_A, collapse=", ")   # too detailed, better be omitted AND should replace Pos... by dat$referenceID[Pos...]
    # spp$ref_list_Title_shortName[i]    <- paste(Pos_Ts, collapse=", ")  # too detailed, better be omitted AND should replace Pos... by dat$referenceID[Pos...]
    # spp$ref_list_Abstract_shortName[i] <- paste(Pos_As, collapse=", ")  # too detailed, better be omitted AND should replace Pos... by dat$referenceID[Pos...]
    
    spp$refs_fullName[i]           <- paste(dat$referenceID[Pos], collapse=",")
    spp$refs_shortName[i]          <- paste(dat$referenceID[Pos_short], collapse=",")
    spp$refs_any[i]                <- paste(dat$referenceID[Pos_any], collapse=",")
    
    spp$Count_refs_full[i] <- nrow(filter(dat,occ==TRUE))
    spp$Count_refs_short[i]<- nrow(filter(dat,occ_short==TRUE))
    spp$Count_refs_any[i]  <- nrow(filter(dat,occ_any==TRUE))
  }
  # Assign to a dataframe named acording to each trait (better to do this before reordering, in case using cbind, although not recommended)
  assign(paste0("spp_ref_",k),spp)
  
  # #(OPTIONAL) Write a single excel file with 2 data sheets (sheet-1 for species, sheet-2 for references):
  # spp <- spp[order(spp$Count_refs_full,spp$Count_refs_any,decreasing=TRUE),] # Before writing, order by decreasing nb of reference
  # dat <-select(dat,-temporary) # Before writing, remove the last temporary variables created in dat to fill in spp.
  # results <-list(spp, dat) # create a list containing the two data.frames to be able to create a xls file with 2 data sheets
  # NameOutFile <-paste0("Spp_Ref_",k,".xlsx") #creates custom file name for each trait
  # write_xlsx(results, path = NameOutFile, col_names = TRUE, format_headers = TRUE,use_zip64 = FALSE)
  # # This writes an excel file with 2 data sheets (sheet-1 for species, sheet-2 for references):
  
}

# rm(dat,filenames,list,results,colstokeep,i,j,k,NameOutFile, sp_split,taxon,shorttaxon,temporary,Pos,Pos_any,Pos_short,pos_A,Pos_A,pos_T,Pos_T, pos_As, Pos_As,pos_Ts, Pos_Ts)
rm(dat,filenames,list,colstokeep,i,j,k,NameOutFile, sp_split,taxon,shorttaxon,temporary,Pos,Pos_any,Pos_short,pos_A,Pos_A,pos_T,Pos_T, pos_As, Pos_As,pos_Ts, Pos_Ts)

# listeDF (done NOT ONLY for writing the excel file) a list of data frames of spp for each trait
listeDF<-list() # 
for (j in traitnames){ # but this is used somewhere else listeDF below so keep this code
  snom <- paste("spp_ref_", j, sep="")
  dnom <- get(paste("spp_ref_", j, sep="")) # ?get: Search by name for an object
  listeDF[[snom]] <-dnom
}
# WRITE excel file with one sheet per spp x trait combination (NOT VERY USEFUL)
write_xlsx (listeDF, path = "spp_Traits_details.xlsx", col_names = TRUE, format_headers = TRUE,use_zip64 = FALSE )

#############################################################################################################
### STEP D : COMBINE ALL TRAITS -> SUMMARY TABLE PER SPECIES
#############################################################################################################

# Just need to the distinct spp dataframes for each trait

#Prepare an output excel file with :
#  sheet 1: summary per species and trait: count of references (full spp.names)
Spp_totRefs_perTrait  <- select(spp, c(scientificName, acceptedNameUsage))
#  sheet 2: list fo references full names per trait
Spp_listRefs_perTrait  <- select(spp, c(acceptedNameUsage, synonymList))
#  sheet 3: list fo references short names per trait
Spp_listRefs_perTrait_shortName  <- select(spp, c(acceptedNameUsage, synonymList))
#  sheet 4: all references (ID, authors, title, abstract,...), renamed as "data' in environment

for (i in traitnames){
  snom <- paste("spp_ref_", i, sep="")
  df <-listeDF[[snom]] # cette listeDF fut creee avant
  Spp_totRefs_perTrait[[i]] <- df$Count_refs_full  
  Spp_listRefs_perTrait[[i]] <- df$refs_fullName
  Spp_listRefs_perTrait_shortName[[i]] <- df$refs_shortName
}

# need to initialize the novel column numeric (surprising?):
Spp_totRefs_perTrait$NbTrait <-numeric(nrow(Spp_totRefs_perTrait))
for (s in 1:nrow(Spp_totRefs_perTrait)){
  f <-length(traitnames)+2
  Spp_totRefs_perTrait$NbTrait[s] <- sum(Spp_totRefs_perTrait[s,3:f]!=0)
}
# Optional (next line): ordering species list to place first those with most traits having references:
# Spp_totRefs_perTrait <-Spp_totRefs_perTrait[order(Spp_totRefs_perTrait$NbTrait,decreasing=TRUE),] 
# Finally I did not order species before writing excel, since can be done easily in excel (and may not be convenient if other sheets are not in the same order)...
write_xlsx (list(NbRefs_perTrait=Spp_totRefs_perTrait, 
                 ListRef_fullNames=Spp_listRefs_perTrait,
                 ListRef_shortNames=Spp_listRefs_perTrait_shortName,
                 References =data),
            path = "SUMMARY_spp_Traits.xlsx")

# exploration of data
which(data$referenceIDmax(data$nbT))
