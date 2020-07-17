#Making a phylogeny + heatmap in Phytools

library(phytools) 

#read in tree, newick format
treedata<-read.tree("C:/Users/aecsk/OneDrive/Desktop/tree.tre")

#read in matrix of continuous traits
traits<-as.matrix(read.table("C:/Users/aecsk/OneDrive/Desktop/traits.txt"))

#this is a list of classes to use as row names, as required - got to be an easier way here
classes<-read.table("C:/Users/aecsk/OneDrive/Desktop/classnames.txt")

#rename rows and columns
rownames(traits)<-classes$V1
colnames(traits)<-c("Effort_m1","Ferti","Residus")

#make figure
phylo.heatmap(treedata,traits,standardize=T,fsize=c(0.5,1,1))

