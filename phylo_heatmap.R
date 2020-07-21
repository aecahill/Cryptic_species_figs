#Making a phylogeny + heatmap in Phytools

library(phytools) 

#For 58 classes, ie all cs

#read in tree, newick format
tree58data<-read.tree("C:/Users/aecsk/OneDrive/Desktop/tree58.tre")

#read in matrix of continuous traits
traits58<-as.matrix(read.table("C:/Users/aecsk/OneDrive/Desktop/traits58.txt"))

#this is a list of classes to use as row names, as required - got to be an easier way here
class58<-read.table("C:/Users/aecsk/OneDrive/Desktop/class58.txt")

#rename rows and columns
rownames(traits58)<-class58$V1
colnames(traits58)<-c("Effort","Fertilization","Residuals")

#make figure
colors<-colorRampPalette(colors=c("white","blue"))(20)
phylo.heatmap(tree58data,traits58,standardize=T,fsize=c(0.45,1,1),colors=colors)


#For 51 classes, ie only css

#read in tree, newick format
tree51data<-read.tree("C:/Users/aecsk/OneDrive/Desktop/tree51.tre")

#read in matrix of continuous traits
traits51<-as.matrix(read.table("C:/Users/aecsk/OneDrive/Desktop/traits51.txt"))

#this is a list of classes to use as row names, as required - got to be an easier way here
class51<-read.table("C:/Users/aecsk/OneDrive/Desktop/class51.txt")

#rename rows and columns
rownames(traits51)<-class51$V1
colnames(traits51)<-c("Effort","Fertilization","Residuals")

#make figure
colors<-colorRampPalette(colors=c("white","blue"))(20)
phylo.heatmap(tree51data,traits51,standardize=T,fsize=c(0.45,1,1),colors=colors)
