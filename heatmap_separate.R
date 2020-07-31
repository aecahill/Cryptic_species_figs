#This is code to make a phylogeny and heatmap separately and then glue them together
library(ggplot2)
library(cowplot)
library(ggtree)


traits<-read.table("C:/Users/aecsk/OneDrive/Desktop/traitstry.txt",header=T)

heatmap<-ggplot(data = traits, mapping = aes(x = Variable,
                                       y = Class,
                                       fill = Value)) +
  geom_tile() +
  theme_bw()+
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())+
  theme(axis.text.x=element_blank())+
  facet_grid(~Variable,switch = "x", scales = "free_x", space = "free_x")

tree<- ggtree(tree58data, branch.length = 'none')+
  #geom_tiplab(size=3)

plot_grid(tree,heatmap)