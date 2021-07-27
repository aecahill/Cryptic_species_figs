#This is code to make a phylogeny and heatmap separately and then glue them together
library(ggplot2)
library(cowplot)
library(ggtree)


#traits2<-read.table("C:/Users/aecsk/OneDrive/Desktop/traits_by_phylum.txt",header=T)

#changing input file to fixed values from Jan 2021
traits<-read.table("C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs/traits_jan12.txt",header=T)
tree51data<-read.tree("C:/Users/aecsk/Documents/GitHub/Cryptic_species_figs/tree51.tre")

tree51data<-read.tree("C:/Users/acahill/Documents/GitHub/Cryptic_species_figs/tree51.tre")
traits<-read.table("C:/Users/acahill/Documents/GitHub/Cryptic_species_figs/traits_jan12.txt",header=T)

heatmap<-ggplot(data = traits, mapping = aes(x = Variable,
                                       y = Class_num,
                                       fill = z_score)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red')+
  theme_bw()+
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_blank(),axis.text.y=element_blank())+
  geom_hline(yintercept=3.5)+
  geom_hline(yintercept=8.5)+
  geom_hline(yintercept=9.5)+
  geom_hline(yintercept=12.5)+
  geom_hline(yintercept=18.5)+
  geom_hline(yintercept=19.5)+
  geom_hline(yintercept=20.5)+
  geom_hline(yintercept=22.5)+
  geom_hline(yintercept=23.5)+
  geom_hline(yintercept=26.5)+
  geom_hline(yintercept=34.5)+
  geom_hline(yintercept=36.5)+
  geom_hline(yintercept=37.5)+
  facet_grid(~Variable,switch = "x", scales = "free_x", space = "free_x")
  

tree<-ggtree(tree51data, branch.length = 'none')
  #geom_tiplab(size=3)

plot_grid(tree,heatmap)


## doing this again with 58 taxa

tree58data<-read.tree("C:/Users/acahill/Documents/GitHub/Cryptic_species_figs/tree58.tre")
traits<-read.table("C:/Users/acahill/Documents/GitHub/Cryptic_species_figs/traits58_jan14.txt",header=T)

heatmap<-ggplot(data = traits, mapping = aes(x = Variable,
                                             y = Class_num,
                                             fill = z_score)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red')+
  theme_bw()+
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank(),)+
  theme(legend.position="none") +
  theme(axis.text.x=element_blank(),axis.text.y=element_blank())+
  geom_hline(yintercept=4.5)+
  geom_hline(yintercept=9.5)+
  geom_hline(yintercept=16.5)+
  geom_hline(yintercept=17.5)+
  geom_hline(yintercept=18.5)+
  geom_hline(yintercept=23.5)+
  geom_hline(yintercept=24.5)+
  geom_hline(yintercept=25.5)+
  geom_hline(yintercept=27.5)+
  geom_hline(yintercept=28.5)+
  geom_hline(yintercept=31.5)+
  geom_hline(yintercept=39.5)+
  geom_hline(yintercept=41.5)+
  geom_hline(yintercept=43.5)+
  facet_grid(~Variable,switch = "x", scales = "free_x", space = "free_x")


tree<- ggtree(tree58data, branch.length = 'none')
#geom_tiplab(size=3)

plot_grid(tree,heatmap)

## 57 Classes, May 2021

tree57data<-read.tree("C:/Users/acahill/Documents/GitHub/Cryptic_species_figs/tree57.tre")

tree57<-ggtree(tree57data)

traits57<-read.table("C:/Users/acahill/Documents/GitHub/Cryptic_species_figs/traits_57_may24.txt",header=T)

heatmap<-ggplot(data = traits57, mapping = aes(x = Variable,
                                             y = Class_num,
                                             fill = z_score)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red')+
  theme_bw()+
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_blank(),axis.text.y=element_blank())+
  geom_hline(yintercept=4.5)+
  geom_hline(yintercept=8.5)+
  geom_hline(yintercept=15.5)+
  geom_hline(yintercept=16.5)+
  geom_hline(yintercept=17.5)+
  geom_hline(yintercept=22.5)+
  geom_hline(yintercept=23.5)+
  geom_hline(yintercept=24.5)+
  geom_hline(yintercept=26.5)+
  geom_hline(yintercept=27.5)+
  geom_hline(yintercept=30.5)+
  geom_hline(yintercept=38.5)+
  geom_hline(yintercept=40.5)+
  geom_hline(yintercept=42.5)+
  facet_grid(~Variable,switch = "x", scales = "free_x", space = "free_x")

## 58 classes, new results, July 2021

tree58data<-read.tree("C:/Users/acahill/Documents/GitHub/Cryptic_species_figs/tree58.tre")

tree58<-ggtree(tree58data)

traits58<-read.table("C:/Users/acahill/Documents/GitHub/Cryptic_species_figs/traits_58_July27.txt",header=T)

heatmap<-ggplot(data = traits58, mapping = aes(x = Variable,
                                               y = Class_num,
                                               fill = z_score)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red')+
  theme_bw()+
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.background = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_blank(),axis.text.y=element_blank())+
  geom_hline(yintercept=4.5)+
  geom_hline(yintercept=9.5)+
  geom_hline(yintercept=16.5)+
  geom_hline(yintercept=17.5)+
  geom_hline(yintercept=18.5)+
  geom_hline(yintercept=23.5)+
  geom_hline(yintercept=24.5)+
  geom_hline(yintercept=25.5)+
  geom_hline(yintercept=27.5)+
  geom_hline(yintercept=28.5)+
  geom_hline(yintercept=31.5)+
  geom_hline(yintercept=39.5)+
  geom_hline(yintercept=41.5)+
  geom_hline(yintercept=43.5)+
  facet_grid(~Variable,switch = "x", scales = "free_x", space = "free_x")
