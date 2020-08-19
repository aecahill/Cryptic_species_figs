#This is code to make a phylogeny and heatmap separately and then glue them together
library(ggplot2)
library(cowplot)
library(ggtree)


traits<-read.table("C:/Users/aecsk/OneDrive/Desktop/traits_by_phylum.txt",header=T)

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
  

tree<- ggtree(tree51data, branch.length = 'none')
  #geom_tiplab(size=3)

plot_grid(tree,heatmap)