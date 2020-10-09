# AstroKnights
# by Rafael S. de Souza

# Gaussian Mixture Model
# importing useful libraries
require(mclust);require(ggplot2)
require(dplyr)

# reading the catalogue
AGN <- read.csv("../data/galaxies.csv",header=T)

# Subsample for quick analysis
index <- sample(seq_len(nrow(AGN)),replace=F, size = 10000)
AGN_short <- AGN[index,]

# GMM clustering
BPT <- AGN_short %>% select(c("xx_BPT_WHAN","yy_BPT"))
CLUST <- Mclust(BPT ,G = 1:4)
BPT <- BPT %>% mutate(type = as.factor(CLUST$classification))
el <- GMM.ellipses(CLUST)
#-----------------------
# BPT PLOT
#-----------------------
xx = seq(-4, 0.0, 0.01)
Ka = 0.61 / (xx - 0.05) + 1.30
gKa <- data.frame(xx,Ka)
#-----------------------
xx1 = seq(-4, 0.4, 0.01)
Ke = 0.61 / (xx1 - 0.47) + 1.19
gKe <- data.frame(xx1,Ke)

#-----------------------
xx2 = seq(-0.43, 5, 0.01)
Sey = 1.05 * xx2 + 0.45
gSey <- data.frame(xx2,Sey)



ggplot(data=BPT,aes(x=xx_BPT_WHAN,y=yy_BPT))+
  geom_point(aes(color=type))+
  xlab(expression(paste('log ([NII]/H', alpha, ')'))) +
  ylab(expression(paste('log ([OIII]/H', beta, ')'))) +
  scale_fill_manual(values = c("#66c2a5","#fc8d62","#8da0cb","#e78ac3"))+
  scale_color_manual(values = c("#66c2a5","#fc8d62","#8da0cb","#e78ac3"))+
  theme_bw() + 
  geom_line(aes(x=xx,y=Ka),data=gKa,size=1.25,linetype="dashed",color="gray25")+
  geom_line(aes(x=xx1,y=Ke),data=gKe,size=1.25,linetype="dotted",color="gray25")+
  geom_line(aes(x=xx2,y=Sey),data=gSey,size=0.75,linetype="dotdash",color="gray25")+
  coord_cartesian(xlim=c(-1.8,1.3),ylim=c(-1.5,1.55))+
  geom_polygon(data=el,aes(x=xval,y=yval,group=classification,color=classification,
                                 fill=classification),alpha=0.5) +
  theme(legend.position = "none",plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(vjust=0.75),
        axis.title.x=element_text(vjust=-0.25),
        text = element_text(size=20))
