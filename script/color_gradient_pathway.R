library(ggplot2)
library(tidyverse)

rm(list=ls())
options(stringsAsFactors = F)

datafr <- read.table("test_data.txt",sep='\t',header=T)
names(datafr)[2] <- "Number of Genes"


datafr <- datafr%>% 
  mutate(Term = as.factor(Term) %>% fct_reorder(`Number of Genes`),
         `log10(PValue)`=-log10(datafr$P.value))

png("./figure/color_graident_pathway.png",width = 16, height = 12, units = 'in', res = 300)
datafr%>% 
  ggplot(aes(x=Term))+
  geom_col(aes(y=`Number of Genes`, fill=`Number of Genes`))+
  geom_line(aes( y =10*`log10(PValue)`),group=1,color="red",size=1.5)+
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "-log10(PValue)"))+ # apply an arithmetical transformation to the scale of the axis. The scale factor depends on your dataset
  theme(panel.background = element_blank(),
        plot.margin = margin(1,1,1,1,"cm"),
        plot.title = element_text(hjust=0.5,vjust=5),  # center plot title, move up vertically
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y=element_text(margin=margin(0,-40,0,0)), #adjust the distance between labels and bar
        axis.line.x = element_line(size = 0.2),
        text = element_text(face = "bold", size = 14), ## styles of font writing
        legend.margin = margin(28, 16, 16, 16))+   #top,right,bottom,left
  coord_flip()+
  scale_fill_gradient(low = "lightblue", high = "royalblue4",name="-log10(PValue)")+   #midpoint = median(datafr$`Number of Genes`)
  geom_text(
    aes(y=`Number of Genes`,label =`Number of Genes`, hjust = -0.4), 
    col = "dodgerblue4", size = 4
  )+
  ggtitle("The Pathway analysis")
dev.off()