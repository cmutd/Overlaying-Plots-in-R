library(ggplot2)
library(tidyverse)

rm(list=ls())
options(stringsAsFactors = F)

datafr <- read.table("test_data.txt",sep='\t',header=T)
names(datafr)[2] <- "Number of Genes"


datafr <- datafr%>% 
  mutate(Term = as.factor(Term) %>% fct_reorder(`Number of Genes`),
         `log10(PValue)`=-log10(datafr$P.value))


png("./figure/single_color_pathway.png",width = 16, height = 12, units = 'in', res = 300)
datafr%>% 
  ggplot(aes(x=Term))+
  geom_col(aes( y = `Number of Genes`),fill ="dodgerblue4")+
  geom_line(aes( y =10*`log10(PValue)`,group=1),color="red",size=1)+  # apply an arithmetical transformation to the scale of the axis. The scale factor depends on your dataset
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "-log10(PValue)"))+ ## adds the secondary Y-axis
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust=0.5,vjust=5),  # center plot title, move up vertically
        text = element_text(face = "bold.italic", color = "dodgerblue4", size = 16), # styles of font writing
        plot.margin = margin(1,1,1,1,"cm"),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = 0.2),
        axis.text.y=element_text(margin=margin(0,-40,0,0)))+   #top,right,bottom,left)
  coord_flip()+
  ggtitle("The Pathway analysis")
dev.off()

