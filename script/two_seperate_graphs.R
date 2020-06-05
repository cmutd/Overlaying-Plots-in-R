library(ggplot2)
library(tidyverse)

options(stringsAsFactors = F)

################################################## step1: clean data ##################################################
datafr <- read.table("test_data.txt",sep='\t',header=T)
names(datafr)[2] <- "Number of Genes"


datafr <- datafr%>% 
  mutate(Term = as.factor(Term) %>% fct_reorder(`Number of Genes`),
         `log10(PValue)`=-log10(datafr$P.value))

################################################## step2: draw the graph ##################################################

# gene plot -------------------------------------------------------------
gene_plot <- datafr%>% ggplot(aes(x = `Number of Genes`, y = Term)) +
  geom_col(fill = "dodgerblue4", width = 0.35) + 
  geom_text(
    aes(label = `Number of Genes`, hjust = -0.2), 
    col = "dodgerblue4", size = 3.5
  ) + 
  scale_x_continuous(breaks = seq(0, 51,5),
                     limits = c(0, 51),
                     position = "top") + 
  labs(
    x = "Number of Genes"
  ) + 
  theme(
    panel.background = element_blank(), 
    plot.margin = margin(1,1,1,1,"cm"),
    plot.title = element_text(hjust=0.5,vjust=5), 
    axis.ticks.y = element_blank(), 
    axis.title.y = element_blank(),
    #axis.text.y=element_text(margin=margin(0,-40,0,0)),
    axis.line.x = element_line(size = 0.4),
    text = element_text(face = "bold", size = 14)
  )




# p.value plot -------------------------------------------------------------
p_value_plot <- datafr%>% 
  #mutate(group=1)%>%
  ggplot(aes(x = `log10(PValue)`, y = Term))+ 
      geom_line(group=1,color="dodgerblue4",size=1)+
     scale_x_continuous(position = "top",breaks = seq(0, 5, 1),
                        limits = c(0, 5)) + 
     labs(
         x = "-log10(PValue)", 
         y = "Genes in Pathway"
       ) + 
     theme(
         axis.line.x = element_line(size = 0.4,color="dodgerblue4"), 
         plot.margin = margin(1,1,1,1,"cm"),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         panel.background = element_blank(),
         text = element_text(face = "bold", size = 14)
       )

################################################## step3: joining plots ##################################################
png("./figure/two_seperate_graphs_pathway.png",width = 16, height = 12, units = 'in', res = 300)
final_plot <- grid.arrange(
  gene_plot, p_value_plot, nrow = 1, 
  top = textGrob("The Pathway Graph", gp = gpar(fontsize = 20)))
dev.off()


