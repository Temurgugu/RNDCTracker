rm(list=ls())    # clear the workspace

#=== Temur Gugushvii === 


library(ggplot2)
library(tidyverse)


Stakeholder <- readr::read_csv("Data/stakeholder_updated.csv",
                                 col_types = cols(Abbreviation = "c",
                                                  Interest = "n",
                                                  Influence = "n",
                                                  Grouped = "c",
                                                  Level = "c"))



Figure1 <- Stakeholder %>%
                           ggplot(aes(Influence, Interest, color=Grouped, group=Grouped)) +
                           geom_vline(xintercept = 5)+
                           geom_hline(yintercept = 5)+
                           theme_classic()+
                           #geom_point(aes(size = Influence),  alpha = 0.6)+
                           geom_point(size = 3, alpha = 0.6)+
                           geom_text(aes(label=Abbreviation), vjust = 0, nudge_y = 0.2, 
                                     show.legend = FALSE, color = "black",
                           data = . %>% filter(Abbreviation %in% c("GIZ", "MEPA", "CoM", 
                                                    "Edu-institutions", "HPP (Small)")))+
                           theme(plot.title=element_text(colour="black", size=16),
                                 legend.position="right")+
                           annotate("text", x = 9, y = 10, label = "Key Players \nEngage & Collaborate")+
                           annotate("text", x = 9, y = 0.5,  label = "Context Setters \nConsult")+
                           annotate("text", x = 0.5, y = 0.5, label = "Crowd \nInform")+
                           annotate("text", x = 0.8, y = 10, label = "Subject \nEmpower & Involve")+
                           scale_y_continuous(breaks=seq(0, 10, 2), limits = c(0,10))+
                           scale_x_continuous(breaks=seq(0, 10, 2),  limits = c(0,10))+
                           scale_color_manual(name = "Stakeholder Type",
                                              labels = c("Academic institutions", "Civil society (Local)", "Donors", "Governmental", "NGO", "Private sector"),
                                              values = c("#4287f5", "#d442f5", "#42f5ce", "#1f6656", "#6e5e1e", "#a34955"),
                                              guide = guide_legend(override.aes = list(size = 5)))+
                           scale_size_continuous(breaks=seq(0, 10, 2), 
                                                 limits = c(0,10), name = "Influence Rate", range = c(-5,10))+
                           labs(title="Interest–influence matrix for NDC Stakeholders",
                                subtitle ="")
  
  


#Save the ggplot
ggsave("Visualisation/Figure1.png", 
       plot = Figure1,
       width = 24, 
       height = 20, 
       units = "cm")  


