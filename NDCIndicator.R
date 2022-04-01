rm(list=ls())    # clear the workspace

#=== Temur Gugushvii === 


library(ggplot2)
library(tidyverse)
library(viridis)

#Import data 

CAPIndicators <- readr::read_csv("Data/CAPIndicator.csv",
                                 col_types = cols(Goal = "c",
                                                  GgCO2e = "n",
                                                  Year = "c",
                                                  Period_ge = "c",
                                                  Period_en = "c",
                                                  Source_ge = "c",
                                                  Source_en = "c"))

CAPIndicators <- CAPIndicators %>%
  filter(Period_en != "Reference scenario projection")



CAPIndicatorsgroup <- CAPIndicators %>%
  filter(Period_en == "Historical") %>%
  group_by(Year) %>%
  summarise(total = sum(GgCO2e))


#Visualization 

Figure2 <- ggplot(CAPIndicators, aes(Year, GgCO2e, fill=Goal, group=Goal)) +
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  geom_hline(yintercept = 29500)+
  geom_curve(aes(x = 4.5, y = 35000, xend = 4, yend = 30000),
             arrow = arrow(length = unit(0.03, "npc"), 
                           type="closed"),
             size = 0.2,
             angle = 120)+
  annotate("text", x = 4.85, y = 35000, label = "NDC Unconditional")+
  hrbrthemes::theme_ipsum() +
  theme(axis.title.x = element_text(colour="black", size=11, hjust=0.5),
        axis.title.y = element_text(colour="black", size=11, hjust=0.5),
        axis.text.x=element_text(angle = 90,  hjust=1, size=11, colour="black"),
        axis.text.y=element_text(size=11),
        plot.caption = element_text(size=9, colour="black", hjust=1),
        plot.title=element_text(colour="black", size=14,  hjust=0),
        legend.title=element_text(size=11),
        legend.text=element_text(size=11),
        legend.position="bottom",
        legend.key.width = unit(3, "line"))+
  scale_y_continuous(breaks=seq(-70000, 40000, 7000))


 

#Save the ggplot
ggsave("Visualisation/Figure2.png", 
       plot = Figure2,
       units = "mm",
       width = 300,
       height = 200) 
