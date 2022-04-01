rm(list=ls())    # clear the workspace

#=== Temur Gugushvii === 

library(collapsibleTree)
library(ggplot2)
library(tidyverse)
library(treemapify)


ClimateAction <- readr::read_csv("Data/ClimateAction.csv",
                                 col_types = cols(Goal = "c",
                                                  Objective = "c",
                                                  Activity = "c",
                                                  Institution = "c",
                                                  Abbreviation = "c",
                                                  Budget = "c",
                                                  Type_origin = "c",
                                                  Type = "c",
                                                  amount = "n",
                                                  Organization = "c"))



ClimateAction_badget <- ClimateAction %>%
                        filter(amount > 0) %>%
                        group_by(Goal, Type)  %>%
                        summarise(Amount = sum(amount)) %>%
                        group_by(Goal) %>%
                        mutate(Sum = sum(Amount)) %>%
                        ungroup() %>%
                        mutate(Per = Amount/Sum*100)%>%
                        mutate(per2 = round(Per, 1))%>%
                        mutate(per3 = paste0(per2, " %"))

ClimateAction_col <- ClimateAction_badget %>%
                     group_by(Goal)  %>%
                     summarise(Amount = sum(Amount)/3.65)
  
col <- ggplot(ClimateAction_col, aes(Goal, Amount, fill = "#f59e42"))+
       geom_col(fill = "#7d6625")+
  theme_minimal()+
  coord_flip()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y=element_text(angle = 0, hjust=1, size=7, colour="black"),
        axis.text.x=element_blank(),
        plot.title=element_text(colour="black", size=10, hjust=-3),
        legend.position = "none")+
  labs(title = "Amount of budget by NDC's mitigation goals")+
  annotate("text", x = 1, y = 250000000, label = c("957,205.5 EUR"), size = 3) +
  annotate("text", x = 2, y = 300000000, label = c("45,914,952.3 EUR"), size = 3)+ 
annotate("text", x = 3, y = 340000000, label = c("1,553,721,095.9 EUR"), size = 3) +
  annotate("text", x = 4, y = 300000000, label = c("7,111,941.9 EUR"), size = 3)+
annotate("text", x = 5, y = 300000000, label = c("9,207,544.1 EUR"), size = 3) +
  annotate("text", x = 6, y = 330000000, label = c("485,988,339.7 EUR"), size = 3)+
annotate("text", x = 7, y = 300000000, label = c("69,420,799.5 EUR"), size = 3)

  
Figure4 <- ggplot(ClimateAction_badget, aes(area = Amount, fill = Type)) +
           geom_treemap()+
           geom_treemap_text(aes(label = per3), fontface = "bold", colour = "white", place = "centre", grow = FALSE)+
           theme(plot.caption=element_text (hjust = 0,  face = "italic"))+
           labs(title = "Percentage distribution of finances by NDC’s mitigation goals and types of financial support",
                caption = "Note: Administrative costs by Government are not included")+
           scale_y_continuous(labels = scales::percent)+
           facet_wrap(vars(Goal))+
  patchwork::inset_element(col, left = 0.33, bottom = -0.03, right = 1, top = 0.35)


ggsave("Visualisation/Figure4.png", Figure4, width=8.81, height=6.47)  
  