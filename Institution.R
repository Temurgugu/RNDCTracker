rm(list=ls())    # clear the workspace

#=== Temur Gugushvii === 
#=== Responsible Institution ===


library(collapsibleTree)
library(ggplot2)
library(tidyverse)
library(viridis)
library(ggwordcloud)

Institution <- readr::read_csv("Data/ClimateAction.csv",
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



Institution_inst <- Institution %>%
                    dplyr::select(Goal, Objective, Activity, Abbreviation) %>%
                    dplyr::distinct() %>%
                    group_by(Goal, Abbreviation) %>%
                    summarise(Value = n())
                      

Figure5   <- ggplot(Institution_inst, aes(Abbreviation , Goal, fill =Value)) +
                    geom_tile() +
                    geom_text(aes(label = Value, color = Value), show.legend = FALSE)+
                    scale_color_viridis(option = "inferno", direction = 1,
                                        begin = 0, end = 1, discrete=FALSE)+
                    scale_fill_viridis(option = "viridis", direction = -1,
                                       begin = 0, end = 1, discrete=FALSE)+
                    theme(axis.title.x = element_text(colour="black", size=11, hjust=0.5),
                          axis.title.y = element_text(colour="black", size=11, hjust=0.5),
                          axis.text.x=element_text(angle = 90,  hjust=1, size=11, colour="black"),
                          axis.text.y=element_text(size=11, colour="black"),
                          plot.caption = element_text(size=9, colour="black", hjust=1),
                          plot.title=element_text(colour="black", size=14,  hjust=0),
                          legend.title=element_text(size=11),
                          legend.text=element_text(size=11),
                          legend.position="bottom",
                          legend.key.width = unit(3, "line"))+
                    xlab("Responsible Institutions")+
                    ylab("Nationally Determined Contributions")+ 
                    labs(title = "Number of Activities by Responsible Institution and NDC",
                         subtitle ="",
                         caption = "Source:Climate Change Action Plan for 2021-2023")+
                    labs(fill = "Number of Activitities")+
                    scale_x_discrete()



ggsave("Visualisation/Figure5.png",  
       InstitutionChart, 
       width=8.81, 
       height=6.47)  


