rm(list=ls())    # clear the workspace

#=== Temur Gugushvii === 


library(ggplot2)
library(tidyverse)


NDCSurvey <- readr::read_csv("Data/ndc_survey.csv")



NDCSurveyScales <- NDCSurvey %>%
                   dplyr::select(Respondents, Organisation_Sector, Stakeholder_Involvement_en, Emission_Data, Municipality_Involvement)


#Stakeholder Involvement 

SummariseStakeholder <- NDCSurveyScales %>%
  group_by(Stakeholder_Involvement_en) %>%
  summarise(Frequency = n())


Figure3  <- ggplot(SummariseStakeholder, aes(Stakeholder_Involvement_en, Frequency)) +
  geom_col(fill = "#2f7578")+
  theme_bw()+
  theme(axis.title.x = element_text(colour="black", size=12, hjust=0.5),
        axis.title.y = element_text(colour="black", size=12, hjust=0.5),
        axis.text.x=element_text(angle = 0, hjust=0.5, size=12, colour="black"),
        axis.text.y=element_text(angle = 0, hjust=1, size=12, colour="black"),
        plot.caption = element_text(size=10, colour="black", hjust=0),
        legend.position = "bottom",
        legend.text=element_text(colour="black", size=11),
        axis.line = element_line(size = 0.2, colour = "black"))+
  scale_y_continuous(breaks=seq(0, 35, 5), limits = c(0,35))+
  labs(title="",
       subtitle ="",
       x = "",
       y = "Number of Stakeholders")


#Save the ggplot
ggsave("Visualisation/Figure3.png", 
       plot = Figure3,
       units = "mm",
       width = 170,
       height = 200) 





#EmissionData


NDCSurveyScales <- NDCSurveyScales %>%
  mutate(Emission_Data_label = recode(Emission_Data, "1" = "(1) Not available",
                                                     "2" = "(2)",
                                                     "3" = "(3)",
                                                     "4" = "(4)",
                                                     "5" = "(5) Available"))



MeanEmissionData <- NDCSurveyScales %>%
  group_by()%>%
  summarise(Mean = mean(Emission_Data),
            Median = median(Emission_Data))

SummariseEmissionData <- NDCSurveyScales %>%
  group_by(Emission_Data_label, Emission_Data) %>%
  summarise(Frequency = n())%>%
  arrange(desc(Frequency))
  


Figure11  <- ggplot(SummariseEmissionData) +
  geom_point(aes(Emission_Data_label, Frequency), color = "#3a588a")+
  geom_text(aes(Emission_Data_label, Frequency, label = Frequency), vjust = 0, nudge_y = 0.5, size = 6)+
  stat_smooth(aes(Emission_Data, Frequency),
    geom = 'area', method = 'loess', span = 7/10,
    alpha = 1/2, fill = "blue") + 
  theme_classic ()+
  theme(axis.title.x = element_text(colour="black", size=12, hjust=0.5),
        axis.title.y = element_text(colour="black", size=12, hjust=0.5),
        axis.text.x=element_text(angle = 0, hjust=0.5, size=12, colour="black"),
        axis.text.y=element_text(angle = 0, hjust=1, size=12, colour="black"),
        plot.caption = element_text(size=10, colour="black", hjust=0),
        legend.position = "bottom",
        legend.text=element_text(colour="black", size=11),
        axis.line = element_line(size = 0.2, colour = "black"))+
  annotate(geom="text", x=4.5, y=12, label="Mean = 2.63",
           color="red")+
  annotate(geom="text", x=4.479, y=11.5, label="Median = 3",
           color="red")+
  scale_y_continuous(breaks=seq(0, 15, 5), limits = c(0,15))+
  labs(title="",
       subtitle ="",
       x = "",
       y = "Number of Stakeholders")


#Save the ggplot
ggsave("Visualisation/Figure11.png", 
       plot = Figure11,
       units = "mm",
       width = 300,
       height = 200) 






#MunicipalityInvolvement


NDCSurveyScales <- NDCSurveyScales %>%
  mutate(Municipality_Involvement_Label = recode(Municipality_Involvement, "1" = "(1) Very Low",
                                      "2" = "(2)",
                                      "3" = "(3)",
                                      "4" = "(4)",
                                      "5" = "(5) Very High"))



MeanMunicipalityInvolvement <- NDCSurveyScales %>%
  group_by()%>%
  summarise(Mean = mean(Municipality_Involvement),
            Median = median(Municipality_Involvement))  


SummariseMunicipalityInvolvement <- NDCSurveyScales %>%
  group_by(Municipality_Involvement_Label, Municipality_Involvement) %>%
  summarise(Frequency = n())%>%
  arrange(desc(Frequency))



Figure6  <- ggplot(SummariseMunicipalityInvolvement) +
  geom_point(aes(Municipality_Involvement_Label, Frequency), color = "#3a588a")+
  geom_text(aes(Municipality_Involvement_Label, Frequency, label = Frequency), vjust = 0, nudge_y = 0.5, size = 6)+
  stat_smooth(aes(Municipality_Involvement, Frequency),
              geom = 'area', method = 'loess', span = 7/10,
              alpha = 1/2, fill = "blue") + 
  theme_classic ()+
  theme(axis.title.x = element_text(colour="black", size=12, hjust=0.5),
        axis.title.y = element_text(colour="black", size=12, hjust=0.5),
        axis.text.x=element_text(angle = 0, hjust=0.5, size=12, colour="black"),
        axis.text.y=element_text(angle = 0, hjust=1, size=12, colour="black"),
        plot.caption = element_text(size=10, colour="black", hjust=0),
        legend.position = "bottom",
        legend.text=element_text(colour="black", size=11),
        axis.line = element_line(size = 0.2, colour = "black"))+
  annotate(geom="text", x=4.5, y=15, label="Mean = 3.03",
           color="red")+
  annotate(geom="text", x=4.479, y=14.5, label="Median = 3",
           color="red")+
  scale_y_continuous(breaks=seq(0, 15, 5), limits = c(0,15))+
  labs(title="",
       subtitle ="",
       x = "",
       y = "Number of Stakeholders")


#Save the ggplot
ggsave("Visualisation/Figure6.png", 
       plot = Figure6,
       units = "mm",
       width = 300,
       height = 200) 



