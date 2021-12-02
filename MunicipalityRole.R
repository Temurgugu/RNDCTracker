rm(list=ls())    # clear the workspace

#=== Temur Gugushvii === 


library(ggplot2)
library(tidyverse)


ndc_survey <- readr::read_csv("data/ndc_survey.csv")

ndc_survey_Role <- ndc_survey %>%
  select(Respondents:Municipality_Role_CAP_7)
               


ndc_survey_Role <- ndc_survey_Role %>%
              pivot_longer(cols = starts_with("Municipality_Role_CAP"),
                           names_to = "Municipality_Role",
                           names_prefix = "Municipality_Role_CAP",
                           values_to = "Sectors",
                           values_drop_na = TRUE) 


ndc_survey_Role <- ndc_survey_Role %>%
  mutate(Sectors_En = recode(Sectors, "სოფლის მეურნეობა"="Agriculture",
                                      "ენერგიის გენერაცია და გადაცემა"="Energy Generation & Transmission",
                                      "ტრანსპორტი"="Transport",
                                      "შენობების ენერგოეფექტურობის ზრდა"="Buildings",
                                      "მრეწველობა"="Industry",
                                      "ნარჩენების მართვა"="Waste Management",
                                      "ტყის მდგრადი მართვა"="Forestry"))


#Question_ge:მუნიციპალიტეტების წვლილი/როლი/პასუხისმგებლობა NDC-ის და/ან კლიმატის სამოქმედო გეგმის (CAP) რომელ სექტორულ მიმართულებებში უნდა იყოს გაძლიერებული? 

#Question_en:

Summarise_Sectors <- ndc_survey_Role %>%
  group_by(Sectors_En) %>%
  summarise(Frequency = n())


MunicipalityRole <- ggplot(Summarise_Sectors, aes(Sectors_En, Frequency)) +
  geom_col(fill = "#2f7578")+
  geom_hline(yintercept = 25)+
  geom_hline(yintercept = 29)+
  theme_bw()+
  theme(axis.title.x = element_text(colour="black", size=12, hjust=0.5),
        axis.title.y = element_text(colour="black", size=12, hjust=0.5),
        axis.text.x=element_text(angle = 90, hjust=0.5, size=12, colour="black"),
        axis.text.y=element_text(angle = 0, hjust=1, size=12, colour="black"),
        plot.caption = element_text(size=10, colour="black", hjust=0),
        legend.position = "bottom",
        legend.text=element_text(colour="black", size=11),
        axis.line = element_line(size = 0.2, colour = "black"))+
  coord_flip()+
  scale_y_continuous(breaks=seq(0, 35, 5), limits = c(0,35))+
  labs(title="",
       subtitle ="",
       x = "NDC Sectors",
       y = "Number of Stakeholders")


#Save the ggplot
ggsave("visualization/MunicipalityRole.png", 
       plot = MunicipalityRole,
       units = "mm",
       width = 300,
       height = 200) 
  