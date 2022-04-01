rm(list=ls())    # clear the workspace

#=== Temur Gugushvii === 


library(ggplot2)
library(tidyverse)


ndc_survey <- readr::read_csv("Data/ndc_survey.csv")



ndc_survey_Policy_Document <- ndc_survey %>%
                   select(Respondents:Stakeholder_Involvement_en,
                          Organisation_Sector:Document_12)

ndc_survey_Org <- ndc_survey_Policy_Document %>%
  group_by(Organisation_Sector) %>%
  summarise(Frequency = n()) 


ndc_survey_Policy_Document <- ndc_survey_Policy_Document %>%
  pivot_longer(cols = starts_with("Document"),
               names_to = "Document",
               names_prefix = "Document",
               values_to = "Policy_Document",
               values_drop_na = TRUE) 


ndc_survey_Policy_Document <- ndc_survey_Policy_Document %>%
  mutate(Policy_Document_En = recode(Policy_Document, "განათლებისა და მეცნიერების ერთიანი სტრატეგია"="Unified Strategy of Education and Science",
                             "და სხვა პოლიტიკის დოკუმენტში"="And other policy documents",
                             "ენერგოეფექტურობის ეროვნული სამოქმედო გეგმა"="National Renewable Energy Action Plan",
                             "მაღალმთიანი დასახლებების განვითარების სტრატეგია"="Strategy for Development of High Mountain Settlements",
                             "მდგრადი განვითარების ეროვნული პროგრამა"="National Program for Sustainable Development",
                             "მუნიციპალიტეტების სამოქმედო გეგმები"="Municipal Action Plans",
                             "რეგიონული განვითარების სტრატეგია"="Regional Development Strategy",
                             "საქართველოს დაბალემისიანი განვითარების სტრატეგია"="Low emission development",
                             "სოფლის მეურნეობისა და სოფლის განვითარების სტრატეგია"="Agriculture and Rural Development Strategy",
                             "საქართველოს ენერგეტიკის და კლიმატის ეროვნული ინტეგრირებული გეგმა"="And other policy documents",
                             "საქართველოს ენერგეტიკული პოლიტიკის კონცეფცია"="Energy policy concept",
                             "ტურიზმის განვითარების სტრატეგია" = "Tourism Development Strategy",
                             "ჯანდაცვის სექტორის განვითარების სტრატეგია"="Health sector development strategy"))


Summarise_Sectors <- ndc_survey_Policy_Document %>%
  group_by(Policy_Document_En, Organisation_Sector) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  pivot_wider(names_from = Organisation_Sector, values_from = Frequency)

