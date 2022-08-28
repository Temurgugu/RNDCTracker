rm(list=ls())    # clear the workspace

#=== Temur Gugushvii === 

library(collapsibleTree)
library(ggplot2)
library(tidyverse)


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



ClimateActionT <- ClimateAction %>%
  filter(amount > 0) %>%
  group_by() %>%
  mutate(Sum = sum(amount)) %>%
  ungroup() %>%
  mutate(Per = amount/Sum*100) 


collapsibleTreeSummary(
  ClimateActionT,
  hierarchy = c("Goal","Institution", "Activity", "Type", "amount"),
  width = 800,
  zoomable = TRUE,
  root = "NDC",
  attribute = "Per",
  tooltip = TRUE,
  maxPercent = 71.5
  )

