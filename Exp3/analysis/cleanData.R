setwd("/Users/loey/Desktop/Research/FakeNews/SnakeEyes/Exp2/analysis/")
library(tidyverse)
library(stats4)

raw <- read_csv("raw.csv")

glimpse(raw)
raw <- raw %>%
  mutate(subjID = as.factor(paste0("subj",str_pad(group_indices(.,subjID),3,pad="0"))),
         catchQuestion = ifelse(catchResponse == -1, "NA", catchQuestion),
         catchQuestionAbbr = case_when(
           catchQuestion == "What number did your opponent report rolling?" ~ "reportOpp",
           catchQuestion == "What number did you roll?" ~ "samplePla"
         ))

bads <- raw %>% filter(catchQuestionAbbr %in% c("reportOpp", "samplePla")) %>% group_by(subjID) %>% 
  summarise(accuracy = mean(catchResponse == catchKey)) %>% 
  mutate(badsubject = accuracy < .75)


bads %>%
  filter(badsubject) %>%
  nrow()

length(unique(raw$subjID))
raw <- raw %>%
  left_join(bads)

raw %>%
  filter(badsubject) %>%
  .$subjID %>%
  unique()

df <- raw %>%
  filter(!badsubject, exptPart == "trial")


