## Libraries
library(tidyverse)
## Load data
df <- read_csv("dta/polls_dta/2022_polls.csv")
## Number of polls
N_survey_questions <- df %>%
  distinct(question_id) %>%
  nrow()
N_surveys<- df %>%
  distinct(survey_id) %>%
  nrow()
cat("There are", N_survey_questions, "and", N_surveys, "surveys.")
## Check total percentage
df %>%
  mutate(percentage = ifelse(percentage == "-", 0, percentage),
         percentage = as.numeric(percentage)) %>%
  filter(candidate != "Abstention") %>%
  group_by(question_id) %>%
  summarize(total_percentage = sum(percentage, na.rm = TRUE)) %>%
  filter(total_percentage != 100)
