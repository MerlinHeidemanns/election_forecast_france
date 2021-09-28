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
cat("\n")
## Check total percentage
N_less_more_100 <- df %>%
  mutate(percentage = ifelse(percentage == "-", 0, percentage),
         percentage = as.numeric(percentage)) %>%
  filter(candidate != "Abstention") %>%
  group_by(question_id) %>%
  summarize(total_percentage = sum(percentage, na.rm = TRUE)) %>%
  filter(total_percentage != 100) %>%
  nrow()
if (N_less_more_100 != 0){
  stop("Some surveys show more than 100 percent.")
}
cat("\n")
## Distribution of respondents
N_summary <- df %>%
  distinct(survey_id, electoral_list) %>%
  pull(electoral_list) %>%
  summary()
print("Distribution of N of respondents")
print(N_summary)
cat("\n")


## Distribution by candidate
dist_by_candidate <- df %>%
  filter(round == 1) %>%
  mutate(percentage = as.numeric(percentage)) %>%
  filter(!is.na(percentage)) %>%
  group_by(candidate) %>%
  summarize(
    q10 = quantile(percentage, 0.1),
    q25 = quantile(percentage, 0.25),
    q50 = quantile(percentage, 0.50),
    q75 = quantile(percentage, 0.75),
    q90 = quantile(percentage, 0.90),
    min = min(percentage),
    max = max(percentage)
  )
for (j in 1:nrow(dist_by_candidate)){
  print(dist_by_candidate[j,] %>% as.data.frame())
}










