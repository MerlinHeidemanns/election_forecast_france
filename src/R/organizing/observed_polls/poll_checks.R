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


## Check previous results
previous_election_sum <- read.csv("dta/polls_dta/election_results_2017.csv") %>%
  summarize(total = sum(percentage)) %>%
  pull(total)
if (previous_election_sum != 100){
  stop("Previous election result sum different from 100 percent.")
}





