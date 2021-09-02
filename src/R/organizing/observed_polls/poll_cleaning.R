#' poll_cleaning
## Libraries
library(tidyverse)
## Load and check data
source("src/R/organizing/observed_polls/poll_checks.R")
## Remove specific questions
remove <- c("Abstention",
            "Un candidate d'extrême-gauch",
            "François Hollande")
remove_question_ids <- df %>%
  filter(candidate %in% remove) %>%
  pull(question_id) %>%
  unique()
df <- df %>%
  filter(!question_id %in% remove_question_ids)
## Assign value to less than 0.5 observations and rescale percentages
df <- df %>%
  mutate(
    percentage = ifelse(percentage == "-", "0.25", percentage),
    percentage = as.numeric(percentage)
  ) %>%
  group_by(question_id) %>%
  mutate(percentage = percentage/sum(percentage)) %>%
  ungroup()
## Unique identifiers for candidates
#' Load candidates from previous rounds
#' Create dataframe with identifiers and candidates
#' Save
#' Merge into original dataframe
previous_candidates <- read.csv("dta/polls_dta/election_results_2017.csv") %>%
  pull(candidate)
current_candidates <- df %>%
  arrange(-percentage) %>%
  distinct(candidate) %>%
  pull(candidate)
previous_candidates_only <- previous_candidates[!previous_candidates %in% current_candidates]
current_candidates_only <- current_candidates[!current_candidates %in% previous_candidates]
both_candidates <- current_candidates[current_candidates %in% previous_candidates]
all_candidates <- c(current_candidates_only, both_candidates, previous_candidates) %>%
  unique()
all_candidates <- data.frame(candidate = all_candidates,
                             candidate_id = 1:length(all_candidates))
write.csv(all_candidates, "dta/polls_dta/candidate_identifiers.csv")
df <- df %>%
  left_join(all_candidates,
            by = "candidate")


## Unique identifiers for pollsters
#' Create data frame with identifiers and candidates
#' Save
#' Merge into original data frame
all_pollster <- df %>%
  distinct(pollName) %>%
  mutate(pollster_id = 1:n())
write.csv(all_pollster, "dta/polls_dta/pollster_identifiers.csv")
df <- df %>%
  left_join(all_pollster,
            by = "pollName")

## Dates
#' Create dates from strings
#' Assign time points when the poll took place
#' Start date is first observed poll in dataset
#' Poll date is end of poll
df <- df %>%
  mutate(start_date = as.Date(start, "%Y_%m_%d"),
         end_date = as.Date(end, "%Y_%m_%d")) %>%
  mutate(t = as.integer(difftime(end_date, min(end_date), units = "days")))


## Dates
#' Create dataframe with unit time skips
#' Save and merge into original
#' Create lag, remove na in beginning, save
df_time <- df %>%
  distinct(end_date, t) %>%
  arrange(t) %>%
  mutate(t_unit = 1:n())
write.csv(df_time %>%
            dplyr::select(-t),
          "dta/polls_dta/time_identifiers.csv")
df <- df %>%
  left_join(df_time)

t_diff <- df_time %>%
  mutate(t_diff = t - lag(t)) %>%
  filter(!is.na(t_diff)) %>%
  pull(t_diff)
write_rds(t_diff, "dta/polls_dta/t_diff.Rds")


## Turn percentages into numbers
#' Multiply percentage with number of respondents on the electoral list
#' Round down
df <- df %>%
  mutate(y = floor(electoral_list * percentage))


## Label previous election results
#' Load
#' Merge with candidate identifiers
read.csv("dta/polls_dta/election_results_2017.csv") %>%
  left_join(all_candidates) %>%
  write.csv("dta/polls_dta/elections_results_2017_w_id.csv")









