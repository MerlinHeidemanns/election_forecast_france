#' poll_cleaning
## Libraries
library(tidyverse)
## Load and check data
source("src/R/organizing/observed_polls/poll_checks.R")
## Remove specific questions
remove <- c("Abstention",
            "Un candidate d'extrême-gauch",
            "François Hollande",
            "Le ou la candidate vainqueur de la primaire écologiste")
remove_question_ids <- df %>%
  filter(candidate %in% remove) %>%
  pull(question_id) %>%
  unique()
df <- df %>%
  filter(!question_id %in% remove_question_ids)
df <- df %>%
  group_by(survey_id) %>%
  mutate(survey_id = cur_group_id())

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
candidates <- df %>%
  arrange(-percentage) %>%
  distinct(candidate) %>%
  pull(candidate)
candidates <- data.frame(candidate = candidates,
                             candidate_id = 1:length(candidates))
write.csv(candidates, "dta/polls_dta/candidate_identifiers.csv")
df <- df %>%
  left_join(candidates,
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

## Change question identifier to numerique
#' Group
#' Create identifier
#' Ungroup
df <- df %>%
  group_by(question_id) %>%
  mutate(question_id = cur_group_id()) %>%
  ungroup()

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


## Count number of polls in folder and number of surveys in df
N_files <- list.files("dta/polls_reports") %>%
  length()
N_df <- df %>%
  distinct(survey_id) %>%
  nrow()
if (N_files > N_df){
  #stop("Warning: There are surveys not included in the dataframe.")
}


## Save
write.csv(df, "dta/polls_dta/2020_polls_clean.csv")


## Clean
rm(list = ls())