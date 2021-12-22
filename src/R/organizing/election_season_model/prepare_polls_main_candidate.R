################################################################################
## Title: Prepare polls for election season model
################################################################################
## Empty environment
rm(list = ls())
################################################################################
## Load libraries
library(tidyverse)
################################################################################
## Vectors
bloc_vector <- c("Abstention",
                 "Gauche radicale et extreme gauche",
                 "Gauche",
                 "Ecologism",
                 "Centre",
                 "Droite",
                 "Droite radicale et extreme droite")
################################################################################
## Import data
df_past <- read_csv("dta/polls_dta/past_polls_clean.csv")
candidates_blocs <- read_csv("dta/france_classification/candidate_bloc_cross_walk.csv")
df_current <-
  read_csv("dta/polls_dta/polls_2020/polls_position_polls_2022_raw.csv")
election_years <- df_past %>% pull(election_year) %>% unique() %>% sort()
election_years <- c(election_years, max(election_years) + 5)
election_results <- read_csv("dta/polls_dta/election_results_clean.csv")
election_dates <- read_csv("dta/polls_dta/election_dates.csv")
################################################################################
## Prepare past polls
df_past <- df_past %>%
  #' subset on time frames
  left_join(election_dates,
            by = c("election_year" = "year")) %>%
  mutate(period_start_day = date_first_round - (7 * 16),
         period_end_day = date_first_round
  ) %>%
  filter(start_day > period_start_day,
         start_day < period_end_day) %>%
  left_join(election_results %>%
              select(-party, -bloc) %>%
              rename(election_result_percentage = percentage),
              by = c("candidate" = "candidate",
                     "election_year" = "year")) %>%
  #filter(!is.na(election_result)) %>%
  group_by(poll_id) %>%
  ungroup() %>%
  #' survey_id
  group_by(pollName, sampleSize, start_day, end_day, election_year) %>%
  mutate(survey_id = cur_group_id()) %>%
  #' time
  mutate(date = as.Date(ifelse(is.na(end_day),
                               as.character(start_day),
                               as.character(end_day)))) %>%
  #' election_id
  mutate(election_id = match(election_year, election_years)) %>%
  #' join blocs
  left_join(candidates_blocs %>%
              distinct(bloc, party,
                       candidate, election_year)) %>%
  mutate(party = ifelse(bloc == "Abstention", "Abstention", party)) %>%
  group_by(election_year, party, poll_id) %>%
  mutate(z1 = ifelse(!is.na(election_result_percentage), 1, 0)) %>%
  group_by(election_year, bloc) %>%
  mutate(main_candidate =
           ifelse(election_result_percentage == max(election_result_percentage, na.rm = TRUE),1,0),
         main_candidate = ifelse(is.na(main_candidate), 0, main_candidate)) %>%
  mutate(bloc = ifelse(!main_candidate, "Autre", bloc)) %>%
  ungroup() %>%
  group_by(poll_id, election_year, pollName, election_id, period_start_day, period_end_day, bloc, start_day) %>%
  summarize(y = sum(y)) %>%
  filter(!is.na(bloc)) %>%
  mutate(z = 1) %>%
  group_by(poll_id) %>%
  mutate(sum_z = sum(z)) %>%
  filter(sum_z > 3) %>%
  select(-sum_z, -z) %>%
  filter(!poll_id %in% c("2002_2_25", "2002_2_10")) %>%
  group_by(poll_id) %>%
  mutate(#poll_id = cur_group_id(),
         t = ceiling((as.integer(difftime(start_day, period_start_day) + 1)/7))) %>%
  ungroup()
################################################################################
## Write
write_csv(df_past,
          file = "dta/polls_dta/election_season_model/poll_input.csv")
################################################################################