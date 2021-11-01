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
                 "Centre",
                 "Droite",
                 "Droite radicale et extreme droite")
################################################################################
## Import data
df_past <- read_csv("dta/polls_dta/past_polls_clean.csv")
candidates_blocs <- read_csv("dta/polls_dta/candidate_bloc_cross_walk.csv")
df_current <-
  read_csv("dta/polls_dta/polls_2020/polls_position_polls_2022_raw.csv")
election_years <- df_past %>% pull(election_year) %>% unique() %>% sort()
election_years <- c(election_years, max(election_years) + 5)
election_results <- read_csv("dta/polls_dta/election_results_clean.csv")
################################################################################
## Prepare past polls
df_past <- df_past %>%
  #' subset on time frames
  mutate(period_start_day = as.Date(paste(as.character(election_year),
                                          "/01/01",
                                          sep = "")),
         period_end_day = as.Date(paste(as.character(election_year),
                                        "/04/30",
                                        sep = ""))
  ) %>%
  filter(start_day > period_start_day,
         start_day < period_end_day) %>%
  left_join(election_results %>%
              select(-party, -bloc) %>%
              rename(election_result = percentage),
              by = c("candidate" = "candidate",
                     "election_year" = "year")) %>%
  filter(!is.na(election_result)) %>%
  group_by(poll_id) %>%
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
  #"
  #' subset
  ungroup() %>%
  group_by(poll_id) %>%
  mutate(poll_id = cur_group_id()) %>%
  ungroup() %>%
  select(poll_id, survey_id, pollName, candidate, date, election_id,
         y, period_start_day, period_end_day, bloc)
################################################################################
df <- df_past
df_cleaned <- lapply(1:max(df$election_id), function(ii){
  tmp <- df %>%
    filter(election_id == ii) %>%
    mutate(bloc_id = as.integer(factor(bloc, levels = bloc_vector))) %>%
    mutate(time_id = as.integer(difftime(date, period_start_day,
                                         units = "days")))
  candidate_vector <- tmp %>%
    distinct(bloc_id, candidate) %>%
    arrange(bloc_id) %>%
    pull(candidate)
  if (!("_Abstention" %in% candidate_vector)){
    candidate_vector <- c("_Abstention", candidate_vector)
  }
  pollster_vector <- tmp %>%
    distinct(bloc_id, pollName) %>%
    arrange(bloc_id) %>%
    pull(pollName)

  tmp <- tmp %>%
    mutate(candidate_id = match(candidate, candidate_vector)) %>%
    mutate(pollster_id = match(pollName, pollster_vector))

  df_t_unit <- tmp %>%
    distinct(time_id) %>%
    add_row(time_id = tmp %>%
              head(1) %>%
              mutate(time_id =
                       as.integer(difftime(period_end_day,
                                           period_start_day,
                                           units = "days"))) %>%
              pull(time_id)) %>%
    arrange(time_id) %>%
    mutate(time_unit_id = 1:n(),
           time_skip = time_id - lag(time_id))

  tmp <- tmp %>%
    left_join(df_t_unit) %>%
    select(candidate, candidate_id, bloc_id, time_unit_id, poll_id, y,
           election_id, survey_id, pollster_id, time_id, pollName) %>%
    rename(question_id = poll_id)

  return(list(
    polls = tmp,
    df_t_unit = df_t_unit,
    candidate_vector = candidate_vector
  ))
})
write_rds(df_cleaned,
          file = "dta/polls_dta/election_season_model/poll_input.rds")
################################################################################