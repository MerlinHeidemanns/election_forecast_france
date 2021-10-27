################################################################################
## Title: Prepare current polls
################################################################################
## Empty environment
rm(list = ls())
################################################################################
## Libraries
library(tidyverse)
################################################################################
## Load data
df <- read_csv("dta/polls_dta/polls_2020/polls_position_polls_2022_raw.csv") %>%
  filter(intentions_exprimees != -1)
df_election_dates <- read_csv("dta/polls_dta/election_dates.csv")
candidates_blocs <- read_csv("dta/polls_dta/candidate_bloc_cross_walk.csv")
################################################################################
## Vectors
#' Blocs
bloc_vector <- c("Abstention",
                 "Gauche radicale et extreme gauche",
                 "Gauche",
                 "Centre",
                 "Droite",
                 "Droite radicale et extreme droite")
#' Pollsters
institute_vector <- df %>%
  pull(institute) %>%
  unique() %>%
  sort()
write_rds(institute_vector, "dta/polls_dta/model_input/instute_vector_current.csv")
#' Candidates
candidate_vector <- df %>%
  add_row(candidates = "_Abstention") %>%
  left_join(candidates_blocs %>%
              select(long_name, bloc),
              by = c("candidates" = "long_name")) %>%
  distinct(candidates, bloc) %>%
  mutate(bloc = factor(bloc, levels = bloc_vector)) %>%
  arrange(bloc)
candidate_blocs <- candidate_vector %>% pull(bloc) %>% as.integer()
candidate_vector <- candidate_vector %>% pull(candidates)
write_rds(candidate_vector, "dta/polls_dta/model_input/candidate_vector_current.rds")
write_rds(candidate_blocs, "dta/polls_dta/model_input/candidate_bloc_vector_current.rds")
#' Time points
dates <- df %>%
  distinct(date_fin) %>%
  pull(date_fin)
election_dates <- df_election_dates %>%
  filter(year %in% c(2017, 2022)) %>%
  pull(date_first_round)
df_t_unit <- data.frame(
  dates = c(
    dates,
    election_dates
  )
) %>%
  arrange(dates) %>%
  mutate(t = as.integer(difftime(dates, min(dates), units = "days")),
         t_unit = 1:n())
write_csv(df_t_unit, "dta/polls_dta/model_input/df_t_unit_current.csv")
t_unit <- df_t_unit %>%
  mutate(skip = t - lag(t)) %>%
  filter(!is.na(skip)) %>%
  pull(skip)
write_rds(t_unit, "dta/polls_dta/model_input/t_unit_current.rds")
################################################################################
## Mangle
df <- df %>%
  group_by(survey_id, question_id) %>%
  mutate(question_id = cur_group_id()) %>%
  ungroup() %>%
  mutate(percentages = percentages / 100)
#' Add abstention as candidate
df <- lapply(1:max(df$question_id), function(j){
  poll <- df %>%
    filter(question_id == j) %>%
    mutate(percentages = percentages * (1 - intentions_exprimees/interroges))
  abstention_share <- poll %>%
    mutate(abstention = 1 - intentions_exprimees/interroges) %>%
    distinct(abstention) %>%
    pull(abstention)
  poll <- poll %>%
    add_row(
      institute = poll$institute[1],
      date_debut = poll$date_debut[1],
      date_fin = poll$date_fin[1],
      methode = poll$methode[1],
      interroges = poll$interroges[1],
      question_id = poll$question_id[1],
      nspp = poll$nspp[1],
      base = poll$base[1],
      intentions_exprimees = poll$intentions_exprimees[1],
      candidates = "_Abstention",
      percentages = abstention_share,
      survey_id = poll$survey_id[1]
    )
  return(poll)
}) %>%
  do.call("bind_rows",.)
#' Add ids
df_clean <- df %>%
  #' Pollster_id
  mutate(
    pollster_id = as.integer(factor(institute, institute_vector)),
  #' Candidate_id
    candidate_id = as.integer(factor(candidates, candidate_vector)),
  #' y
    y = ceiling(interroges * percentages)
  ) %>%
  #' t
  left_join(
    df_t_unit %>%
      select(dates, t_unit),
    by = c("date_fin" = "dates")
  ) %>%
  select(question_id, survey_id, pollster_id, candidate_id, y, t_unit)
#' Arrange
df_clean <- df_clean %>%
  arrange(pollster_id, survey_id, question_id, candidate_id, t_unit) %>%
  group_by(pollster_id, survey_id) %>%
  mutate(survey_id = cur_group_id())
#' Save
write_csv(df_clean, "dta/polls_dta/model_input/polls_current_clean.csv")
################################################################################