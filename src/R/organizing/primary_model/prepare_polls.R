################################################################################
## Title: Prepare polls for primary model
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
################################################################################
## Prepare past polls
df_past <- df_past %>%
  #' subset on time frames
  mutate(period_start_day = as.Date(paste(as.character(election_year - 1),
                                  "/06/01",
                                  sep = "")),
         period_end_day = as.Date(paste(as.character(election_year - 1),
                                          "/12/31",
                                          sep = ""))
         ) %>%
  filter(start_day > period_start_day,
         start_day < period_end_day) %>%
  group_by(poll_id) %>%
  mutate(remove = max(candidate == "X nominee.")) %>%
  filter(remove != 1) %>%
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
## Prepare current polls
df_current <- df_current %>%
  #' date
  mutate(date = as.Date(ifelse(is.na(date_fin),
                             as.character(date_debut),
                             as.character(date_fin)))) %>%
  #' election_id
  mutate(election_year = 2022,
         election_id = match(election_year, election_years))
################################################################################
## Prepare current polls
df_current <- df_current %>%
  group_by(survey_id, question_id) %>%
  mutate(question_id = cur_group_id()) %>%
  ungroup() %>%
  mutate(percentages = percentages / 100)
#' Add abstention as candidate
df_current <- lapply(1:max(df_current$question_id), function(j){
  poll <- df_current %>%
    filter(question_id == j) %>%
    mutate(percentages = percentages * (1 - nspp/100),
           sample_size = ifelse(base == "I", interroges, intentions_exprimees))
  abstention_share <- poll %>%
    mutate(abstention = nspp/100) %>%
    distinct(abstention) %>%
    pull(abstention)
  poll <- poll %>%
    add_row(
      institute = poll$institute[1],
      date_debut = poll$date_debut[1],
      date_fin = poll$date_fin[1],
      methode = poll$methode[1],
      sample_size = poll$sample_size[1],
      question_id = poll$question_id[1],
      nspp = poll$nspp[1],
      base = poll$base[1],
      intentions_exprimees = poll$intentions_exprimees[1],
      candidates = "_Abstention",
      percentages = abstention_share,
      survey_id = poll$survey_id[1],
      election_year = poll$election_year[1],
      election_id = poll$election_id[1],
      date = poll$date[1]
    )
  return(poll)
}) %>%
  do.call("bind_rows",.)
#' Subset
df_current <- df_current %>%
  rename(pollName = institute,
         poll_id = question_id,
         candidate = candidates) %>%
  left_join(candidates_blocs %>%
              select(long_name, bloc) %>%
              distinct(),
            by = c("candidate" = "long_name")) %>%
  #' y
  mutate(y = ceiling(sample_size * percentages)) %>%
  #' period_start_day period_end_day
  mutate(period_start_day = as.Date(paste(as.character(election_year - 1),
                                          "/06/01",
                                          sep = "")),
         period_end_day = as.Date(paste(as.character(election_year - 1),
                                        "/12/31",
                                        sep = ""))
  ) %>%
  group_by(poll_id) %>%
  mutate(poll_id = cur_group_id()) %>%
  ungroup() %>%
  select(poll_id, pollName, survey_id, candidate, date, election_id,
    y, period_start_day, period_end_day, bloc)
################################################################################
## Join polls and clean
df <- bind_rows(
  df_current,
  df_past
)
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
           election_id, survey_id, pollster_id, time_id) %>%
    rename(question_id = poll_id)

  return(list(
    polls = tmp,
    df_t_unit = df_t_unit,
    candidate_vector = candidate_vector
  ))
})
write_rds(df_cleaned,
          file = "dta/polls_dta/primary_model_input/poll_input.rds")
################################################################################