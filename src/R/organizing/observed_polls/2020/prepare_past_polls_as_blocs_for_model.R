################################################################################
## Title: Prepare past polls as blocs
################################################################################
## Empty environment
rm(list = ls())
################################################################################
## Libraries
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
## Load data
df <- read_csv("dta/polls_dta/polls_02_17/polls_02_17_summarized_blocs.csv")
df_elections <- read_csv("dta/polls_dta/model_input/election_results_bloc_clean.csv")
df_elections_dates <- read_csv("dta/polls_dta/election_dates.csv")
################################################################################
## Mangle
#' Pollster vector
pollsters <- df %>%
  distinct(election_year, pollName) %>%
  arrange(election_year, pollName) %>%
  mutate(pollster_election = paste(pollName, election_year, sep = "_")) %>%
  pull(pollster_election)
#' Time points
df_t_unit <- df %>%
  rename(date = start_day) %>%
  distinct(date) %>%
  bind_rows(
    df_elections_dates %>%
      rename(date = date_first_round) %>%
      filter(date != max(date)) %>%
      select(date)
  ) %>%
  mutate(t = as.integer(difftime(date, min(date), units = "days"))) %>%
  arrange(t) %>%
  mutate(t_unit = 1:n())
write_csv(df_t_unit, "dta/polls_dta/model_input/df_t_unit_past.csv")
#' Skip vector
t_unit <- df_t_unit %>%
  mutate(t_skip = t - lag(t)) %>%
  filter(!is.na(t_skip)) %>%
  pull(t_skip)
#' Save
write_rds(t_unit, "dta/polls_dta/model_input/t_unit.rds")
################################################################################
## Elections
df_elections_dates <- df_elections_dates %>%
#' Remove 2022
  filter(date_first_round != max(date_first_round)) %>%
#' Create election id
  mutate(election_id = as.integer(factor(year, levels = sort(year))))
df_elections <- df_elections %>%
#' Merge in dates
  left_join(df_elections_dates) %>%
#' Merge in unit time points
  left_join(df_t_unit %>%
              select(date, t_unit),
            by = c("date_first_round" = "date")) %>%
#' Subset
  select(election_id, bloc_id, percentage, t_unit)
#' Write
write_csv(df_elections,
          file = "dta/polls_dta/model_input/election_results_blocs_cleaned.csv")
################################################################################
#' Past polls
df_polls_w_ids <- df %>%
  #' Time points
  left_join(df_t_unit %>%
              select(date, t_unit),
            by = c("start_day" = "date")) %>%
  #' Turn blocs into integers
  mutate(
    bloc = factor(bloc, levels = bloc_vector),
    bloc_id = as.integer(bloc),
  #' Election id
    election_id = as.integer(factor(election_year, levels = c(1995, seq(2002, 2017, 5)))),
  #' Pollster id
    pollster_id = as.integer(factor(paste(pollName, election_year, sep = "_"),
                                    levels = pollsters))
  ) %>%
  #' arrange
  arrange(election_id, pollster_id, poll_id, bloc_id) %>%
  #' subset
  select(election_id, pollster_id, poll_id, bloc_id, y, t_unit)
#' Save
write_csv(df_polls_w_ids, "dta/polls_dta/model_input/polls_blosc_w_ids.csv")
################################################################################