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
main_candidates_list <- c("Macron", "Le Pen", "Pecresse",
                     "Hidalgo", "Jadot", "Zemmour", "Melenchon")
################################################################################
## Import data
df_2017 <- read_csv("dta/polls_dta/2017_polls_clean.csv")
df_2012 <- read_csv("dta/polls_dta/2012_polls_clean.csv")
df_2007 <- read_csv("dta/polls_dta/2007_polls_clean.csv")
df_2002 <- read_csv("dta/polls_dta/2002_polls_clean.csv")
df_past <- bind_rows(df_2017,
                      df_2012,
                      df_2007,
                      df_2002)
candidates_blocs <- read_csv("dta/france_classification/candidate_bloc_cross_walk.csv")
df_current <-
  read_csv("dta/polls_dta/election_season_model/poll_input_current.csv")
election_years <- df_past %>% pull(election_year) %>% unique() %>% sort()
election_years <- c(election_years, max(election_years) + 5)
election_results <- read_csv("dta/polls_dta/election_results_clean.csv")
election_dates <- read_csv("dta/polls_dta/election_dates.csv")
################################################################################
## Prepare current polls
df_current <- df_current %>%
  rename(pollName = institute,
         start_day = date_debut,
         end_day = date_fin,
         sampleSize = interroges,
         poll_id = question_id,
         abstention = nspp,
         candidate = candidates) %>%
  mutate(percentage = percentages/100,
         election_year = 2022,
         poll_id = paste(2022, start_day, pollName))

df_current <- df_current %>%
  bind_rows(
    df_current %>%
      select(poll_id, abstention) %>%
      mutate(candidate = "_Abstention",
             percentage = abstention/100,
             election_year = 2022) %>%
      select(-abstention)
  ) %>%
  group_by(poll_id) %>%
  mutate(sampleSize = max(sampleSize, na.rm = TRUE),
          percentage = percentage/sum(percentage),
         y = as.integer(percentage * sampleSize))


## Prepare past polls
df_past <- df_past %>%
  bind_rows(df_current) %>%
  arrange(-election_year) %>%
  #' subset on time frames
  left_join(election_dates,
            by = c("election_year" = "year")) %>%
  mutate(period_start_day = date_first_round - (7 * 18),
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
  filter(n() > 3) %>%
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
  mutate(party = ifelse(bloc == "Abstention", "Abstention", party),
         bloc = ifelse(grepl("Taubira", candidate), "Gauche", bloc)) %>%
  group_by(election_year, party, poll_id) %>%
  mutate(z1 = ifelse(!is.na(election_result_percentage) | election_year == 2022, 1, 0)) %>%
  group_by(election_year, bloc) %>%
  mutate(main_candidate =
           ifelse(election_result_percentage == max(election_result_percentage, na.rm = TRUE) |
                  (candidate %in% main_candidates_list & election_year == 2022),1,0),
         main_candidate = ifelse(is.na(main_candidate), 0, main_candidate)) %>%
  mutate(bloc = ifelse(!main_candidate, "Autre", bloc)) %>%
  ungroup()

df_le_pen_zemmour <- df_past %>%
  filter(candidate %in% c("Le Pen", "Zemmour")) %>%
  group_by(poll_id) %>%
  arrange(poll_id) %>%
  filter(n() == 2) %>%
  group_by(poll_id, election_year, pollName, election_id, period_start_day, period_end_day, candidate, start_day) %>%
  summarize(y = sum(y)) %>%
  group_by(poll_id) %>%
  mutate(#poll_id = cur_group_id(),
    t = ceiling((as.integer(difftime(start_day, period_start_day) + 1)/7))) %>%
  ungroup()

df_past <- df_past %>%
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
write_csv(df_le_pen_zemmour,
          file = "dta/polls_dta/election_season_model/poll_input_le_pen_zemmour.csv")
################################################################################