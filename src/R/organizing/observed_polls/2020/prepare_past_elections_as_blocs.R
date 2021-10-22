################################################################################
## Title: Prepare past elections as blocs
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
df_elections <- read_csv("dta/polls_dta/election_results_clean.csv")
df_bloc_id <- read_csv("dta/polls_dta/candidate_bloc_cross_walk.csv")
################################################################################
## Mangle
df_election <- df_elections %>%
  #' Merge
  select(-bloc, -party) %>%
  left_join(df_bloc_id %>%
              distinct(bloc, party,
                       long_name, election_year),
            by = c("candidate" = "long_name", "year" = "election_year")) %>%
  distinct(candidate, year, .keep_all = TRUE) %>%
  filter(!is.na(bloc), year >= 1995) %>%
  #' summarize
  group_by(year, bloc) %>%
  summarize(percentage = sum(percentage)) %>%
  ungroup() %>%
  add_row(year = 1995, bloc = "Centre", percentage = 0) %>%
  #' bloc_id
  mutate(bloc_id = as.integer(factor(bloc, levels = bloc_vector)),
         election_id = as.integer(factor(year,
                                         levels = c(1995, seq(2002, 2017, 5))))) %>%
  #' arrange
  arrange(election_id, bloc_id) %>%
  #' subset
  select(election_id, bloc_id, percentage)
#' write
write_csv(df_election,
          file = "dta/polls_dta/model_input/election_results_bloc_clean.csv")
################################################################################