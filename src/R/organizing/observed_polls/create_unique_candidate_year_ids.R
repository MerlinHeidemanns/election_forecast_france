###############################################################################
## Title: Create unique candidate_year_ids for past data
###############################################################################
## Empty environment
rm(list = ls())
###############################################################################
## libraries
library(tidyverse)
###############################################################################
## Load data
df_2002 <- read_csv(file = "dta/polls_dta/2002_polls_clean.csv")
df_2007 <- read_csv(file = "dta/polls_dta/2007_polls_clean.csv")
df_2012 <- read_csv(file = "dta/polls_dta/2012_polls_clean.csv")
df_2017 <- read_csv(file = "dta/polls_dta/2017_polls_clean.csv")
df_2022 <- read_csv("dta/polls_dta/polls_2020/polls_position_polls_2022_raw.csv")
bloc_association <- read_csv("dta/polls_dta/party_association.csv")
###############################################################################
## Load data
bloc_vector <- c("Abstention",
                 "Gauche radicale et extrême gauche",
                 "Gauche",
                 "Centre",
                 "Droite",
                 "Droite radicale et extreme droite")
###############################################################################
## Clean bloc_association
## Remove -
df_bloc_id <- bloc_association %>%
  mutate(candidate = str_replace_all(candidate, "\\-", " "),
         candidate_long_id = NA) %>%
  distinct(candidate, party, bloc, year, long_name, candidate_long_id)
###############################################################################
#' Combine all names plus year
candidates_blocs <- bind_rows(
  df_2002 %>%
    distinct(candidate, long_name, election_year),
  df_2007 %>%
    distinct(candidate, long_name, election_year),
  df_2012 %>%
    distinct(candidate, long_name, election_year),
  df_2017 %>%
    distinct(candidate, long_name, election_year),
  df_2022 %>%
    distinct(candidates) %>%
    rename(long_name = candidates) %>%
    mutate(election_year = 2022)
) %>%
#' Add blocs
  full_join(df_bloc_id %>%
             select(bloc,
                    party,
                    candidate, year),
           by = c("candidate" = "candidate",
                  "election_year" = "year")) %>%
  mutate(bloc = ifelse(candidate == "_Abstention", "Abstention", bloc)) %>%
#' Clean names
  mutate(long_name = ifelse(candidate == "Hollande", "Francois Hollande", long_name),
         long_name = gsub("-", " ", long_name)) %>%
  bind_rows(df_bloc_id %>%
              select(bloc,
                     party,
                     candidate, year,
                     long_name) %>%
              rename(election_year = year)) %>%
  filter(!is.na(bloc), bloc != "Autre")

#' Add abstention
for (j in c(1995, seq(2002, 2022, 5))){
  candidates_blocs <- candidates_blocs %>%
    add_row(candidate = "Abstention", long_name = "_Abstention",
            bloc = "Abstention", election_year = j)
}
#' Remove duplicates
candidates_blocs <- candidates_blocs %>%
  distinct()
#' Save
write_csv(candidates_blocs, "dta/polls_dta/candidate_bloc_cross_walk.csv")
###############################################################################