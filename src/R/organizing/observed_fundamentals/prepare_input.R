###############################################################################
## Title: Prepare input to fundamentals model
###############################################################################
## Clean environment
rm(list = ls())
###############################################################################
## Load libraries
library(tidyverse)
###############################################################################
## Load data
election_df <- read_csv("dta/election_results/department_total_votes.csv")
unemployment_df <-
  read_csv("dta/fundamentals_dta/cleaned_input/departmental_unemployment.csv")
df_approval <- read_csv("dta/fundamentals_dta/approval.csv")
###############################################################################
## Input
#' Unemployment
election_years <- election_df %>%
  filter(year > 1981) %>%
  distinct(year) %>%
  pull(year)
df_input_departement <- unemployment_df %>%
  mutate(date =
           as.Date(paste(Year, "-", c(1,4,7,10)[Quarter], "-01", sep = "")),
         election = ifelse(Year %in% election_years & Quarter == 2, 1, 0),
          election_date = as.Date(ifelse(election, as.character(date), NA)),
          election_year = ifelse(election, Year, NA)) %>%
  group_by(departement) %>%
  fill(election_date, election_year, .direction = 'up') %>%
  mutate(diff_election_date = ceiling(as.integer(difftime(election_date,
                                       date,
                                       units = "days"))/89) - 1,
         diff_election_date = ifelse(election, 0, diff_election_date)) %>%
  filter(diff_election_date < 10) %>%
  select(rate, departement, election_year, diff_election_date) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(departement, election_year),
    names_from = diff_election_date,
    names_prefix = "q",
    values_from = rate
  )
## Ids
election_year_vector <- df_input_departement %>%
  distinct(election_year) %>%
  arrange() %>%
  pull()
department_vector <- df_input_departement %>%
  distinct(departement) %>%
  arrange() %>%
  pull()
## unemployment ids
df_input_departement <- df_input_departement %>%
  mutate(
    election_id = match(election_year, election_year_vector),
    department_id = match(departement, department_vector)
  ) %>%
  arrange(election_id, department_id)
## Write
write_csv(df_input_departement,
          file = "dta/fundamentals_dta/cleaned_input/input_data.csv")


###############################################################################
## Data into correct format
#df_approval <-
df_approval <- df_approval %>%
  mutate(election_year = lubridate::year(date_next_first_round)) %>%
  filter(election_year %in% election_year_vector) %>%
  mutate(start_date = as.Date(paste(
    lubridate::year(date_next_first_round) - 1,
    "-08-01", sep = ""))) %>%
  filter(date >= (date_next_first_round - 360)) %>%
  mutate(time_id = 1 + floor((1 + as.integer(difftime(date, date_next_first_round - 360, units = "days")))/7)) %>%
  group_by(pollster_id, president_id) %>%
  mutate(pollster_president_id = cur_group_id()) %>%
  ungroup() %>%
  mutate(president_id = president_id - min(president_id) + 1,
         pollster_id = as.integer(factor(pollster))) %>%
  select(election_year, p_approve, N, pollster_president_id, president,
         pollster, pollster_id, president_id, method_id, method, time_id) %>%
  arrange(president_id) %>%
  mutate(pollster_president_id = as.integer(factor(paste(president_id, pollster))))
## Write
write_csv(df_approval,
          file = "dta/fundamentals_dta/cleaned_input/input_national.csv")
###############################################################################
## Election results
df_results_department <- election_df %>%
  filter(categorie %in% c("Inscrits", "Abstentions")) %>%
  mutate(
    departement = str_replace_all(departement, "\\-", " "),
    election_id = match(year, election_year_vector),
    department_id = match(departement, department_vector)
  ) %>%
  filter(departement != "Mayotte") %>%
  select(categorie, votes, departement, year, election_id, department_id) %>%
  pivot_wider(
    id_cols = c(departement, year, election_id, department_id),
    names_from = categorie,
    values_from = votes
  ) %>%
  rename(
    y = Abstentions,
    n = Inscrits
  ) %>%
  arrange(year, department_id) %>%
  mutate(p_abstentions = y/n) %>%
  group_by(department_id) %>%
  mutate(lag_p_abstentions = lag(p_abstentions)) %>%
  ungroup() %>%
  filter(year %in% election_years)
## Write
write_csv(df_results_department,
          file = "dta/fundamentals_dta/cleaned_input/input_results.csv")
###############################################################################