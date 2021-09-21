## Model on 2022 polls
# -- Clean
rm(list = ls())
# -- Libraries
library(tidyverse)
library(cmdstanr)
# -- Run cleaning file
source("src/R/organizing/observed_polls/poll_cleaning.R")
# -- Source functions
source("src/R/functions/obs_create_variable_inclusion_input.R")
# -- Load
## 1. file with polls
df <- read_csv("dta/polls_dta/2020_polls_clean.csv")
## 2. Skip vector
t_diff <- read_rds("dta/polls_dta/t_diff.Rds")
t_diff <- t_diff
## 3. Time identifiers
df_time <- read_csv("dta/polls_dta/time_identifiers.csv")
# -- Prepare data list
NSurveys <- df %>%
  distinct(survey_id) %>%
  nrow()
NPolls <- df %>%
  distinct(question_id) %>%
  nrow()
NCandidates <- df %>%
  distinct(candidate_id) %>%
  nrow()
NPollsters <- df %>%
  distinct(pollster_id) %>%
  nrow()
NTime <- df %>%
  distinct(t_unit) %>%
  nrow()
t_unit <- t_diff

id_P_survey <- df %>%
  distinct(survey_id, question_id) %>%
  pull(survey_id)
id_S_pollster <- df %>%
  distinct(survey_id, pollster_id) %>%
  pull(pollster_id)
id_S_time <- df %>%
  distinct(survey_id, t_unit) %>%
  pull(t_unit)

inclusion_input <- obs_create_variable_inclusion_input(df)

NCandidates_Poll <- inclusion_input$P_first_round
NCombinations <- inclusion_input$N_combinations
NCandidate_Combinations <- inclusion_input$P_N_combinations
candidates_included <- inclusion_input$p_first_round_included
candidates_excluded <- inclusion_input$p_first_round_excluded
id_P_combinations <- inclusion_input$p_id
y <- inclusion_input$y_first_round

## Load past data
df_past <- read.csv(file = "dta/polls_dta/2017_polls_clean.csv")
election_date <- read_csv("dta/polls_dta/election_dates.csv") %>%
  filter(year == 2017) %>%
  pull(date_first_round)
df_past <- df_past %>%
  filter(difftime(election_date, end_day) < 21,
         difftime(election_date, end_day) > 0) %>%
  group_by(poll_id) %>%
  filter(n() > 2)
df_past <- df_past %>%
  arrange(poll_id, candidate_long_id)

NElections_past <- 1
NPolls_past <- df_past %>%
  distinct(poll_id) %>%
  nrow()
NCandidates_past <- df_past %>%
  distinct(candidate_long_id) %>%
  nrow()
id_r_past <- df_past %>%
  distinct(poll_id, pollster_id) %>%
  group_by(pollster_id) %>%
  mutate(pollster_id_model = cur_group_id()) %>%
  pull(pollster_id_model)

id_t_past <- df_past %>%
  distinct(poll_id) %>%
  mutate(i = 1) %>%
  pull(i)

y_past <-
df_past %>%
  arrange(poll_id, candidate_long_id) %>%
  dplyr::select(poll_id, candidate_long_id, y) %>%
  pivot_wider(id_cols = poll_id,
              names_from = candidate_long_id,
              names_prefix = "c",
              values_from = y) %>%
  View()

## data list
data_list <- list(
  NSurveys = NSurveys,
  NPolls = NPolls,
  NCandidates = NCandidates,
  NPollsters = NPollsters,
  NTime = NTime,
  t_unit = t_unit,
  id_P_survey = id_P_survey,
  id_S_pollster = id_S_pollster,
  id_S_time = id_S_time,
  NCandidates_Poll = NCandidates_Poll,
  NCombinations = NCombinations,
  NCandidate_Combinations = NCandidate_Combinations,
  candidates_included = candidates_included,
  candidates_excluded = candidates_excluded,
  id_P_combinations = id_P_combinations,
  y = y %>% t(),

  NElections_past = 1,
  NPolls_past = ,
  NCandidates_past,
  NPollsters_past,
  id_r_past,
  id_rt_past,
  id_t_past,
  results,
  y_past
  int<lower = 1, upper = NPollsters_past> id_r_past[NPolls_past]; // which pollster
  int<lower = 1, upper = NElections_past> id_rt_past[NPollsters_past]; // which election the pollster belongs to
  int<lower = 1, upper = NElections_past> id_t_past[NPolls_past];
  matrix[max(NCandidates_past), NElections_past] results;
  int<lower = 0> y_past[max(NCandidates_past), NPolls_past];
)
for (j in 1:length(data_list)){
  if (data_list[[j]] %>% is.na() %>% any()){
    print(names(data_list)[j])
  }
}

## Model
# -- Load
mod <- cmdstan_model("src/stan/v1_with_past.stan")
# -- Fit
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 250,
  iter_warmup = 750,
  parallel_chains = 6,
  refresh = 50,
  init = 0.2
)
fit$save_object(file = "dta/fits/2021_09_16.Rds")
# -- Posterior Predictive Checks
fit <- read_rds("dta/fits/2021_09_16.Rds")

source("src/R/functions/ppc_obs_alpha.R")
source("src/R/functions/ppc_obs_xi.R")
source("src/R/functions/ppc_obs_theta_mway_election_day.R")
source("src/R/functions/ppc_obs_theta_plt_hist.R")
fit$summary("sigma_alpha")
supvec_names <- read.csv("dta/polls_dta/candidate_identifiers.csv") %>%
  pull(candidate)
supvec_time <- read_csv("dta/polls_dta/time_identifiers.csv") %>%
  pull(end_date)
supvec_bloc <- read.csv("dta/polls_dta/candidate_party_identifiers.csv")
## Obs theta by bloc
ppc_obs_theta_bloc_politiques(fit)
## Obs three-way Bertrand, Macron, Le Pen
df_out <- ppc_obs_theta_mway_election_day(fit, c("Xavier Bertrand", "Emmanuel Macron"), 500)
ppc_obs_theta_plt_hist(df_out)
## Polling error
ppc_obs_xi(fit, supvec_names = supvec_names)
## Polling house deviation
supvec_pollster <- read_csv("dta/polls_dta/pollster_identifiers.csv") %>%
  pull(pollName)
ppc_obs_alpha(fit, supvec_names = supvec_names, supvec_pollster)



























