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
  y = y %>% t()
)
for (j in 1:length(data_list)){
  if (data_list[[j]] %>% is.na() %>% any()){
    print(names(data_list)[j])
  }
}

## Model
# -- Load
mod <- cmdstan_model("src/stan/v1_current_election_transition_matrix_no_round_no_past.stan")
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


source("src/R/functions/ppc_obs_alpha.R")
source("src/R/functions/ppc_obs_xi.R")
fit$summary("sigma_alpha")
supvec_names <- read.csv("dta/polls_dta/candidate_identifiers.csv") %>%
  pull(candidate)
supvec_time <- read_csv("dta/polls_dta/time_identifiers.csv") %>%
  pull(end_date)
supvec_bloc <- read.csv("dta/polls_dta/candidate_party_identifiers.csv")
## Obs theta by bloc
ppc_obs_theta_bloc_politiques(fit)
## Obs three-way Bertrand, Macron, Le Pen
ppc_obs_mway_election_day(fit, c("Xavier Bertrand", "Emmanuel Macron", "Marine Le Pen"), 500)
## Polling error
ppc_obs_xi(fit, supvec_names = supvec_names)
## Polling house deviation
supvec_pollster <- read_csv("dta/polls_dta/pollster_identifiers.csv") %>%
  pull(pollName)
ppc_obs_alpha(fit, supvec_names = supvec_names, supvec_pollster)






