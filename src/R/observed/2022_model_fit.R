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
  filter(n() > 2) %>%
  ungroup()
df_past <- df_past %>%
  arrange(poll_id, candidate_long_id)


NElections_past <- 1
NPolls_past <- df_past %>%
  distinct(poll_id) %>%
  nrow() %>%
  array()
NPollsters_past <- df_past %>%
  distinct(pollster_id) %>%
  nrow() %>%
  array()
NCandidates_past <- df_past %>%
  distinct(candidate_long_id) %>%
  nrow() %>%
  array()
id_r_past <- df_past %>%
  distinct(poll_id, pollster_id) %>%
  group_by(pollster_id) %>%
  mutate(pollster_id_model = cur_group_id()) %>%
  pull(pollster_id_model)

id_rt_past <- df_past %>%
  distinct(pollster_id) %>%
  mutate(i = 1) %>%
  pull(i)

id_t_past <- df_past %>%
  distinct(poll_id) %>%
  mutate(i = 1) %>%
  pull(i)

results <- read_csv("dta/polls_dta/election_results_2017_clean.csv") %>%
  left_join(read_csv("dta/polls_dta/candidate_identifiers_2017_long.csv"),
            by = c("candidate" = "long_name")) %>%
  arrange(candidate_long_id) %>%
  pull(percentage) %>%
  matrix()


y_past <- df_past %>%
  arrange(poll_id, candidate_long_id) %>%
  dplyr::select(poll_id, candidate_long_id, y) %>%
  pivot_wider(id_cols = poll_id,
              names_from = candidate_long_id,
              names_prefix = "c",
              values_from = y,
              values_fill = -999) %>%
  dplyr::select(-poll_id)

abstention_omitted <- inclusion_input$abstention_omitted
abstention_omitted_past <- as.integer(-999 == y_past[,1])

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
  NPolls_past = NPolls_past,
  NCandidates_past = NCandidates_past,
  NPollsters_past = NPollsters_past,
  id_r_past = id_r_past,
  id_rt_past  = id_rt_past,
  id_t_past = id_t_past,
  results = results,
  y_past = y_past %>% t(),

  abstention_omitted = abstention_omitted,
  abstention_omitted_past = abstention_omitted_past

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
  iter_sampling = 400,
  iter_warmup = 300,
  parallel_chains = 6,
  refresh = 25,
  init = 0.2
)
fit$save_object(file = "dta/fits/2021_09_22.Rds")
# -- Posterior Predictive Checks
fit <- read_rds("dta/fits/2021_09_22.Rds")

source("src/R/functions/ppc_obs_alpha.R")
source("src/R/functions/ppc_obs_xi.R")
source("src/R/functions/ppc_obs_theta_mway_election_day.R")
source("src/R/functions/ppc_obs_theta_plt_hist.R")
supvec_names <- read.csv("dta/polls_dta/candidate_identifiers.csv") %>%
  pull(candidate)
supvec_time <- read_csv("dta/polls_dta/time_identifiers.csv") %>%
  pull(end_date)
supvec_bloc <- read.csv("dta/polls_dta/candidate_party_identifiers.csv")
## Obs theta by bloc
ppc_obs_theta_bloc_politiques(fit)
## Obs three-way Bertrand, Macron, Le Pen
df_out <- ppc_obs_theta_mway_election_day(fit, c("_Abstention", "Xavier Bertrand", "Emmanuel Macron"), 500)
ppc_obs_theta_plt_hist(df_out)
## Polling error
ppc_obs_xi(fit, supvec_names = supvec_names)
## Polling house deviation
supvec_pollster <- read_csv("dta/polls_dta/pollster_identifiers.csv") %>%
  pull(pollName)
ppc_obs_alpha(fit, supvec_names = supvec_names, supvec_pollster)
<<<<<<< HEAD




prob_theta <- fit$draws('prob_theta') %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  filter(iter < 100) %>%
  pivot_longer(c(-iter),
               names_to = "variable",
               values_to = "draws") %>%
  mutate(
    time = as.integer(str_match(variable, ",([\\d+]+)")[,2]),
    candidate_id = as.integer(str_match(variable, "([\\d+]+),")[,2])
  ) %>%
  filter(!is.na(time)) %>%
  filter(time == max(time)) %>%
  dplyr::select(-variable, -time)

prob_theta <- prob_theta %>%
  rename(draws_a = draws) %>%
  full_join(prob_theta %>%
              rename(draws_b = draws),
            by = "iter")

prob_theta %>%
  filter((candidate_id.x < 10) | (candidate_id.x == max(candidate_id.x)),
         (candidate_id.y < 10) | (candidate_id.y == max(candidate_id.y)),
         candidate_id.y != candidate_id.x) %>%
  ggplot(aes(x = draws_a, y = draws_b)) +
    geom_point(alpha = 0.5) +
    facet_grid(candidate_id.y ~ candidate_id.x)
=======
## Prob sigma cov
ppc_obs_prob_sigma_cov(fit, supvec_names)
## Transition matrix
ppc_obs_transition_matrix(fit, supvec_names)
## two-way win probability second round
ppc_obs_theta_twoway_plt(fit, NIter = 300)
## Sigma
ppc_obs_sigma(fit)



>>>>>>> efe07e49776d6659be19d2ff400b156246386fee


















