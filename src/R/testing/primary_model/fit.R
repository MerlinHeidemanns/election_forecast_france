################################################################################
## Title: Test primary model
################################################################################
## Libraries
library(tidyverse)
library(cmdstanr)
################################################################################
## Functions
source("src/R/functions/sim_random_walk_bloc.R")
source("src/R/functions/sim_polling_data_blocs.R")
source("src/R/functions/exp_softmax.R")
source("src/R/functions/create_variable_inclusion_input.R")
source("src/R/functions/create_y_first_round.R")
################################################################################
## Fake data
data_true_list <- list()
data_polls_list <- list()
for (jj in 1:2){
  NElections_past <- 1
  NCandidates <- 10
  NTime <- 100
  rho <- 0.1
  sigma <- 0.07
  data_true <- sim_random_walk_blocs(NElections_past, NCandidates, NTime, rho, sigma)

  ## Simulate polls
  NPolls <- 20
  NPolls_past <- 400
  NPollsters <- 5
  NCombinations <- 7
  sigma_alpha <- 0.08
  sigma_tau <- 0.03
  sigma_xi <- 0.1
  theta_matrix_blocs <- data_true$theta_matrix_blocs
  prob_theta_matrix_blocs <- data_true$prob_theta_matrix_blocs
  theta_matrix_candidates <- data_true$theta_matrix_candidates
  transition_matrix <- data_true$transition_matrix_preferences
  id_C_blocs <- data_true$id_C_blocs
  data_polls <- sim_polling_data_blocs(NPolls, NPolls_past, NPollsters, NCombinations, NElections_past,
                                       sigma_alpha,
                                       sigma_tau,
                                       sigma_xi, theta_matrix_blocs,
                                       prob_theta_matrix_blocs, theta_matrix_candidates, transition_matrix,
                                       id_C_blocs = id_C_blocs,
                                       combinations = TRUE)
  data_true_list[[jj]] <- data_true
  data_polls_list[[jj]] <- data_polls
}

################################################################################
## Datalist
data_polls <- bind_rows(
  data_polls_list[[1]]$polls %>%
    mutate(election = 1),
  data_polls_list[[2]]$polls %>%
    mutate(election = 2)
) %>%
  group_by(election, survey_id) %>%
  mutate(survey_id = cur_group_id()) %>%
  ungroup()
NSurveys <- data_polls %>%
  distinct(election, survey_id) %>%
  nrow()
NBlocs <- 6
NPolls <- data_polls %>%
  distinct(election, question_id) %>%
  nrow()
NSeasons <- 2
NCandidates <- data_polls %>%
  distinct(election, candidate_id) %>%
  group_by(election) %>%
  summarize(N = n()) %>%
  pull(N)
NPollsters <- data_polls %>%
  distinct(election, pollster_id) %>%
  nrow()
t_unit_df <- data_polls %>%
  distinct(election, time_id) %>%
  arrange(election, time_id) %>%
  group_by(election) %>%
  mutate(t_unit = 1:n(),
         t_unit_skip = time_id - lag(time_id)) %>%
  ungroup()
NTime <- t_unit_df %>%
  group_by(election) %>%
  summarize(N = n()) %>%
  pull(N)
t_unit <- matrix(-99, nrow = 2, ncol = max(NTime) - 1)

t_unit[1,1:(NTime[1] - 1)] <- t_unit_df %>% filter(election == 1, !is.na(t_unit_skip)) %>% pull(t_unit_skip)
t_unit[2,1:(NTime[2] - 1)] <- t_unit_df %>% filter(election == 2, !is.na(t_unit_skip)) %>% pull(t_unit_skip)

data_polls <- data_polls %>%
  arrange(election, pollster_id) %>%
  mutate(survey_id = as.integer(factor(survey_id, levels = unique(survey_id)))) %>%
  arrange(election, pollster_id, survey_id) %>%
  group_by(election, pollster_id) %>%
  mutate(pollster_id = cur_group_id()) %>%
  ungroup()



NSurveys_Pollster <- data_polls %>%
  distinct(survey_id, pollster_id) %>%
  group_by(pollster_id) %>%
  summarize(N = n()) %>%
  pull(N)
NPollsters_Season <- data_polls %>%
  distinct(election, pollster_id) %>%
  group_by(election) %>%
  summarize(N = n()) %>%
  pull(N)
id_C_blocs <- matrix(-99, nrow = 2, ncol = max(NCandidates))
id_C_blocs[1, 1:NCandidates[1]] <- data_polls %>%
  filter(election == 1) %>%
  distinct(candidate_id, bloc_id) %>%
  arrange(candidate_id) %>%
  pull(bloc_id)
id_C_blocs[2, 1:NCandidates[2]] <- data_polls %>%
  filter(election == 2) %>%
  distinct(candidate_id, bloc_id) %>%
  arrange(candidate_id) %>%
  pull(bloc_id)

id_P_survey <- data_polls %>%
  distinct(question_id, survey_id) %>%
  pull(survey_id)
id_S_season <-  data_polls %>%
  distinct(election, survey_id) %>%
  arrange(survey_id) %>%
  pull(election)
id_P_season <-  data_polls %>%
  distinct(election, pollster_id) %>%
  arrange(pollster_id) %>%
  pull(election)
id_S_pollster <- data_polls %>%
  distinct(survey_id, pollster_id) %>%
  arrange(survey_id) %>%
  pull(pollster_id)
id_S_pollster <- data_polls %>%
  distinct(survey_id, pollster_id) %>%
  arrange(survey_id) %>%
  pull(pollster_id)

id_S_time <- data_polls %>%
  left_join(t_unit_df) %>%
  distinct(election, survey_id, t_unit) %>%
  arrange(survey_id) %>%
  pull(t_unit)

inclusion_data1 <- create_variable_inclusion_input(data_polls %>%
                                                     filter(election == 1))
inclusion_data2 <- create_variable_inclusion_input(data_polls %>%
                                                     filter(election == 2))

NCandidates_Poll <- c(
  inclusion_data1$NCandidates_Poll,
  inclusion_data2$NCandidates_Poll
)

NCombinations <- c(
  inclusion_data1$NCombinations,
  inclusion_data2$NCombinations
)

NCandidate_Combinations <- matrix(-99, nrow = 2, ncol = max(NCombinations))
NCandidate_Combinations[1, 1:NCombinations[1]] <- inclusion_data1$NCandidate_Combinations
NCandidate_Combinations[2, 1:NCombinations[2]] <- inclusion_data2$NCandidate_Combinations

candidates_included <- array(-99, dim = c(
  max(NCombinations),
  2,
  max(NCandidates)
))
candidates_included[1:NCombinations[1],1, 1:NCandidates[1]] <- inclusion_data1$candidates_included
candidates_included[1:NCombinations[2],2, 1:NCandidates[2]] <- inclusion_data2$candidates_included

candidates_excluded <- array(-99, dim = c(
  max(NCombinations),
  2,
  max(NCandidates)
))
candidates_excluded[1:NCombinations[1],1, 1:NCandidates[1]] <- inclusion_data1$candidates_excluded
candidates_excluded[1:NCombinations[2],2, 1:NCandidates[2]] <- inclusion_data2$candidates_excluded

id_P_combinations <- c(
  inclusion_data1$combination_df$combination_id,
  inclusion_data2$combination_df$combination_id
)

y <- matrix(-99, nrow = max(NCandidates), ncol = NPolls)
y[1:NCandidates[1], 1:dim(inclusion_data1$y)[1]] <- inclusion_data1$y %>% t()
y[1:NCandidates[2], (dim(inclusion_data1$y)[1] + 1):NPolls] <- inclusion_data2$y %>% t()

abstention_omitted <- as.integer(y[1, ] == -99)

prior_sigma_alpha = 0.1
prior_sigma_tau = 0.1
prior_sigma_cov = 0.1
################################################################################
## Datalist
data_list <- list(
  NSurveys = NSurveys,
  NBlocs = NBlocs,
  NPolls = NPolls,
  NSeasons = NSeasons,
  NCandidates = NCandidates,
  NPollsters = NPollsters,
  NTime = NTime,
  t_unit = t_unit,

  NSurveys_Pollster = NSurveys_Pollster,
  NPollsters_Season = NPollsters_Season,
  id_C_blocs = id_C_blocs,
  id_P_survey = id_P_survey,
  id_P_season = id_P_season,
  id_S_season = id_S_season,
  id_S_pollster = id_S_pollster,
  id_S_time = id_S_time,
  abstention_omitted = abstention_omitted,

  NCandidates_Poll = NCandidates_Poll,
  NCombinations = NCombinations,
  NCandidate_Combinations = NCandidate_Combinations,
  candidates_included = candidates_included,
  candidates_excluded = candidates_excluded,
  id_P_combinations = id_P_combinations,
  y = y,

  # prior_sigma_alpha = prior_sigma_alpha + 1,
  # prior_sigma_tau = prior_sigma_tau + 1,
  # prior_sigma_cov = prior_sigma_cov + 1
  prior_sigma_alpha = 0.001,
  prior_sigma_tau = 0.001,
  prior_sigma_cov = 0.001
)
################################################################################
## Model
mod <- cmdstanr::cmdstan_model("src/stan/primary_model.stan")
################################################################################
## Fit model
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 300,
  iter_warmup = 300,
  parallel_chains = 6,
  refresh = 100,
  init = 1
)
################################################################################
## Evaluate
#' Time
source("src/R/functions/performance_check.R")
performance_check(fit)
#' Recovery check
fit$summary("sigma_alpha")
fit$summary("sigma_tau")
fit$summary("sigma_cov")
data_true_list[[1]]$transition_matrix_random_walk_candididates %>%
  diag() %>% sqrt()
data_true_list[[2]]$transition_matrix_random_walk_candididates %>%
  diag() %>% sqrt()
##
fit$summary("sum_bloc_prob")
################################################################################