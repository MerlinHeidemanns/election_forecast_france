## Clean
rm(list = ls())
## Libraries
library(tidyverse)
library(cmdstanr)
## Source
source("src/R/functions/exp_softmax.R")
source("src/R/functions/sim_random_walk_bloc.R")
source("src/R/functions/sim_polling_data_blocs.R")
source("src/R/functions/ppc_plt_pi_theta_first_round.R")
source("src/R/functions/ppc_plt_pi_theta_second_round.R")
source("src/R/functions/ppc_plt_sigma_tau.R")
source("src/R/functions/ppc_plt_sigma_alpha.R")
source("src/R/functions/ppc_plt_sigma_xi.R")
source("src/R/functions/ppc_plt_alpha.R")
source("src/R/functions/ppc_plt_xi.R")
source("src/R/functions/ppc_plt_xi_past.R")
source("src/R/functions/ppc_plt_cov_theta.R")
source("src/R/functions/create_y_first_round.R")
source("src/R/functions/create_variable_inclusion_input.R")
source("src/R/functions/ppc_plt_alpha_sum_to_0.R")
source("src/R/functions/ppc_plt_sigma_cov.R")
source("src/R/functions/ppc_plt_sum_alpha_xi.R")
source("src/R/functions/data_list_check_y_1r.R")
## Simulate true data
NElections_past <- 4
NCandidates <- 8
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


## Plot simulated data
plt_1st_round <- ggplot(data_true$df_blocs, aes(x = time_id,
                                            y = share,
                                            color =as.factor(bloc_id))) +
geom_line()
plt_2nd_round <- ggplot(data_true$df_candidates, aes(x = time_id,
                                      y = share,
                                      color = as.factor(candidate_id))) +
geom_line()
gridExtra::grid.arrange(plt_1st_round, plt_2nd_round)


## Prepare data
# -- Create unit steps
#' Current election
t_unit_df <- data_polls$polls %>%
  distinct(time_id) %>%
  arrange(time_id) %>%
  mutate(t_unit = 1:n())
t_unit_skip <- t_unit_df %>%
  mutate(t_skip = time_id - lag(time_id)) %>%
  filter(!is.na(t_skip)) %>%
  pull(t_skip)
data_polls$polls <- data_polls$polls %>%
  left_join(t_unit_df, by = "time_id")
#' Past election
t_unit_df_past <- data_polls$polls_past %>%
  distinct(time_id) %>%
  add_row(time_id = data_polls$election_time_points) %>%
  arrange(time_id) %>%
  mutate(t_unit = 1:n())
t_unit_skip_past <- t_unit_df_past %>%
  mutate(t_skip = time_id - lag(time_id)) %>%
  filter(!is.na(t_skip)) %>%
  pull(t_skip)
data_polls$polls_past <- data_polls$polls_past %>%
  left_join(t_unit_df_past, by = "time_id")

data_polls$election_df <- data_polls$election_df %>%
  left_join(t_unit_df_past, by = "time_id")
###############################################################################
## Past polls
#' Should show that there is no overlap
all(data_polls$polls_past %>%
      arrange(election_id) %>%
      group_by(election_id) %>%
      summarize(min_t = min(t_unit),
                max_t = max(t_unit)) %>%
      ungroup() %>%
      mutate(lead_min_t = lead(min_t)) %>%
      filter(!is.na(lead_min_t)) %>%
      mutate(okay = lead_min_t > min_t) %>%
      pull(okay))
#' Arrange in correct order
data_polls$polls_past <- data_polls$polls_past %>%
  arrange(election_id, pollster_id, poll_id, bloc_id) %>%
  group_by(election_id, pollster_id) %>%
  mutate(pollster_election_id = cur_group_id()) %>%
  ungroup() %>%
  arrange(election_id, pollster_election_id, poll_id, bloc_id)
#' How many polls by pollster x election
NPolls_Pollster_past <- data_polls$polls_past %>%
  distinct(election_id, pollster_election_id, poll_id) %>%
  group_by(election_id, pollster_election_id) %>%
  summarize(n = n()) %>%
  pull(n)
#' Count of pollsters per election
NPollsters_past <- data_polls$polls_past %>%
  distinct(election_id, pollster_id) %>%
  group_by(election_id) %>%
  summarize(n = n()) %>%
  pull(n)
#' Skip vector distances
t_unit_past <- t_unit_skip_past
#' time of pollst in unit time
id_P_time_past <- data_polls$polls_past %>%
  distinct(poll_id, t_unit) %>%
  pull(t_unit)
#' id of pollster
#' Create new pollster id that is unique overall
id_P_pollster_past <- data_polls$polls_past %>%
  distinct(poll_id, pollster_id, election_id) %>%
  group_by(pollster_id, election_id) %>%
  mutate(pollster_election_id = pollster_id + NPollsters_past[1] * (election_id - 1)) %>%
  pull(pollster_election_id)
#' index for which election polls belong to
id_P_elections_past <- data_polls$polls_past %>%
  distinct(poll_id, election_id) %>%
  pull(election_id)
#' past election results
election_results <- data_polls$results_matrix
#' Time in unit time of elections
id_E_time <- data_polls$election_df %>%
  distinct(time_id, t_unit) %>%
  pull(t_unit)
#' y_past
y_past <- data_polls$polls_past %>%
  dplyr::select(poll_id, bloc_id, y) %>%
  pivot_wider(id_cols = poll_id,
            names_from = bloc_id,
            values_from = y) %>%
  dplyr::select(-poll_id) %>%
  as.matrix()
#' Abstention omitted in the past
#' 1 = omitted, 0 included
abstention_omitted_past <- data_polls$polls_past %>%
  distinct(poll_id, abstention_omitted) %>%
  pull(abstention_omitted)
###############################################################################
## Current data
#' Closest time point to the previous election
t_bloc_unit_prior <- data_polls$polls %>%
  pull(time_id) %>%
  min()
#' Arrange the survey id such that correct elements are grabbed
#' by the indexes
data_polls$polls <- data_polls$polls %>%
  arrange(pollster_id, survey_id, question_id) %>%
  mutate(survey_id = as.integer(factor(survey_id, levels = unique(survey_id))))
#' Create inclusion data
inclusion_data <- create_variable_inclusion_input(data_polls$polls)
#' Check whether inclusion data is in the right order
inclusion_data_order <- all(inclusion_data$combination_df$question_id == data_polls$polls %>%
  distinct(question_id) %>%
  pull(question_id))
if (inclusion_data_order == FALSE){
  stop('Inclusion data in the incorrect order.')
}
#' Transition probability prior
#' Add 1 to candidates because datalist also adds 1 for abstentions
transition_probability_prior <- matrix(20,
                                       nrow = NCandidates + 1,
                                       ncol = NCandidates)
###############################################################################
## Data list
data_list <- list(
  NElections = NElections_past + 1,
  NBlocs = 6,
  NSurveys = data_polls$polls %>%
    distinct(survey_id) %>%
    nrow(),
  NPolls = data_polls$polls %>%
    distinct(question_id) %>%
    nrow(),
  NSurveys_Pollster = data_polls$polls %>%
    distinct(pollster_id, survey_id) %>%
    group_by(pollster_id) %>%
    summarize(N = n()) %>%
    pull(N),
  NBlocs_Candidates = data_polls$polls %>%
    distinct(candidate_id, bloc_id) %>%
    group_by(bloc_id) %>%
    summarize(n = n()) %>%
    pull(n),
  id_P_survey = data_polls$polls %>%
    distinct(survey_id, question_id) %>%
    pull(survey_id),
  id_C_blocs = id_C_blocs,
  NCandidates = NCandidates + 1,
  NPollsters = NPollsters,
  NTime = nrow(t_unit_df),
  t_unit = t_unit_skip,
  id_S_time = data_polls$polls %>%
    distinct(survey_id, t_unit) %>%
    arrange(survey_id) %>% ## We access elements out of here so this has to be ordered
    pull(t_unit),
  id_S_pollster = data_polls$polls %>%
    distinct(survey_id, pollster_id) %>%
    arrange(survey_id) %>% ## We access elements out of here so this has to be ordered
    pull(pollster_id),
  transition_probability_prior = transition_probability_prior,
  ## -- variable inclusion dynamic
  NCandidates_Poll = inclusion_data$NCandidates_Poll,
  NCombinations = inclusion_data$NCombinations,
  NCandidate_Combinations = inclusion_data$NCandidate_Combinations %>% array(),
  candidates_included = inclusion_data$candidates_included,
  candidates_excluded = inclusion_data$candidates_excluded,
  id_P_combinations = inclusion_data$combination_df$combination_id,
  y = inclusion_data$y %>% t(),

  ## -- past data
  NPolls_Pollster_past = NPolls_Pollster_past %>% array(),
  NPollsters_past = NPollsters_past %>% array(),
  NTime_past = nrow(t_unit_df_past),
  t_bloc_unit_prior = t_bloc_unit_prior,
  t_unit_past = t_unit_past,
  id_P_time_past = id_P_time_past,
  id_P_pollster_past = id_P_pollster_past,
  id_P_elections_past = id_P_elections_past,
  elections_results = round(election_results) %>% t(),
  id_T_election = id_T_election,
  id_E_time = id_E_time,
  y_past = y_past %>% t(),
  abstention_omitted_past = abstention_omitted_past,
  abstention_omitted = data_polls$polls %>%
    distinct(question_id, abstention_omitted) %>%
    pull(abstention_omitted),

  abstention_omitted_pollster = data_polls$polls %>%
    distinct(pollster_id, abstention_omitted) %>%
    arrange(pollster_id) %>%
    pull(abstention_omitted),

  abstention_omitted_pollster_past = data_polls$polls_past %>%
    distinct(pollster_election_id, abstention_omitted) %>%
    arrange(pollster_election_id) %>%
    pull(abstention_omitted),

  prior_sigma_xi = sigma_xi,
  prior_sigma_alpha = sigma_alpha,
  prior_sigma_tau = sigma_tau,
  prior_sigma_cov = sigma
)
###############################################################################
## Input checks
if (FALSE){
  y_share_data_list <- data_list$y %>%
    as.data.frame() %>%
    mutate(candidate_id = 1:9) %>%
    pivot_longer(c(-candidate_id),
                names_to = "id",
                values_to = "y",
                names_prefix = "V") %>%
    mutate(id = as.integer(id)) %>%
    arrange(id, candidate_id) %>%
    group_by(id) %>%
    mutate(share = y/sum(y)) %>%
    pull(share)
  y_share_data <- data_polls$polls %>%
    group_by(question_id) %>%
    mutate(share = y/sum(y)) %>%
    pull(share)
  plot(y_share_data, y_share_data_list)

  ## Should evaluate to true
  all(data_list$y == data_polls$polls %>%
    select(y, candidate_id, question_id) %>%
    pivot_wider(id_cols = question_id,
                names_from = candidate_id,
                values_from = y) %>%
    select(-question_id) %>%
    as.matrix() %>%
    t())

  ## Time points
  data_polls$polls %>%
    distinct(question_id, t_unit, survey_id)
  data_list$id_S_time[data_list$id_P_survey]



  id_S_time = data_polls$polls %>%
    distinct(survey_id, t_unit) %>%
    pull(t_unit)

  id_P_survey = data_polls$polls %>%
    distinct(survey_id, question_id) %>%
    pull(survey_id)
}
## Abstention omitted correct
all((data_list$y_past[1,] < 0) == data_list$abstention_omitted_past)
###############################################################################
## Load model
mod <- cmdstan_model("src/stan/v2_main_add_combinations_back_in.stan")
## Fit model
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 200,
  iter_warmup = 400,
  parallel_chains = 6,
  refresh = 100,
  init = 1
)
###############################################################################
## Posterior Predictive Checks
#' Performance and behavior checks
source("src/R/functions/performance_check.R")
performance_check(fit)
## Tau
source("src/R/functions/functionality_check_tau.R")
functionality_check_tau(fit, data_list)
source("src/R/functions/functionality_check_tau_past.R")
functionality_check_tau_past(fit, data_list)
#' Qualitative evaluation
source("src/R/functions/ppc_plt_epsilon.R")
ppc_plt_epsilon(fit, data_list)
# Plot prob_theta by blocs for past
source("src/R/functions/ppc_plt_theta_blocs.R")
ppc_plt_theta_blocs(fit, data_polls$polls_past, t_unit_df_past, data_true$df_blocs)
# Plot pi_theta
ppc_plt_pi_theta_first_round(fit, data_polls$polls, t_unit_df, data_true$df_candidates)
# Plot sigma_tau
ppc_plt_sigma_tau(fit, data_polls$sigma_parameters$sigma_tau)
# Plot sigma_alpha
ppc_plt_sigma_alpha(fit, data_polls$sigma_parameters$sigma_alpha)
# Plot sigma_xi
ppc_plt_sigma_xi(fit, data_polls$sigma_parameters$sigma_xi)
# Plot alpha
ppc_plt_alpha(fit, true_alpha = data_polls$true_alpha_current)
# Plot alpha_past
source("src/R/functions/ppc_plt_alpha_past.R")
ppc_plt_alpha_past(fit, data_list, data_polls)
# Plot xi
ppc_plt_xi(fit, true_xi = data_polls$true_xi_current)
# Sum to zero constraint
ppc_plt_alpha_sum_to_0(fit)
# sigma_cov
ppc_plt_sigma_cov(fit, data_true$transition_matrix_random_walk_candididates)
# sigma_cov_blocs
source("src/R/functions/ppc_plt_cov_theta_blocs.R")
ppc_plt_cov_theta_blocs(fit, data_true$transition_matrix_random_walk_blocs)
## Xi against sum of polling house deviations
ppc_plt_sum_alpha_xi(fit)
## Plot xi_past_hat
ppc_plt_xi_past(fit, data_polls$true_xi_past)
## Pair plot for xi_past
source("src/R/functions/ppc_plt_xi_past_pair.R")
ppc_plt_xi_past_pair(fit, past_election = 3, 300)
## Pair plot for xi
source("src/R/functions/ppc_plt_xi_pair.R")
ppc_plt_xi_pair(fit, 300)
## Alpha sum tau
source("src/R/functions/ppc_plt_alpha_sum_tau.R")
ppc_plt_alpha_sum_tau(fit, data_list)
## Alpha sum tau
source("src/R/functions/ppc_plt_alpha_sum_tau_past.R")
ppc_plt_alpha_sum_tau_past(fit, data_list)
## Trace plots
np <- bayesplot::nuts_params(fit)
bayesplot::mcmc_trace(fit$draws(c("sigma_tau", "sigma_alpha", "sigma_xi")))
bayesplot::mcmc_trace(fit$draws(c("theta_blocs[2,40]",
                                  "theta_blocs[2,41]",
                                  "theta_blocs[2,42]",
                                  "theta_blocs[2,43]",
                                  "theta_blocs[2,44]")))
## Prior theta candidates
bayesplot::mcmc_trace(fit$draws(c("prior_theta_candidates")))
bayesplot::mcmc_pairs(fit$draws(c("prior_theta_candidates", "lp__")))
## Sigmas
bayesplot::mcmc_trace(fit$draws(c("sigma_tau", "sigma_alpha", "sigma_xi")))
bayesplot::mcmc_pairs(fit$draws(c("sigma_tau", "sigma_alpha", "sigma_xi",
                                  "lp__")),
                      np = np)

fit$draws("tau_past") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "var",
               values_to = "draws") %>%
  mutate(
    bloc = str_match(var, "([\\d]+)," )[,2]
  ) %>%
  filter(!is.na(bloc)) %>%
  ggplot(aes(x = draws)) +
    geom_histogram(bins = 100) +
    facet_wrap(bloc ~ .)