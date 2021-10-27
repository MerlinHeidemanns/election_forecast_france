## Clean
rm(list = ls())
## Libraries
library(tidyverse)
library(cmdstanr)
## Source
source("src/R/functions/exp_softmax.R")
source("src/R/functions/sim_random_walk.R")
source("src/R/functions/sim_polling_data.R")
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
## Generate data
#' Parameters
#' Fake true data
#' Observed polls
NTime <- 40
NPolls <- 40
NPolls_past <- 20
NElections_past <- 1
NCandidates <- 5
NPollsters <- 3
NCombinations <- 4
NPollsters_past <- 3
data <- sim_random_walk(NElections_past = NElections_past,
                        NCandidates = NCandidates,
                        NTime = NTime,
                        rho = 0.1,
                        sigma = 0.1,
                        K_VAR = 1)
df <- sim_polling_data(NPolls = NPolls,
                       NPolls_past = NPolls_past,
                       NPollsters = NPollsters,
                       NPollsters_past = NPollsters_past,
                       NCombinations = NCombinations,
                       sigma_alpha = 0.2,
                       sigma_tau = 0.2,
                       sigma_xi = 0.2,
                       theta_matrix = data$theta_matrix,
                       transition_matrix = data$transition_matrix_preferences,
                       prob_theta_past = data$prob_theta_past)


## Plot simulated data
plt_1st_round <- ggplot(data$df, aes(x = t, y = share, color = candidate_id)) +
  geom_line()
plt_2nd_round <- ggplot(data$df_coll, aes(x = t, y = share, color = as.factor(candidate_id))) +
  geom_line()
gridExtra::grid.arrange(plt_1st_round, plt_2nd_round)



## Prepare data
# -- Create unit steps
t_unit_df <- df$polls %>%
  distinct(time_id) %>%
  arrange(time_id) %>%
  mutate(t_unit = 1:n())
t_unit_skip <- t_unit_df %>%
  mutate(t_skip = time_id - lag(time_id)) %>%
  filter(!is.na(t_skip)) %>%
  pull(t_skip)
df$polls <- df$polls %>%
  left_join(t_unit_df, by = "time_id")

inclusion_data <- create_variable_inclusion_input(df$polls)

transition_probability_prior <- matrix(1,
                                       nrow = NCandidates + 1,
                                       ncol = NCandidates)


data_list <- list(
  NSurveys = df$polls %>%
    distinct(survey_id) %>%
    nrow(),
  NPolls = df$polls %>%
    distinct(question_id) %>%
    nrow(),
  id_P_survey = df$polls %>%
    distinct(survey_id, question_id) %>%
    pull(survey_id),
  NCandidates = NCandidates + 1,
  NPollsters = NPollsters,
  NTime = nrow(t_unit_df),
  t_unit = t_unit_skip,
  id_S_time = df$polls %>%
    distinct(survey_id, t_unit) %>%
    pull(t_unit),
  id_S_pollster = df$polls %>%
    distinct(survey_id, pollster_id) %>%
    pull(pollster_id),
  ## -- variable inclusion dynamic
  NCandidates_Poll = inclusion_data$NCandidates_Poll,
  NCombinations = inclusion_data$NCombinations,
  NCandidate_Combinations = inclusion_data$NCandidate_Combinations,
  candidates_included = inclusion_data$candidates_included,
  candidates_excluded = inclusion_data$candidates_excluded,
  id_P_combinations = inclusion_data$combination_id,
  y = inclusion_data$y %>% t(),

  ## -- past data
  NElections_past = df$NElections_past,
  NPolls_past = df$polls_past %>%
    distinct(t, survey_id) %>%
    arrange(t) %>%
    group_by(t) %>%
    summarize(n = n()) %>%
    pull(n) %>%
    array(),
  NCandidates_past = df$NCandidates_past %>%
    array(),
  NPollsters_past = df$polls_past %>%
    distinct(t, pollster_id) %>%
    arrange(t) %>%
    group_by(t) %>%
    summarize(n = n()) %>%
    pull(n) %>%
    array(),
  id_r_past = df$polls_past %>%
    distinct(survey_id, pollster_id) %>%
    pull(pollster_id),
  id_rt_past = df$polls_past %>%
    distinct(pollster_id, t) %>%
    arrange(pollster_id) %>%
    pull(t),
  id_t_past = df$polls_past %>%
    distinct(survey_id, t) %>%
    pull(t),
  results = data$prob_theta_past %>%
    t(),
  y_past = df$polls_past %>%
    select(survey_id, y, candidate_id) %>%
    pivot_wider(id_cols = survey_id,
                names_from = candidate_id,
                values_from = y,
                values_fill = 0) %>%
    select(-survey_id) %>%
    as.matrix() %>%
    t(),
  abstention_omitted = df$polls %>%
    distinct(question_id, abstention_omitted) %>%
    pull(abstention_omitted),
  abstention_omitted_past = df$polls_past %>%
    distinct(survey_id, abstention_omitted) %>%
    pull(abstention_omitted),

  transition_probability_prior = transition_probability_prior
)


## Load model
mod <- cmdstan_model("src/stan/v1_with_past.stan")


## Fit model
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 500,
  iter_warmup = 300,
  parallel_chains = 6,
  refresh = 100
)


## Posterior Predictive Checks



df$sigma_parameters
# Plot pi_theta
ppc_plt_pi_theta_first_round(fit, df$polls, t_unit_df, data$df)
# Plot sigma_tau
ppc_plt_sigma_tau(fit, df$sigma_parameters$sigma_tau)
# Plot sigma_alpha
ppc_plt_sigma_alpha(fit, df$sigma_parameters$sigma_alpha)
# Plot sigma_xi
ppc_plt_sigma_xi(fit, df$sigma_parameters$sigma_xi)
# Plot alpha
ppc_plt_alpha(fit, true_alpha = df$alpha)
# Plot xi
ppc_plt_xi(fit, true_xi = df$xi)
# Sum to zero constraint
ppc_plt_alpha_sum_to_0(fit)
# sigma_cov
ppc_plt_sigma_cov(fit, data$transition_matrix_random_walk)
## Xi against sum of polling house deviations
ppc_plt_sum_alpha_xi(fit)
## Plot xi_past_hat
ppc_plt_xi_past(fit, df$xi_past)
## Pair plot for xi_past
source("src/R/functions/ppc_plt_xi_past_pair.R")
ppc_plt_xi_past_pair(fit, past_election = 1, 300)
## Pair plot for xi
source("src/R/functions/ppc_plt_xi_pair.R")
ppc_plt_xi_pair(fit, 300)

ppc_plt_alpha_sum_tau(fit, data_list, 3)





ppc_plt_alpha_sum_tau <- function(fit, data_list, pollster_n){
  tau_pollster <- fit$draws("tau") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    filter(iter < 200) %>%
    pivot_longer(c(-iter),
                 names_to = "variable",
                 values_to = "draws") %>%
    mutate(
      candidate_id = as.integer(str_match(variable, "([\\d]+),")[,2]),
      survey_id = as.integer(str_match(variable, ",([\\d]+)")[,2])
    ) %>%
    dplyr::select(-variable) %>%
    filter(!is.na(survey_id)) %>%
    left_join(data.frame(survey_id = 1:NPolls,
                         pollster_id = data_list$id_S_pollster)) %>%
    filter(pollster_id == pollster_n) %>%
    rename(draws_tau = draws) %>%
    group_by(pollster_id, candidate_id, iter) %>%
    summarize(sum_draws_tau = sum(draws_tau)) %>%
    ungroup() %>%
    dplyr::select(-pollster_id)


  full_join(tau_pollster %>%
              rename(sum_draws_tau_a = sum_draws_tau,
                     candidate_id_a = candidate_id),
            tau_pollster %>%
              rename(sum_draws_tau_b = sum_draws_tau,
                     candidate_id_b = candidate_id)) %>%
    ggplot(aes(x = sum_draws_tau_a, y = sum_draws_tau_b)) +
      geom_point(alpha = 0.3) +
      facet_grid(candidate_id_a ~ candidate_id_b) +
      coord_equal()

  alpha <- fit$draws("alpha") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "variable",
                 values_to = "draws") %>%
    mutate(
      candidate_id = as.integer(str_match(variable, "([\\d]+),")[,2]),
      pollster_id = as.integer(str_match(variable, ",([\\d]+)")[,2])
    ) %>%
    dplyr::select(-variable) %>%
    filter(!is.na(pollster_id), pollster_id == pollster_n) %>%
    rename(draws_alpha = draws)

  plt <- left_join(tau_pollster, alpha) %>%
    ggplot(aes(x = draws_alpha, y = sum_draws_tau)) +
    geom_point(alpha = 0.3) +
    theme_light() +
    facet_wrap(candidate_id ~ .)
  return(plt)
}
















