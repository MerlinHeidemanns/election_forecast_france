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
source("src/R/functions/ppc_plt_alpha.R")
source("src/R/functions/ppc_plt_xi.R")
source("src/R/functions/ppc_plt_cov_theta.R")
source("src/R/functions/create_y_first_round.R")
source("src/R/functions/create_variable_inclusion_input.R")
source("src/R/functions/ppc_plt_alpha_sum_to_0.R")
source("src/R/functions/ppc_plt_sigma_cov.R")
source("src/R/functions/ppc_plt_sum_alpha_xi.R")
source("src/R/functions/data_list_check_y_1r.R")
source("src/R/functions/ppc_plt_y_yhat.R")
## Generate data
#' Parameters
#' Fake true data
#' Observed polls
T <- 100
T_prior <- 10
N_first_round_surveys <- 10
N_second_round <- 20
N_first_round_past <- 60
N_past_election <- 4
NCandidate <- 6
N_R <- 3
N_combinations <- 2
data <- sim_random_walk(N_past_election = N_past_election,
                        NCandidate = NCandidate,
                        T = T,
                        T_prior = T_prior,
                        rho = 0.1,
                        sigma = 0.1,
                        K_VAR = 1)
df <- sim_polling_data(N_first_round_surveys = N_first_round_surveys,
                       N_second_round = N_second_round,
                       N_first_round_past = N_first_round_past,
                       N_R = N_R,
                       N_combinations = N_combinations,
                       sigma_alpha = 0.2,
                       sigma_tau = 0.2,
                       sigma_xi = 0.2,
                       data$eta_matrix,
                       transition_matrix = data$transition_matrix,
                       pi_past = data$pi_past)


## Plot simulated data
plt_1st_round <- ggplot(data$df, aes(x = t, y = share, color = candidate_id)) +
  geom_line()
plt_2nd_round <- ggplot(data$df_coll, aes(x = t, y = share, color = candidate_id)) +
  geom_line()
gridExtra::grid.arrange(plt_1st_round, plt_2nd_round)
cat("Distribution of error in second round polls")
df$polls_second_round %>%
  left_join(data$df_coll) %>%
  filter(candidate_id == 1) %>%
  mutate(error = share - y/n) %>%
  pull(error) %>%
  summary()


## Prepare data
# -- Create unit steps
t_1r <- df$polls_first_round %>%
  pull(t)
t_2r <- df$polls_second_round %>%
  pull(t)
t_unit_df <- data.frame(t = c(t_1r, t_2r)) %>%
  distinct(t) %>%
  arrange(t) %>%
  mutate(t_unit = 1:n())
t_unit_skip <- t_unit_df %>%
  mutate(t_skip = t - lag(t)) %>%
  filter(!is.na(t_skip)) %>%
  pull(t_skip)
df$polls_first_round <- df$polls_first_round %>%
  left_join(t_unit_df, by = "t")
df$polls_second_round <- df$polls_second_round %>%
  left_join(t_unit_df, by = "t")


inclusion_data <- create_variable_inclusion_input(df$polls_first_round)
data_list <- list(
  NSurveys = df$polls_first_round %>%
    distinct(survey_id) %>%
    nrow(),
  NPolls = df$polls_first_round %>%
    distinct(question_id) %>%
    nrow(),
  NCandidates = df$polls_first_round %>%
    distinct(candidate_id) %>%
    nrow(),
  NPolls_Candidates = df$polls_first_round %>%
    nrow(),
  NPollsters = df$polls_first_round %>%
    distinct(r) %>%
    nrow(),
  TUnit = nrow(t_unit_df),
  t_unit = t_unit_skip,

  y = df$polls_first_round %>%
    mutate(logit_pr = y/n) %>%
    pull(logit_pr),
  n = df$polls_first_round %>%
    pull(n),

  Sid_t = df$polls_first_round %>%
    distinct(survey_id, t_unit) %>%
    arrange(survey_id) %>%
    pull(t_unit),
  Sid_r = df$polls_first_round %>%
    distinct(survey_id, r) %>%
    arrange(survey_id) %>%
    pull(r),

  NCandidates_p = df$polls_first_round %>%
    group_by(question_id) %>%
    mutate(z = 1) %>%
    summarize(NCandidates_p = sum(z)) %>%
    pull(NCandidates_p),

  Pid_s = df$polls_first_round %>%
    distinct(question_id, survey_id) %>%
    arrange(question_id) %>%
    pull(survey_id),

  Candidates_Polls = inclusion_data$NCandidate_first_round,
  NCombinations = inclusion_data$N_combinations,
  NCandidates_Combinations = inclusion_data$P_N_combinations %>% array(),
  Candidates_included = inclusion_data$candidate_first_round_included,
  Candidates_excluded = inclusion_data$candidate_first_round_excluded,
  id_c = inclusion_data$candidate_id,

  NElections_past = df$N_elections_past,
  NPolls_past = df$polls_first_round_past %>%
    distinct(id) %>%
    nrow(),
  NCandidates_past = df$P_past_elections %>%
    array(),
  NPollsters_past = df$polls_first_round_past %>%
    distinct(r_id) %>%
    nrow(),
  id_p_past = df$polls_first_round_past %>%
    distinct(id, r_id) %>%
    pull(r_id),
  id_rt_past = df$polls_first_round_past %>%
    distinct(r_id, t) %>%
    arrange(r_id) %>%
    pull(t),
  id_t_past = df$polls_first_round_past %>%
    distinct(id, t) %>%
    pull(t),
  results = data$pi_past %>%
    t(),
  y_past = df$polls_first_round_past %>%
    select(id, y, p) %>%
    pivot_wider(id_cols = id,
                names_from = p,
                values_from = y,
                values_fill = 0) %>%
    select(-id) %>%
    as.matrix() %>%
    t()
)

## Checks
data_list_check_y_1r(data_list)

#
# NCombinations <- data_list$NCombinations
# NCandidates_Combinations <- data_list$NCandidates_Combinations
# NCandidates <- data_list$NCandidates
# NCandidates_Combinations_neg <- NCandidates - NCandidates_Combinations
# Candidates_included <- data_list$Candidates_included
# Candidates_excluded <- data_list$Candidates_excluded
# for (i in 1:NCombinations){
#   print(Candidates_excluded[i, 1:NCandidates_Combinations[i]])
#   print(Candidates_included[i, 1:NCandidates_Combinations_neg[i]])
# }


## Load model
mod <- cmdstan_model("src/stan/v1_current_election_normal_approximation.stan")
#mod <- cmdstan_model("src/stan/v2_current_election.stan")
## Fit model
fit <- mod$sample(
  data = data_list,
  chains = 4,
  iter_sampling = 500,
  iter_warmup = 500,
  parallel_chains = 4,
  refresh = 250,
  init = 0.2
)


pi_theta_first_round <- fit$draws("theta") %>%
  posterior::as_draws_df() %>%
  pivot_longer(
    everything(),
    names_to = "pt",
    values_to = "draws"
  ) %>%
  mutate(
    candidate_id = str_match(pt, "(\\d+),")[, 2],
    t_unit = as.integer(str_match(pt, ",(\\d+)")[, 2]),
    draws = boot::inv.logit(draws)
  )
## Posterior Predictive Checks
# Plot pi_theta
ppc_plt_pi_theta_first_round(fit, df$polls_first_round, t_unit_df, data$df)
ppc_plt_pi_theta_second_round(fit, df$polls_second_round, t_unit_df, data$df_coll)
# Plot sigma_tau
ppc_plt_sigma_tau(fit, 0.2)
# Plot sigma_alpha
ppc_plt_sigma_alpha(fit, 0.2)
# Plot alpha
ppc_plt_alpha(fit, true_alpha = df$alpha)
# Plot xi
ppc_plt_xi(fit, true_xi = df$xi)
# cov_theta
ppc_plt_cov_theta(fit, transition_matrix = data$transition_matrix)
# Sum to zero constraint
ppc_plt_alpha_sum_to_0(fit)
# sigma_cov
ppc_plt_sigma_cov(fit, data$transition_matrix)
#
ppc_plt_sum_alpha_xi(fit)
## y against yhat
ppc_plt_y_yhat(fit, data_list)

tau <- fit$draws("tau") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everythings(),
               names_to = "survey_id",
               values_to = "draws") %>%
  mutate(
    candidate_id = str_match(pt, "(\\d+),")[, 2],
    survey_id = as.integer(str_match(pt, ",(\\d+)")[, 2])
  )






