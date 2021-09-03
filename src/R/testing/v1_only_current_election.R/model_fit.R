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
## Load model
mod <- cmdstan_model("src/stan/v1_current_election.stan")
## Generate data
#' Parameters
#' Fake true data
#' Observed polls
T <- 10
T_prior <- 10
N_first_round_surveys <- 7
N_second_round <- 10
N_first_round_past <- 40
N_past_election <- 3
P_both <- 4
P_past <- 2
P_new <- 2
N_R <- 3
data <- sim_random_walk(N_past_election = N_past_election,
                        P_both = P_both,
                        P_past = P_past,
                        P_new = P_new,
                        T = T,
                        T_prior = T_prior,
                        rho = 0.1,
                        sigma = 0.1,
                        K_VAR = 1)
df <- sim_polling_data(N_first_round_surveys = N_first_round_surveys,
                       N_second_round = N_second_round,
                       N_first_round_past = N_first_round_past,
                       N_R = N_R,
                       sigma_alpha = 0.2,
                       sigma_tau = 0.2,
                       sigma_xi = 0.2,
                       data$eta_matrix,
                       transition_matrix = data$transition_matrix,
                       pi_past = data$pi_past)


## Plot simulated data
plt_1st_round <- ggplot(data$df, aes(x = t, y = share, color = p)) +
  geom_line()
plt_2nd_round <- ggplot(data$df_coll, aes(x = t, y = share, color = p)) +
  geom_line()
gridExtra::grid.arrange(plt_1st_round, plt_2nd_round)
cat("Distribution of error in second round polls")
df$polls_second_round %>%
  left_join(data$df_coll) %>%
  filter(p == 1) %>%
  mutate(error = share - y/n) %>%
  pull(error) %>%
  summary()


## Prepare data
inclusion_data <- create_variable_inclusion_input(df$polls_first_round)
data_list <- list(
  S_1r_surveys = df$polls_first_round %>%
    distinct(id) %>%
    nrow(),
  N_1r = df$polls_first_round %>%
    distinct(question_id) %>%
    nrow(),
  s_1r = df$polls_first_round %>%
    distinct(id, question_id) %>%
    pull(id),
  N_2r = df$polls_second_round %>%
    distinct(id) %>%
    nrow(),
  P = P_new + P_both,
  P_past_present = P_new + P_both + P_past,
  R = N_R,
  T = T,
  T_prior = T_prior,
  theta_prior = data$eta_start,
  t_1r = df$polls_first_round %>%
    distinct(id, t) %>%
    pull(t),
  t_2r = df$polls_second_round %>%
    distinct(id, t) %>%
    pull(t),
  r_1r = df$polls_first_round %>%
    distinct(id, r) %>%
    pull(r),
  r_2r = df$polls_second_round %>%
    distinct(id, r) %>%
    pull(r),
  ## -- variable inclusion dynamic
  P_1r = inclusion_data$P_first_round,
  N_combinations = inclusion_data$N_combinations,
  P_N_combinations = inclusion_data$P_N_combinations,
  p_1r_included = inclusion_data$p_first_round_included,
  p_1r_excluded = inclusion_data$p_first_round_excluded,
  p_id = inclusion_data$p_id,

  ##
  y_1r = inclusion_data$y_first_round %>% t(),
  y_2r = df$polls_second_round %>%
    pull(y),
  n_2r = df$polls_second_round %>%
    pull(n),
  N_elections_past = df$N_elections_past,
  N_1r_past = df$polls_first_round_past %>%
    distinct(id) %>%
    nrow(),
  P_past = df$P_past_elections,
  R_past = df$polls_first_round_past %>%
    distinct(r) %>%
    nrow(),
  r_past = df$polls_first_round_past %>%
    distinct(id, r_id) %>%
    pull(r_id),
  rt_past = df$polls_first_round_past %>%
    distinct(r_id, t) %>%
    arrange(r_id) %>%
    pull(t),
  t_past = df$polls_first_round_past %>%
    distinct(id, t) %>%
    pull(t),
  results = data$pi_past %>%
    t(),
  y_1r_past = df$polls_first_round_past %>%
    select(id, y, p) %>%
    pivot_wider(id_cols = id,
                names_from = p,
                values_from = y,
                values_fill = 0) %>%
    select(-id) %>%
    as.matrix() %>%
    t()
)


## Fit model
fit <- mod$sample(
  data = data_list,
  chains = 4,
  iter_sampling = 250,
  iter_warmup = 250,
  parallel_chains = 4,
  refresh = 250,
  init = 0.2
)


## Posterior Predictive Checks
# Plot pi_theta
ppc_plt_pi_theta_first_round(fit, df$polls_first_round, data$df)
ppc_plt_pi_theta_second_round(fit, df$polls_second_round, data$df_coll)
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








