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
T <- 10
T_prior <- 10
N_first_round <- 15
N_second_round <- 25
N_first_round_past <- 40
N_past_election <- 3
P_both <- 4
P_past <- 2
P_new <- 2
data <- sim_random_walk(N_past_election = N_past_election,
                        P_both = P_both,
                        P_past = P_past,
                        P_new = P_new,
                        T = T,
                        T_prior = T_prior,
                        rho = 0.1,
                        sigma = 0.1,
                        K_VAR = 1)
ggplot(data$df, aes(x = t, y = share, color = p)) +
  geom_line()
ggplot(data$df_coll, aes(x = t, y = share, color = p)) +
  geom_line()
df <- sim_polling_data(N_first_round = N_first_round,
                       N_second_round = N_second_round,
                       N_first_round_past = N_first_round_past,
                       N_R = 3,
                       sigma_alpha = 0.2,
                       sigma_tau = 0.2,
                       sigma_xi = 0.2,
                       data$eta_matrix,
                       transition_matrix = data$transition_matrix,
                       pi_past = data$pi_past)
df$polls_first_round_past %>%
  left_join(data$pi_past_dataframe) %>%
  ggplot(aes(x = share - y/n)) +
    geom_histogram()
ggplot(df$polls_first_round, aes(x = t, y = y/n, color = as.factor(p))) +
  geom_point()
ggplot(df$polls_second_round, aes(x = t, y = y/n)) +
  geom_point()




inclusion_data <- create_variable_inclusion_input(df$polls_first_round)


## Prepare data
data_list <- list(
  N_first_round = df$polls_first_round %>%
    distinct(id) %>%
    nrow(),
  N_second_round = df$polls_second_round %>%
    distinct(id) %>%
    nrow(),
  P = P_new + P_both,
  P_past_present = P_new + P_both + P_past,
  R = 3,
  T = T,
  T_prior = T_prior,
  theta_prior = data$eta_start,
  t_first_round = df$polls_first_round %>%
    distinct(id, t) %>%
    pull(t),
  t_second_round = df$polls_second_round %>%
    distinct(id, t) %>%
    pull(t),
  r_first_round = df$polls_first_round %>%
    distinct(id, r) %>%
    pull(r),
  r_second_round = df$polls_second_round %>%
    distinct(id, r) %>%
    pull(r),
  ## -- variable inclusion dynamic
  P_first_round = inclusion_data$P_first_round,
  N_combinations = inclusion_data$N_combinations,
  P_N_combinations = inclusion_data$P_N_combinations,
  p_first_round_included = inclusion_data$p_first_round_included,
  p_first_round_excluded = inclusion_data$p_first_round_excluded,
  p_id = inclusion_data$p_id,

  ##
  y_first_round = inclusion_data$y_first_round %>% t(),
  y_second_round = df$polls_second_round %>%
    pull(y),
  n_second_round = df$polls_second_round %>%
    pull(n),
  N_elections_past = df$N_elections_past,
  N_first_round_past = df$polls_first_round_past %>%
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
  y_first_round_past = df$polls_first_round_past %>%
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
  seed = 1234,
  chains = 4,
  iter_sampling = 250,
  iter_warmup = 250,
  parallel_chains = 4,
  refresh = 250,
  init = 0.2
)
## Plot pi_theta
ppc_plt_pi_theta_first_round(fit, df$polls_first_round, data$df)
ppc_plt_pi_theta_second_round(fit, df$polls_second_round, data$df_coll)
## Plot sigma_tau
ppc_plt_sigma_tau(fit, 0.2)
## Plot sigma_alpha
ppc_plt_sigma_alpha(fit, 0.2)
## Plot alpha
ppc_plt_alpha(fit, true_alpha = df$alpha)
## Plot xi
ppc_plt_xi(fit, true_xi = df$xi)
## cov_theta
ppc_plt_cov_theta(fit, transition_matrix = data$transition_matrix)
## Sum to zero constraint
ppc_plt_alpha_sum_to_0(fit)









