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
## Generate data
#' Parameters
#' Fake true data
#' Observed polls
T <- 30
T_prior <- 10
N_first_round_surveys <- 20
N_second_round <- 20
N_first_round_past <- 60
N_past_election <- 4
P <- 6
N_R <- 3
N_combinations <- 5
data <- sim_random_walk(N_past_election = N_past_election,
                        P = P,
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
                       sigma_alpha = 0,
                       sigma_tau = 0,
                       sigma_xi = 0,
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
  P = P,
  R = N_R,
  T_unit = nrow(t_unit_df),
  t_unit_skip = t_unit_skip,
  t_unit_1r = df$polls_first_round %>%
    distinct(id, t_unit) %>%
    pull(t_unit),
  t_unit_2r = df$polls_second_round %>%
    distinct(id, t_unit) %>%
    pull(t_unit),
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
  P_past = df$P_past_elections %>%
    array(),
  R_past = df$polls_first_round_past %>%
    distinct(r_id) %>%
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

## Checks
data_list_check_y_1r(data_list)



## Load model
mod <- cmdstan_model("src/stan/v1_current_election.stan")
#mod <- cmdstan_model("src/stan/v2_current_election.stan")
## Fit model
fit <- mod$sample(
  data = data_list,
  chains = 4,
  iter_sampling = 400,
  iter_warmup = 500,
  parallel_chains = 4,
  refresh = 250,
  init = 0.2
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









