## Clean
rm(list = ls())
## Libraries
library(tidyverse)
library(cmdstanr)
## Source
source("src/R/functions/exp_softmax.R")
source("src/R/functions/sim_random_walk.R")
source("src/R/functions/sim_polling_data.R")
source("src/R/functions/ppc_pi_theta.R")
source("src/R/functions/ppc_plt_sigma_tau.R")
source("src/R/functions/ppc_plt_sigma_alpha.R")
source("src/R/functions/ppc_plt_alpha.R")
source("src/R/functions/ppc_plt_xi.R")
## Load model
mod <- cmdstan_model("src/stan/v1_current_election.stan")
## Generate data
T <- 50
N <- 50
data <- sim_random_walk(4, T, 0, 0.1)
df <- sim_polling_data(N, 3,
                       sigma_alpha = 0.2,
                       sigma_tau = 0.2,
                       sigma_xi = 0.2, data$eta_matrix)
ggplot(df$polls, aes(x = t, y = y/n, color = as.factor(p))) +
  geom_point()
## Prepare data
data_list <- list(
  N = df$polls %>% distinct(id) %>% nrow(),
  P = 4,
  R = 3,
  T = T,
  t = df$polls %>% distinct(id, t) %>% pull(t),
  r = df$polls %>% distinct(id, r) %>% pull(r),
  y = df$polls %>%
    dplyr::select(y, p, id) %>%
    pivot_wider(id_cols = id,
                names_from = p,
                values_from = y) %>%
    dplyr::select(-id) %>%
    as.matrix() %>%
    t()
)
## Fit model
fit <- mod$sample(
  data = data_list,
  seed = 1234,
  chains = 4,
  iter_sampling = 300,
  iter_warmup = 300,
  parallel_chains = 4,
  refresh = 250
)
## Plot pi_theta
pi_theta <- ppc_pi_theta(fit)
ggplot(pi_theta, aes(x = t, y = q50)) +
  geom_line() +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
  theme_light() +
  geom_point(data = df$polls, aes(x = t, y = y/n)) +
  geom_line(data = data$df, aes(x = t, y = share), linetype = 2,
            color = "red") +
  facet_wrap(p ~ .)
## Plot sigma_tau
ppc_plt_sigma_tau(fit, 0.2)
## Plot sigma_alpha
ppc_plt_sigma_alpha(fit, 0.2)
## Plot alpha
ppc_plt_alpha(fit, true_alpha = df$alpha)
## Plot xi
ppc_plt_xi(fit, true_xi = df$xi)
##
cov_theta <- fit$draws("cov_theta") %>%
  posterior::as_draws_df() %>%
  pivot_longer(
    everything(),
    names_to = "pp",
    values_to = "draws"
  ) %>%
  mutate(
    p1 = as.integer(str_match(pp, "(\\d+),")[, 2]),
    p2 = as.integer(str_match(pp, ",(\\d+)")[, 2])
  ) %>%
  filter(!is.na(p1)) %>%
  group_by(p1, p2) %>%
  summarise(
    q50 = quantile(draws, 0.5),
    q25 = quantile(draws, 0.25),
    q75 = quantile(draws, 0.75),
    q10 = quantile(draws, 0.10),
    q90 = quantile(draws, 0.90)
  )

cov_theta_true <- data$transition_matrix %>%
  as.data.frame() %>%
  mutate(p1 = 1:n()) %>%
  pivot_longer(
    c(-p1),
    names_to = "p2",
    values_to = "cov",
    names_prefix = "V"
  ) %>%
  mutate(p2 = as.integer(p2))


cov_theta %>%
  left_join(cov_theta_true) %>%
  ggplot(aes(x = interaction(p1, p2))) +
    geom_point(aes(y = q50)) +
    geom_point(aes(y = cov), color = "red", shape = 3) +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.5, width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
    theme_light()
