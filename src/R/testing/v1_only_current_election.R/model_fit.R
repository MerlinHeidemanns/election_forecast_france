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
## Load model
mod <- cmdstan_model("src/stan/v1_current_election.stan")
## Generate data
T <- 50
N <- 200
data <- sim_random_walk(4, T, 0, 0.1)
df <- sim_polling_data(N, 3,
                       sigma_alpha = 0.2,
                       sigma_tau = 0.2,
                       sigma_xi = 0, data$eta_matrix)
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
  geom_line(data = data$df, aes(x = t, y = share), linetype = 2, color = "red") +
  facet_wrap(p ~ .)
## Plot sigma_tau
ppc_plt_sigma_tau(fit, 0.2)
## Plot sigma_alpha
ppc_plt_sigma_alpha(fit, 0.2)
## Plot alpha
ppc_plt_alpha(fit, true_alpha = df$alpha)




