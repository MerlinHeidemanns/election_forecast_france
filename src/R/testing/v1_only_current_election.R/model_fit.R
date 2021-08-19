## Clean
rm(list = ls())
## Libraries
library(tidyverse)
library(cmdstanr)
## Source
source("src/R/functions/exp_softmax.R")
source("src/R/functions/sim_random_walk.R")
source("src/R/functions/sim_polling_data.R")
## Load model
mod <- cmdstan_model("src/stan/v1_current_election.stan")
## Generate data
T <- 20
data <- sim_random_walk(4, T, 0, 0.1)
df <- sim_polling_data(T, 3, 0.2, 0.2, 0.2, data$eta_matrix)
ggplot(df, aes(x = t, y = y/n, color = as.factor(p))) +
  geom_point()
## Prepare data
data_list <- list(
  N = df %>% distinct(id) %>% nrow(),
  P = 4,
  R = 3,
  T = T,
  t = df %>% distinct(id, t) %>% pull(t),
  r = df %>% distinct(id, r) %>% pull(r),
  y = df %>%
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
pi_theta <- fit$draws("pi_theta") %>%
  posterior::as_draws_df() %>%
  pivot_longer(
    everything(),
    names_to = "pt",
    values_to = "draws"
  ) %>%
  mutate(
    p = str_match(pt, "(\\d+),")[, 2],
    t = as.integer(str_match(pt, ",(\\d+)")[, 2])
  ) %>%
  filter(!is.na(t)) %>%
  dplyr::select(-pt) %>%
  group_by(p, t) %>%
  summarize(
    q50 = quantile(draws, 0.5),
    q25 = quantile(draws, 0.25),
    q75 = quantile(draws, 0.75),
    q10 = quantile(draws, 0.10),
    q90 = quantile(draws, 0.90)
  )
ggplot(pi_theta, aes(x = t, y = q50)) +
  geom_line() +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
  theme_light() +
  geom_point(data = df, aes(x = t, y = y/n)) +
  facet_wrap(p ~ .)





