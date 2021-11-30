################################################################################
## Title: Testing Gaussian processes
################################################################################
## libraries
library(tidyverse)
library(cmdstanr)
################################################################################
## simulate data function
mod <- cmdstan_model("src/stan/testing/gp/gp_sim.stan")
data_list <- list(N = 2000,
                 alpha = 1,
                 length_scale = 0.15,
                 sigma = sqrt(0.1))
set <- sample(1:data_list$N,size = 30, replace = F)
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 1,
  iter_warmup = 1,
  parallel_chains = 6,
  refresh = 25,
  init = 0,
  fixed_param = TRUE
)
draws <- fit$draws() %>%
  posterior::as_draws_matrix()
plt_df <- data.frame(x = c(draws[1,grepl("x",colnames(draws))]),
                     y = c(draws[1,grepl("y",colnames(draws))]),
                     f = c(draws[1,grepl("f",colnames(draws))]))
ggplot(data = plt_df[set,], aes(x=x, y=y)) +
  geom_point(aes(colour = 'Realized data')) +
  geom_line(data = plt_df, aes(x = x, y = f, colour = 'Latent mean function')) +
  theme_bw() + theme(legend.position="bottom") +
  scale_color_manual(name = '', values = c('Realized data'='black',
                                           'Latent mean function'= "red")) +
  xlab('X') +
  ylab('y')
## Load model
mod <- cmdstan_model("src/stan/testing/gp/gp.stan")
## Datalist
N <- sample(1:nrow(df), 1)
N_pred <- nrow(df) - N
x <- sample(1:nrow(df), N)
x_pred <- (1:nrow(df))[!(1:nrow(df)) %in% x]
y <- df$y[x]
data_list <- list(
  N = N,
  N_pred = N_pred,
  x = x,
  x_pred = x_pred,
  y = y
)
## Fit
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 300,
  iter_warmup = 300,
  parallel_chains = 6,
  refresh = 25,
  init = 0
)
## Jumble
f_pred <- fit$draws("f_pred") %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = "id",
               values_to = "val",
               names_pattern = "([\\d]+)") %>%
  mutate(id = as.integer(id),
         x = x_pred[id]) %>%
  filter(!is.na(id))
eta <- fit$draws("f_pred") %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = "id",
               values_to = "val",
               names_pattern = "([\\d]+)") %>%
  mutate(id = as.integer(id),
         x = x[id]) %>%
  filter(!is.na(id))
bind_rows(f_pred,
          eta) %>%
  filter(
    iter %in% sample(1:max(iter), 100)
  ) %>%
  ggplot(aes(x = x, y = val, group = iter)) +
    geom_line(color = "blue", alpha = 0.3)






