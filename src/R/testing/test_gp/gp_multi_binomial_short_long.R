################################################################################
## Title: Testing Gaussian processes
################################################################################
## libraries
library(tidyverse)
library(cmdstanr)
################################################################################
## simulate data function
mod <- cmdstan_model("src/stan/testing/gp/gp_normal_multi_short_long_sim.stan")
data_list <- list(N = 1000,
                  D = 3,
                  T = 300,
                  alpha = c(0.1, 0.2, 0.5),
                  rho_short = 10,
                  rho_long = 101,
                  sigma = 0.3,
                 eta = 0.4)
set <- sample(1:data_list$N,size = 50, replace = F)
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
plt_df <- lapply(1:data_list$D, function(ii){
  plt_df <- data.frame(x = c(draws[1,grepl("x",colnames(draws))]),
                       f = c(draws[1,
                                   grepl("f",colnames(draws)) &
                                   grepl(paste(ii, "\\]", sep = ""),colnames(draws))]),
                       y = c(draws[1,
                                   grepl("y",colnames(draws)) &
                                   grepl(paste(ii, "\\]", sep = ""),colnames(draws))]),
                       n = c(draws[1,grepl("n",colnames(draws))])) %>%
    mutate(d  = ii,
           i = 1:n())
}) %>%
  do.call("bind_rows", .)
ggplot(data = plt_df %>% filter(i %in% set), aes(x=x, y=y/n)) +
  geom_point(aes(colour = as.factor(d))) +
  geom_line(data = plt_df, aes(x = x, y = boot::inv.logit(f),
                               colour = as.factor(d))) +
  theme_bw() + theme(legend.position="bottom") +
  xlab('X') +
  ylab('y')
## Load model
mod_gp <- cmdstan_model("src/stan/testing/gp/gp_normal_multi_short_long.stan")
## Datalist
plt_df_set <- plt_df %>%
  filter(i %in% set) %>%
  select(i, x, n, y, d) %>%
  pivot_wider(id_cols = c(i, x, n),
              values_from = y,
              names_from = d)
stan_data <- list(N1 = length(set),
                  D = data_list$D,
                  x1 = plt_df_set %>% pull(x),
                  y = plt_df_set[,c("1", "2", "3")] %>% as.matrix(),
                  n = plt_df_set %>% pull(n),
                  N2 = data_list$T/5,
                  x2 = seq(1, data_list$T, 5))
## Fit
fit <- mod_gp$sample(
  data = stan_data,
  chains = 6,
  iter_sampling = 100,
  iter_warmup = 500,
  parallel_chains = 6,
  refresh = 25,
  init = 0
)
## Jumble
post_pred <- lapply(1:data_list$D, function(ii){
  data.frame(x = stan_data$x2,
             pred_mu = fit$summary("y2") %>%
               filter(grepl(paste(ii, "\\]", sep = ""), variable)) %>%
               pull(mean)) %>%
    mutate(d = ii) %>%
    return(.)
}) %>%
  do.call("bind_rows", .)
plt_df_rt_melt = fit$draws("y2") %>%
  posterior::as_draws_df() %>%
  select(!contains(".")) %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = c("n", "d"),
               values_to = "draws",
               names_pattern = "([\\d]+),([\\d]+)") %>%
  mutate(n = as.integer(n)) %>%
  left_join(data.frame(
    n = 1:length(stan_data$x2),
    x2 = stan_data$x2
  ))

p <- ggplot(data = plt_df %>% filter(i %in% set), aes(x=x, y=y/n)) +
  geom_line(data = plt_df_rt_melt %>%
              filter(iter %in% sample(1:1800, 100)), aes(x = x2, y = draws,
            group = iter,
            colour = 'Posterior mean functions'), alpha = 0.3) +
  geom_point(aes(colour = 'Realized data')) +
  geom_line(data = plt_df, aes(x = x, y = boot::inv.logit(f), colour = 'Latent mean function')) +
  geom_line(data = post_pred, aes(x = x, y = pred_mu, colour = 'Posterior mean function')) +
  theme_bw() + theme(legend.position="bottom") +
  scale_color_manual(name = '',
    values = c('Realized data'='black',
    'Latent mean function'='red',
    "Posterior mean functions" = "blue",
    "Posterior mean function" = "green")) +
  xlab('X') +
  ylab('y') +
  facet_wrap(d ~ .)
p


