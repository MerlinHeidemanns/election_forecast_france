################################################################################
## Title: Testing Gaussian processes
################################################################################
## libraries
library(tidyverse)
library(cmdstanr)
################################################################################
## simulate data function
mod <- cmdstan_model("src/stan/testing/gp/gp_binomial_hierarchical_sim.stan")
data_list <- list(
                  R = 4,
                  N = 1000,
                  T = 30,
                  G = 3,
                  alpha = 0.5,
                  mu_length_scaleG = 10,
                  sigma_length_scaleG = 1,
                  mu_length_scaleR = 10,
                  sigma_length_scaleR = 5)
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
set <- sample(1:data_list$N,size = 100, replace = F)

plt_df <- lapply(1:data_list$G, function(ii){
  lapply(1:data_list$R, function(jj){
    data.frame(x = c(draws[1,grepl("x",colnames(draws))]),
               y = c(draws[1,grepl("y",colnames(draws))]),
               n = c(draws[1,grepl("n",colnames(draws)) & !grepl("scale",colnames(draws))]),
               g = c(draws[1,grepl("g",colnames(draws)) & !grepl("scale",colnames(draws))]),
               r = c(draws[1,grepl("r",colnames(draws)) & !grepl("scale",colnames(draws))])
    ) %>%
      mutate(f = c(draws[1,grepl("f",colnames(draws)) &
                           grepl(paste("\\[", ii, sep = ""),colnames(draws)) &
                           grepl(paste(",", jj,",", sep = ""),colnames(draws))])[x]) %>%
      filter(g == ii) %>%
      filter(r == jj) %>%
      return(.)
  }) %>%
    do.call("bind_rows", .) %>%
    return(.)
}) %>%
  do.call("bind_rows", .)
ggplot(data = plt_df[set,], aes(x=x, y=y/n, color = as.factor(g))) +
  geom_point() +
  geom_line(data = plt_df, aes(x = x, y = boot::inv.logit(f),
                               color = as.factor(g),
                               linetype = 'Latent mean function')) +
  theme_bw() + theme(legend.position="bottom") +
  xlab('X') +
  ylab('y') +
  facet_wrap(r ~.)
## Load model
mod_gp <- cmdstan_model("src/stan/testing/gp/gp_binomial_hierarchical.stan")
## Datalist
stan_data <- list(N = length(set),
                  zeros = rep(0,length(set)),
                  R = data_list$R,
                  G = data_list$G,
                  T = data_list$T,
                  g = plt_df[set, "g"],
                  r = plt_df[set, "r"],
                  x = plt_df[set,"x"],
                  y = plt_df[set,"y"],
                  n = plt_df[set,"n"])
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
post_pred <- data.frame(x = stan_data$x_pred,
                        pred_mu = fit$summary("y_pred") %>% pull(mean))
plt_df_rt_melt = fit$draws("y_pred") %>%
  posterior::as_draws_df() %>%
  select(!contains(".")) %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = "f",
               values_to = "draws",
               names_pattern = "([\\d]+)") %>%
  mutate(f = as.integer(f)) %>%
  left_join(data.frame(
    f = 1:length(stan_data$x_pred),
    x = stan_data$x_pred
  ))

p <- ggplot(data = plt_df[set,], aes(x=x, y=y/n)) +
  geom_line(data = plt_df_rt_melt %>%
              filter(iter %in% sample(1:1800, 300)), aes(x = x, y = draws,
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
  ylab('y')
p


