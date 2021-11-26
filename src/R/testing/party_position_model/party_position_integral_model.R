library(cmdstanr)
mod <- cmdstan_model("src/stan/testing/party_position_integral_model.stan")

data_list <- list(
  N = 2,
  D = 6,
  y = datalist$y[1:2, ]
)


fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 300,
  iter_warmup = 300,
  parallel_chains = 6,
  refresh = 10,
  init = 0.2
)