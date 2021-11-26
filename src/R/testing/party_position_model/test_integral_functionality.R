library(cmdstanr)
mod <- cmdstan_model("src/stan/testing/party_position_integral_model.stan")

data_list <- list(
  D = 4,
  positions = c(-2, 1, 3, 5)
)


fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 300,
  iter_warmup = 300,
  parallel_chains = 6,
  refresh = 100
)