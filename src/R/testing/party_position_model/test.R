library(cmdstanr)
###############################################################################
## Party position mixture
y = data_list$YVoteshare


lagged_results <- df_election_results %>%
  select(-votes) %>%
  pivot_wider(id_cols = c(year, departement),
              names_from = bloc,
              values_from = percentage,
              values_fill = 0) %>%
  mutate(department_id = department_vector[departement]) %>%
  arrange(year, department_id) %>%
  filter(year > 1980, year < 2017) %>%
  select(-year, -departement, -department_id)
lagged_results <- lagged_results[, bloc_vector] %>%
  as.matrix()

datalist <- list(
  N = 600,
  NElections = 6,
  NDepartments = 100,
  id_Obs_elections = data_list$id_Obs_elections,
  NDepartments = data_list$NDepartments,
  P = 6,
  K = 1,
  x = matrix(data_list$XDepartment[,2], ncol = 1),
  y = y,
  id_Obs_departments = data_list$id_Obs_departments,

  NBlocs_Elections = data_list$NBlocs_Elections,
  included_blocs = data_list$included_blocs,
  NParticipated = data_list$NParticipated,
  participated = data_list$participated,
  y_lag = lagged_results
)
mod <- cmdstan_model("src/stan/testing/party_position_mixture.stan")

fit <- mod$sample(
  data = datalist,
  chains = 6,
  iter_sampling = 500,
  iter_warmup = 300,
  parallel_chains = 6,
  refresh = 100
)
ggplot(fit$summary("y_star"), aes(x = mean)) +
  geom_histogram()

bayesplot::mcmc_pairs(fit$draws(c("mu[1,1]",
              "mu[2,1]",
              "mu[3,1]",
              "mu[4,1]",
              "mu[5,1]",
              "mu[6,1]",
              "lp__")) %>%
                posterior::subset_draws(iteration = 1:200))


bayesplot::mcmc_pairs(fit$draws(c("mu[1,2]",
                                  "mu[2,2]",
                                  "mu[3,2]",
                                  "mu[4,2]",
                                  "mu[5,2]",
                                  "mu[6,2]",
                                  "lp__")) %>%
                        posterior::subset_draws(iteration = 1:200))


fit$summary("beta")
fit$summary("alpha")
fit$summary("mu") %>% View()
fit$summary("sigma")
fit$summary("lambda")
bayesplot::mcmc_pairs(fit$draws(c("lambda[1]",
                                "mu[1]",
                                "sigma[1]",
                                "beta",
                                "lp__")) %>%
                        posterior::subset_draws(iteration = 1:200))


bayesplot::mcmc_pairs(fit$draws(c("beta",
                                  "sigma")) %>%
                        posterior::subset_draws(iteration = 1:200))


bayesplot::mcmc_pairs(fit$draws(c("mu",
                                  "lp__")) %>%
                        posterior::subset_draws(iteration = 1:200))


bayesplot::mcmc_pairs(fit$draws(c("sigma",
                                  "lp__")) %>%
                        posterior::subset_draws(iteration = 1:200))

###############################################################################


y = data_list$YVoteshare


lagged_results <- df_election_results %>%
  group_by(year, bloc) %>%
  summarize(percentage = sum(percentage * votes)/sum(votes)) %>%
  group_by(bloc) %>%
  arrange(year) %>%
  mutate(lag_percentage = lag(percentage),
         lag_percentage = ifelse(is.na(lag_percentage), 0.05, lag_percentage)) %>%
  select(-percentage) %>%
  pivot_wider(id_cols = c(year),
              names_from = bloc,
              values_from = lag_percentage,
              values_fill = 0.05) %>%
  filter(year > 1982) %>%
  select(-year)
lagged_results <- lagged_results[, bloc_vector] %>%
  as.matrix()

datalist <- list(
  N = 600,
  NElections = 6,
  NDepartments = 100,
  id_Obs_elections = data_list$id_Obs_elections,
  id_Obs_department = data_list$id_Obs_departments,
  NDepartments = data_list$NDepartments,
  P = 6,
  K = 1,
  x = matrix(data_list$XDepartment[,2], ncol = 1),
  y = y,
  id_Obs_departments = data_list$id_Obs_departments,

  NBlocs_Elections = data_list$NBlocs_Elections,
  included_blocs = data_list$included_blocs,
  NParticipated = data_list$NParticipated,
  participated = data_list$participated,
  y_lag = lagged_results
)
mod <- cmdstan_model("src/stan/testing/party_position_spatial_v3.stan")

fit <- mod$sample(
  data = datalist,
  chains = 6,
  iter_sampling = 300,
  iter_warmup = 300,
  parallel_chains = 6,
  refresh = 100
)

bayesplot::mcmc_pairs(fit$draws(c("mu_nation",
                                  "lp__")) %>%
                        posterior::subset_draws(iteration = 1:200))


bayesplot::mcmc_pairs(fit$draws(c("mu_party",
                                  "lp__")) %>%
                        posterior::subset_draws(iteration = 1:200))



###############################################################################
P <- 5
mu <- sort(rnorm(P, 0, 3))
sigma <- abs(rnorm(P, 0, 3))

N <- 500
K <- 2
x <- matrix(rnorm(N * K, 0, 3), ncol = K, nrow = N)
beta <- matrix(rnorm(K, 0, 1), nrow = K, ncol = 1)
y_star <- x %*% beta
dens <- sapply(1:P, function(j){
  dnorm(y_star, mu[j], sigma[j])
}) %>% cbind(.)
prob <- sapply(1:N, function(j){
  dens[j,]/sum(dens[j,])
}) %>% t()
y <- sapply(1:N, function(j){
  sample(1:P, size = 1, prob = prob[j, ])
})

data_list <- list(
  N = N,
  P = P,
  K = K,
  x = x,
  y = y
)

mod <- cmdstan_model("src/stan/testing/party_position_spatial.stan")


y = data_list$YVoteshare
y_new = matrix(NA, nrow = 600, ncol = 5)
y_new[, 1] = y[, 1]
y_new[, 2] = y[, 2]
y_new[, 3] = y[, 3] + y[, 4]
y_new[, 4] = y[, 5]
y_new[, 5] = y[, 6]
datalist <- list(
  N = 600,
  NDepartments = data_list$NDepartments,
  P = 5,
  K = 2,
  x = data_list$XDepartment[,2:3],
  y = y_new,
  id_Obs_departments = data_list$id_Obs_departments
)
mod <- cmdstan_model("src/stan/testing/party_position_spatial.stan")


fit <- mod$sample(
  data = datalist,
  chains = 6,
  iter_sampling = 500,
  iter_warmup = 500,
  parallel_chains = 6,
  refresh = 100,
  init = 0.2
)

bayesplot::mcmc_pairs(fit$draws("raw_beta"))


beta <- fit$draws("beta") %>%
  posterior::as_draws_df() %>%
  select(contains("beta")) %>%
  as.matrix()

hist(apply(as.matrix(datalist$x) %*% t(beta), 1, mean))

mu <- fit$draws("mu") %>%
  posterior::as_draws_df() %>%
  select(contains("mu")) %>%
  as.matrix()

sigma <- fit$draws("sigma") %>%
  posterior::as_draws_df() %>%
  select(contains("sigma")) %>%
  as.matrix()

out <- lapply(sample(1:1800, 1800), function(jj){
  out0 <- lapply(1:5, function(ii){
    out1 <- data.frame(p = ii,
                       x = rnorm(300, mu[jj,ii], sigma[jj, ii]))
    return(out1)
  }) %>%
    do.call("bind_rows", .)
  return(out0)
}) %>%
  do.call("bind_rows", .)
ggplot(out, aes(x = x, fill = as.factor(p))) +
  geom_histogram(position = "identity", alpha = 0.3, bins = 50) +
  lims(x = c(-3, 3))



###############################################################################
## Simulate a distribution of the department position by passing stdnormal
## draws to Stan
mod <- cmdstan_model("src/stan/testing/party_position_vary_departments.stan")


y = data_list$YVoteshare
y_new = matrix(NA, nrow = 600, ncol = 5)
y_new[, 1] = y[, 1]
y_new[, 2] = y[, 2]
y_new[, 3] = y[, 3] + y[, 4]
y_new[, 4] = y[, 5]
y_new[, 5] = y[, 6]
datalist <- list(
  N = 600,
  NDepartments = data_list$NDepartments,
  P = 5,
  K = 2,
  x = data_list$XDepartment[,2:3],
  y = y_new,
  id_Obs_departments = data_list$id_Obs_departments,
  Nstd = 2,
  xstd = rep(0, 2)
)

fit <- mod$sample(
  data = datalist,
  chains = 6,
  iter_sampling = 500,
  iter_warmup = 500,
  parallel_chains = 6,
  refresh = 100,
  init = 0.2
)

## Party position two based on Merrill 1994
library(cmdstanr)
mod <- cmdstan_model("src/stan/testing/party_position_spatial_v2.stan")

datalist <- list(
  N = 300,
  NDepartments = data_list$NDepartments,
  NElections = 3,
  P = 5,
  K = 1,
  D = 2,
  x = matrix(data_list$XDepartment[1:300,2], ncol = 1),
  y = y_new[1:300,],
  id_Obs_departments = data_list$id_Obs_departments[1:300],
  id_Obs_elections = data_list$id_Obs_elections[1:300]
)

fit <- mod$sample(
  data = datalist,
  chains = 6,
  iter_sampling = 500,
  iter_warmup = 500,
  parallel_chains = 6,
  refresh = 100,
  init = 0.2
)

ggplot(fit$summary("y_star") %>%
         add_column(election = datalist$id_Obs_elections),
       aes(x = mean, fill = as.factor(election))) +
  geom_histogram(position = "identity", alpha = 0.6)

fit$summary("party_position",
            ~ quantile(., c(0.1,0.25, 0.5, 0.75, 0.9))) %>%
  mutate(e = as.integer(str_match(variable, "\\[([\\d]+)")[,2]),
         p = as.integer(str_match(variable, "([\\d]+)\\]")[,2])) %>%
  ggplot(aes(x = e, y = `50%`, color = as.factor(p))) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = `25%`, ymax = `75%`),
                  position = position_dodge(width = 0.3),
                  width = 0, size = 0.5) +
    theme_light()












