## Test fundamentals model
rm(list = ls())
## library
library(tidyverse)
library(cmdstanr)
## functions
source("src/R/functions/sim_fundamentals_ordered.R")
## Generate data
NElections <- 20
NBlocs <- 6
list_df <- sim_fundamentals_ordered(NElections = NElections,
                            NBlocs = NBlocs)
## Plot data
list_df$y_plt

## Prepare data frame
data_list <- list(
  NElections_past_fundamentals = list_df$NElections,
  NBlocs = list_df$NBlocs,
  y_fundamentals = list_df$y,
  incumbency = list_df$incumbency,
  K = dim(list_df$x_fundamentals)[2],
  x_fundamentals = list_df$x_fundamentals,
  incumbency_distance = list_df$incumbency_distance,
  incumbency_bloc = c(list_df$incumbency %*% seq(1, 6)) - 1
)
## Compile model
mod <- cmdstan_model("src/stan/v1_fundamentals_ordered.stan")
## Run model
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 800,
  iter_warmup = 800,
  parallel_chains = 6,
  refresh = 100
)
## --- Posterior Predictive Checks
## prob_alpha
source("src/R/functions/ppc_fundamentals_prob_alpha.R")
ppc_fundamentals_prob_alpha(fit, list_df$y_df)
## sigma_alpha
source("src/R/functions/ppc_fundamentals_sigma_alpha.R")
ppc_fundamentals_sigma_alpha(fit, list_df$sigma_alpha)
## errors
source("src/R/functions/ppc_fundamentals_epsilon.R")
ppc_fundamentals_epsilon(fit)
## beta
source("src/R/functions/ppc_fundamentals_beta_incumbency.R")
ppc_fundamentals_beta_incumbency(fit, true_beta = list_df$beta_incumbency)
## beta fundamentals
source("src/R/functions/ppc_fundamentals_beta.R")
ppc_fundamentals_beta(fit, list_df$beta_fundamentals)

true_beta <- list_df$beta_fundamentals
true_beta_names <- true_beta %>% distinct(name) %>% pull(name)
beta_fundamentals <- fit$summary("beta_fundamentals", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9)))
colnames(beta_fundamentals) <- c("variable", "q10", "q25", "q50", "q75", "q90")
beta_fundamentals <- beta_fundamentals %>%
  mutate(
    name = true_beta_names[as.integer(str_match(variable, "([\\d]+),")[,2])],
    bloc_id = as.integer(str_match(variable, ",([\\d]+)")[,2])
  ) %>%
  dplyr::select(-variable) %>%
  left_join(true_beta)


ggplot(beta_fundamentals, aes(x = interaction(bloc_id, name))) +
  geom_point(aes(y = q50)) +
  geom_point(aes(y = value), color = "red", shape = 2) +
  geom_errorbar(aes(ymin =q25, ymax = q75), size = 0.5, width = 0) +
  theme_light()



beta_fundamentals_popularity <- fit$draws("beta_fundamentals") %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  filter(iter < 300) %>%
  pivot_longer(c(-iter),
               names_to = "variable",
               values_to = "draws") %>%
  mutate(
    name = true_beta_names[as.integer(str_match(variable, "([\\d]+),")[,2])],
    bloc_id = as.integer(str_match(variable, ",([\\d]+)")[,2])
  ) %>%
  filter(name == "popularity") %>%
  dplyr::select(-variable, -name)

full_join(beta_fundamentals_popularity %>%
            rename(draws_a = draws),
          beta_fundamentals_popularity %>%
            rename(draws_b = draws), by = "iter") %>%
  filter(bloc_id.x != bloc_id.y) %>%
  ggplot(aes(x = draws_a, y = draws_b)) +
    geom_point(alpha = 0.3) +
    facet_grid(bloc_id.x ~ bloc_id.y)













