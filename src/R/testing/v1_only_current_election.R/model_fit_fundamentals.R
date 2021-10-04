## Test fundamentals model
rm(list = ls())
## library
library(tidyverse)
library(cmdstanr)
## functions
source("src/R/functions/sim_fundamentals.R")
## Generate data
NElections <- 20
NBlocs <- 6
list_df <- sim_fundamentals(NElections = NElections,
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
  x_fundamentals = list_df$x_fundamentals
)
## Compile model
mod <- cmdstan_model("src/stan/v1_fundamentals.stan")
## Run model
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 400,
  iter_warmup = 400,
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

bayesplot::mcmc_trace(fit$draws("raw_alpha[1,2]"))

mcmc_nuts_treedepth(fit$cmdstan_diagnose())


alpha <- fit$draws("alpha") %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  filter(iter < 600) %>%
  pivot_longer(c(-iter),
               names_to = "var",
               values_to = "draws") %>%
  mutate(
    bloc_id = str_match(var, "([\\d]+),")[,2],
    election_id = str_match(var, ",([\\d]+)")[,2]
  ) %>%
  filter(!is.na(bloc_id)) %>%
  dplyr::select(-var)
alpha_join <- full_join(alpha %>%
            rename(draws_a = draws,
                   bloc_id_a = bloc_id),
          alpha %>%
            rename(draws_b = draws,
                   bloc_id_b = bloc_id)) %>%
  filter(election_id == 2,
         draws_a != 0,
         draws_b != 0,
         bloc_id_a != bloc_id_b)
ggplot(alpha_join, aes(x = draws_a, y = draws_b)) +
  geom_point(alpha = 0.3) +
  facet_grid(bloc_id_b ~ bloc_id_a)










