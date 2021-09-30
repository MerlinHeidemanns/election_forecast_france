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
  iter_sampling = 100,
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





