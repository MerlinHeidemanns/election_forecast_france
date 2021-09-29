## Test fundamentals model
rm(list = ls())
## library
library(tidyverse)
library(cmdstanr)
## functions
source("src/R/functions/sim_fundamentals.R")
## Generate data
NElections <- 10
NBlocs <- 6
list_df <- sim_fundamentals(NElections = NElections,
                            NBlocs = NBlocs)
## Prepare data frame
data_list <- list(
  NElections_past_fundamentals = list_df$NElections,
  NBlocs = list_df$NBlocs,
  y_fundamentals = list_df$y
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
## Posterior Predictive Checks
prob_alpha <- fit$summary("prob_alpha", ~ quantile(., c(0.10, 0.25, 0.5, 0.75, 0.9)))
colnames(prob_alpha) <- c("variable", "q10", "q25", "q50", "q75", "q90")
prob_alpha <- prob_alpha %>%
  mutate(
    election_id = as.integer(str_match(variable, ",([\\d]+)")[,2]),
    bloc_id = as.integer(str_match(variable, "([\\d]+),")[,2])
  ) %>%
  dplyr::select(-variable)

ggplot(prob_alpha, aes(x = election_id, y = bloc_id))
  geom_line() +
  geom_ribbon(aes(ymin = q25, ymax = q75), color = "blue", alpha = 0.5) +
  geom_ribbon(aes(ymin = q25, ymax = q75), color = "blue", alpha = 0.5) +
  theme_light() +
  labs(x = "Election", y = "Share") +
  geom_line(data = list_df$y_df, aes(x = election_id, y = share),
            linetype = 2, color = "red")

