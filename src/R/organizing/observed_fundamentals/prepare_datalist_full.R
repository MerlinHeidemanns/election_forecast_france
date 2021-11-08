###############################################################################
## Title: Prepare input to fundamentals model
###############################################################################
## Clean environment
rm(list = ls())
###############################################################################
## Load libraries
library(tidyverse)
library(cmdstanr)
###############################################################################
## Load data
df_input_department <-
  read_csv("dta/fundamentals_dta/cleaned_input/input_data.csv")
df_results_department <-
  read_csv(file = "dta/fundamentals_dta/cleaned_input/input_results.csv")
df_approval <- read_csv(file = "dta/fundamentals_dta/cleaned_input/input_national.csv")
###############################################################################
## Data list
NElections <- df_input_department %>%
  distinct(election_id) %>%
  nrow()
NDepartements <- df_input_department %>%
  distinct(department_id) %>%
  nrow()
NObs <- df_results_department %>%
  nrow()
## Input national
#### Approval
NPolls <- df_approval %>%
  nrow()
NPollsters <- df_approval %>%
  distinct(pollster_id) %>%
  nrow()
NPresidents <- df_approval %>%
  distinct(president_id) %>%
  nrow()
NPollsters_Presidents <- df_approval %>%
  distinct(president_id, pollster_president_id) %>%
  group_by(president_id) %>%
  summarize(N = n()) %>%
  pull(N)
NPolls_Presidents <- df_approval %>%
  group_by(president_id) %>%
  summarize(N = n()) %>%
  pull(N)
NTime <- max(df_approval$time_id)
id_Polls_time <- df_approval %>%
  mutate(t = time_id + (president_id - 1) * NTime) %>%
  pull(t)

id_Polls_president <- df_approval %>%
  pull(president_id)
id_Polls_pollster <- df_approval %>%
  pull(pollster_id)
id_Polls_pollster_president <- df_approval %>%
  pull(pollster_president_id)
y_approval <- df_approval %>%
  mutate(y = floor(p_approve * N)) %>%
  pull(y)
n_approval <- df_approval %>%
  pull(N)
## Input department
XDepartements <- df_input_department %>%
  select(q2)
M <- ncol(XDepartements)
XDepartements[is.na(XDepartements)] <- -99
NMiss_X <- apply(XDepartements == -99, 2, sum)
id_X_miss <- matrix(-99, ncol = M, nrow = max(NMiss_X))
for (j in 1:M){
  tmp <- seq(1:NObs)[XDepartements[,j] == -99]
  id_X_miss[1:length(tmp), j] <- tmp
}
## Index
id_Obs_departements <- df_input_department %>%
  pull(department_id)
id_Obs_elections <- df_input_department %>%
  pull(election_id)
## Results
y <- df_results_department %>%
  pull(p_abstentions)
lag_y <- df_results_department %>%
  pull(lag_p_abstentions)
###############################################################################
## Datalist
data_list <- list(
  NElections = NElections,
  NDepartements = NDepartements,
  NObs = NObs,

  NPolls = NPolls,
  NPollsters = NPollsters,
  NPresidents = NPresidents,
  NPollsters_Presidents = NPollsters_Presidents,
  NPolls_Presidents = NPolls_Presidents,
  NTime = NTime,
  id_Polls_time = id_Polls_time,
  id_Polls_pollster = id_Polls_pollster,
  id_Polls_president = id_Polls_president,
  id_Polls_pollster_president = id_Polls_pollster_president,
  y_approval = y_approval,
  n_approval = n_approval,
  beta_prior = 0.1,
  M = M,
  XDepartements = XDepartements,
  NMiss_X = NMiss_X %>% array(),
  id_X_miss = id_X_miss,
  M = M,
  XDepartements = XDepartements %>% as.matrix(),
  NMiss_X = NMiss_X,
  id_X_miss = id_X_miss,
  id_Obs_departements = id_Obs_departements,
  id_Obs_elections = id_Obs_elections,
  y_result = y,
  lag_y_result = lag_y
)
###############################################################################
## Model
mod <- cmdstan_model("src/stan/models_fundamentals/v1_fundamentals.stan")
###############################################################################
## Fit
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 500,
  iter_warmup = 500,
  parallel_chains = 6,
  refresh = 250,
  init = 0.25
)
###############################################################################
theta <- fit$draws("theta") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "var",
               values_to = "draws") %>%
  mutate(
    time_id = as.integer(str_match(var, ",([\\d]+)")[,2]),
    election_id = as.integer(str_match(var, "([\\d]+),")[,2]),
    draws = boot::inv.logit(draws)
  ) %>%
  filter(!is.na(time_id)) %>%
  left_join(
    df_approval %>%
      distinct(election_year, president) %>%
      arrange(election_year) %>%
      mutate(election_id = 1:n())
  ) %>%
  group_by(time_id, election_year, president, election_id) %>%
  summarize(
    q50 = quantile(draws, 0.5),
    q25 = quantile(draws, 0.25),
    q75 = quantile(draws, 0.75),
    q10 = quantile(draws, 0.10),
    q90 = quantile(draws, 0.90)
  )
ggplot(theta, aes(x = time_id, y = q50)) +
  geom_line(color = "green") +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "green", alpha = 0.3) +
  geom_ribbon(aes(ymin = q10, ymax = q90), fill = "green", alpha = 0.15) +
  geom_point(data = df_approval, aes(x = time_id, y = floor(p_approve * N)/N)) +
  theme_light() +
  facet_wrap(president ~ .)

mu_pollster_president <-
  fit$summary("mu_pollster_president", ~ quantile(., c(.10, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(pollster_president_id = 1:n()) %>%
  left_join(
    df_approval %>%
      distinct(pollster_president_id, president)
  ) %>%
  group_by(president) %>%
  summarize(q50_average = sum(`50%`))



y_hat_result <-
  fit$summary("hat_y_result", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9)))
colnames(y_hat_result) <- c("variable", "q10", "q25", "q50", "q75", "q90")
bayesplot::ppc_dens_overlay(data_list$y_result,
                    fit$draws("hat_y_result") %>%
                      posterior::as_draws_matrix() %>%
                      head(100))













