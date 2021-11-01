###############################################################################
## Title: Prepare data list for approval model
###############################################################################
## Load data
df <- read_csv("dta/fundamentals_dta/approval.csv")
###############################################################################
## Libraries
library(tidyverse)
library(cmdstanr)
###############################################################################
## Data into correct format
df <- df %>%
  arrange(president_id)
NPolls <- df %>%
  group_by(president_id) %>%
  summarize(N = n()) %>%
  pull(N)
NPollsters <- df %>%
  distinct(pollster_id) %>%
  nrow()
NElections <- df %>%
  pull(president_id) %>%
  max()
id_Polls_elections <- df %>%
  pull(president_id)
id_Polls_pollster <- df %>%
  pull(pollster_id)
NTime <- df %>%
  group_by(president_id) %>%
  summarize(NTime = max(three_weeks)) %>%
  pull(NTime)
id_Polls_time <- df %>%
  pull(three_weeks)
y <- df %>%
  pull(y)
n <- df %>%
  pull(N)
###############################################################################
## Data list
data_list <- list(
  NPolls = NPolls,
  NElections = NElections,
  NPollsters = NPollsters,
  id_Polls_elections = id_Polls_elections,
  NTime = NTime,
  id_Polls_time = id_Polls_time,
  id_Polls_pollster = id_Polls_pollster,
  y = y,
  n = n,
  beta_prior = 0.0001
)
###############################################################################
## Model
mod <- cmdstan_model("src/stan/models_fundamentals/v1_fundamentals_approval.stan")
## Fit
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 300,
  iter_warmup = 300,
  parallel_chains = 6,
  refresh = 25,
  init = 0.25
)
###############################################################################
## Fit
prob_theta <- fit$summary("prob_theta", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9)))
colnames(prob_theta) <- c("variable", "q10", "q25", "q50", "q75", "q90")
prob_theta <- prob_theta %>%
  mutate(president_id = as.integer(str_match(variable, "\\[([\\d]+)")[,2]),
         week_id = as.integer(str_match(variable, "([\\d]+)\\]")[,2]))
ggplot(prob_theta, aes(x = week_id, y = q50)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "blue", alpha = 0.25) +
  geom_ribbon(aes(ymin = q10, ymax = q90), fill = "blue", alpha = 0.10) +
  theme_light() +
  facet_grid(president_id ~ .)





