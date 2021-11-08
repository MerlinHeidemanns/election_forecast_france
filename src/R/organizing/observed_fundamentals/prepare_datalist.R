###############################################################################
## Title: Prepare data list for approval model
###############################################################################
## Libraries
library(tidyverse)
library(cmdstanr)
###############################################################################
## Load data
df <- read_csv("dta/fundamentals_dta/approval.csv")
###############################################################################
## Data into correct format
df <- df %>%
  arrange(president_id) %>%
  mutate(start_date = min(date),
         two_weeks = as.integer(ceiling(difftime(date, start_date, units = "weeks")/2)),
         two_weeks = ifelse(two_weeks == 0, 1, two_weeks)) %>%
  group_by(president_id) %>%
  mutate(id_election_shift = min(two_weeks)) %>%
  group_by(two_weeks) %>%
  mutate(min_pres = min(president_id)) %>%
  filter(president_id == min_pres) %>%
  ungroup() %>%
  group_by(pollster_id, president_id) %>%
  mutate(pollster_president_id = cur_group_id()) %>%
  group_by(president_id) %>%
  mutate(time_president_id = two_weeks - min(two_weeks) + 1) %>%
  ungroup() %>%
  mutate(time_president_id = time_president_id/max(time_president_id))
NPolls <- df %>%
  nrow()
NPresidents <- df %>%
  distinct(president_id) %>%
  nrow()
NPollsters <- df %>%
  distinct(pollster_id) %>%
  nrow()
NTime <- df %>%
  pull(two_weeks) %>%
  max()
NPollsters_Presidents <- df %>%
  distinct(pollster_president_id) %>%
  nrow()
id_Polls_time <- df %>%
  pull(two_weeks)
id_Polls_pollster <- df %>%
  pull(pollster_id)
id_Polls_pollster_president <- df %>%
  pull(pollster_president_id)
id_Polls_time_president <- df %>%
  pull(time_president_id)
id_Polls_president <- df %>%
  pull(president_id)
y <- df %>%
  pull(p_approve)
n <- df %>%
  pull(N)
election_points <- c(df %>%
                       distinct(id_election_shift) %>%
                       pull(),
                     max(df$two_weeks))
###############################################################################
## Data list
data_list <- list(
  NPresidents = NPresidents,
  NPolls = NPolls,
  NPollsters = NPollsters,
  NTime = NTime,
  NPollsters_Presidents = NPollsters_Presidents,
  election_points = election_points,
  id_Polls_time = id_Polls_time,
  id_Polls_time_president = id_Polls_time_president,
  id_Polls_pollster = id_Polls_pollster,
  id_Polls_pollster_president = id_Polls_pollster_president,
  id_Polls_president = id_Polls_president,
  y = y,
  n = n,
  beta_prior = 0.01
)
###############################################################################
## Model
mod <- cmdstan_model("src/stan/models_fundamentals/v2_fundamentals_approval.stan")
## Fit
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 300,
  iter_warmup = 300,
  parallel_chains = 6,
  refresh = 50,
  init = 0.25
)
###############################################################################
## Fit
prob_theta <- fit$summary("prob_theta", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9)))
colnames(prob_theta) <- c("variable", "q10", "q25", "q50", "q75", "q90")
prob_theta <- prob_theta %>%
  mutate(week_id = as.integer(str_match(variable, "([\\d]+)\\]")[,2]))
ggplot(prob_theta, aes(x = week_id, y = q50)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "green", alpha = 0.25) +
  geom_ribbon(aes(ymin = q10, ymax = q90), fill = "green", alpha = 0.10) +
  theme_light()







