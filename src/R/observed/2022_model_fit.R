## Model on 2022 polls
# -- Clean
rm(list = ls())
# -- Libraries
library(tidyverse)
library(cmdstanr)
# -- Run cleaning file
source("src/R/organizing/observed_polls/poll_cleaning.R")
# -- Source functions
source("src/R/functions/obs_create_variable_inclusion_input.R")
# -- Load
## 1. file with polls
df <- read_csv("dta/polls_dta/2020_polls_clean.csv") %>%
  filter(t_unit <= 3) %>%
  group_by(survey_id) %>%
  mutate(survey_id = cur_group_id()) %>%
  group_by(candidate_id) %>%
  mutate(candidate_id = cur_group_id()) %>%
  group_by(question_id) %>%
  mutate(question_id = cur_group_id()) %>%
  group_by(pollster_id) %>%
  mutate(pollster_id = cur_group_id()) %>%
  ungroup()
## 2. Skip vector
t_diff <- read_rds("dta/polls_dta/t_diff.Rds")
t_diff <- t_diff[1:2]
## 3. Time identifiers
df_time <- read_csv("dta/polls_dta/time_identifiers.csv")
# -- Prepare data list
S_1r_surveys <- df %>%
  distinct(survey_id) %>%
  nrow()
N_1r <- df %>%
  distinct(question_id) %>%
  nrow()
N_2r <- 0
P <- df %>%
  distinct(candidate_id) %>%
  nrow()
R <- df %>%
  distinct(pollster_id) %>%
  nrow()
T_unit <- df %>%
  distinct(t_unit) %>%
  nrow()
t_unit_skip <- t_diff

s_1r <- df %>%
  distinct(survey_id, question_id) %>%
  pull(survey_id)
r_1r <- df %>%
  distinct(survey_id, pollster_id) %>%
  pull(pollster_id)
t_unit_1r <- df %>%
  distinct(survey_id, t_unit) %>%
  pull(t_unit)

inclusion_input <- obs_create_variable_inclusion_input(df)

P_1r <- inclusion_input$P_first_round
N_combinations <- inclusion_input$N_combinations
P_N_combinations <- inclusion_input$P_N_combinations
p_1r_included <- inclusion_input$p_first_round_included
p_1r_excluded <- inclusion_input$p_first_round_excluded
p_id <- inclusion_input$p_id
y_1r <- inclusion_input$y_first_round

data_list <- list(
  S_1r_surveys = S_1r_surveys,
  N_1r = N_1r,
  N_2r = N_2r,
  P = P,
  R = R,
  T_unit = T_unit,
  t_unit_skip = t_unit_skip,
  s_1r = s_1r,
  r_1r = r_1r,
  t_unit_1r = t_unit_1r,
  P_1r = P_1r,
  N_combinations = N_combinations,
  P_N_combinations = P_N_combinations,
  p_1r_included = p_1r_included,
  p_1r_excluded = p_1r_excluded,
  p_id = p_id,
  y_1r = y_1r %>% t()
)

## Model
# -- Load
mod <- cmdstan_model("src/stan/v1_current_election_no_round.stan")
# -- Fit
fit <- mod$sample(
  data = data_list,
  chains = 4,
  iter_sampling = 400,
  iter_warmup = 500,
  parallel_chains = 4,
  refresh = 250,
  init = 0.2
)



fit$summary("sigma_alpha")


