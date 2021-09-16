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
df <- read_csv("dta/polls_dta/2020_polls_clean.csv")
## 2. Skip vector
t_diff <- read_rds("dta/polls_dta/t_diff.Rds")
t_diff <- t_diff
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

#check_y_input(p_id, p_1r_included, df, P_N_combinations)
#check_y_input <- function(p_id, p_1r_included, df, P_N_combinations){
  input_df <- lapply(1:length(p_id), function(j){
    out <- data.frame(
      question_id = j,
      y = y_1r[j, 1:P_N_combinations[p_id[j]]],
      candidate_id = p_1r_included[p_id[j], 1:P_N_combinations[p_id[j]]]
    )
    return(out)
  }) %>%
    do.call("bind_rows", .)
  input_df <- input_df %>%
    group_by(question_id) %>%
    mutate(percentage = y / sum(y)) %>%
    left_join(
      df %>%
        distinct(candidate, candidate_id),
      by = c("candidate_id")
    ) %>%
    mutate(n = n()) %>%
    filter(n > 2) %>%
    group_by(candidate) %>%
    summarize(
      q10 = quantile(percentage, 0.1),
      q25 = quantile(percentage, 0.25),
      q50 = quantile(percentage, 0.50),
      q75 = quantile(percentage, 0.75),
      q90 = quantile(percentage, 0.90),
      min = min(percentage),
      max = max(percentage)
    )
  return(input_df)
}







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
for (j in 1:length(data_list)){
  if (data_list[[j]] %>% is.na() %>% any()){
    print(names(data_list)[j])
  }
}

## Model
# -- Load
mod <- cmdstan_model("src/stan/v1_current_election_transition_matrix_no_round_no_past.stan")
# -- Fit
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 250,
  iter_warmup = 750,
  parallel_chains = 6,
  refresh = 50,
  init = 0.2
)
fit$save_object(file = "dta/fits/2021_09_15.Rds")
# -- Posterior Predictive Checks


fit$summary("sigma_alpha")
supvec_names <- read.csv("dta/polls_dta/candidate_identifiers.csv") %>%
  pull(candidate)
supvec_time <- read_csv("dta/polls_dta/time_identifiers.csv") %>%
  pull(end_date)
supvec_bloc <- read.csv("dta/polls_dta/candidate_party_identifiers.csv")
pi_theta <- fit$summary("pi_theta_1r", ~ quantile(., c(0.10, 0.25, 0.5, 0.75, 0.9))) %>%
  pivot_longer(c(-variable),
               names_to = "quantile",
               values_to = "val",
               names_pattern = "(\\d+)") %>%
  pivot_wider(id_cols = variable,
              names_from = quantile,
              values_from = val,
              names_prefix = "q") %>%
  mutate(
    candidate = supvec_names[as.integer(str_match(variable, "(\\d+),")[,2])],
    time = supvec_time[as.integer(str_match(variable, ",(\\d+)")[,2])]
  )
ppc_obs_theta_bloc_politiques(fit)








candidate_list <- supvec_names
candidate_subset <- c("Marine Le Pen", "Emmanuel Macron")
candidate_subset_id <- match(candidate_subset, candidate_list)
NCandidates <- length(candidate_list)
NCandidates <- 24
NIter <- 500

#' Determine number of iterations
iterations <- fit$metadata()$iter_sampling * length(fit$metadata()$id)

#' Sample iterations
iter_subset <- sort(sample(1:iterations, NIter))

#' Subset transition matrix and turn into data frame
trans_mat <- fit$draws("transition_matrix") %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = "cc",
               values_to = "val") %>%
  filter(iter %in% iter_subset) %>%
  mutate(
    p1 = as.integer(str_match(cc, "(\\d+),")[,2]),
    p2 = as.integer(str_match(cc, ",(\\d+)")[,2])
  ) %>%
  dplyr::select(-cc) %>%
  filter(!is.na(p1))
#' Shape into matrizes
trans_mat_list <- lapply(unique(trans_mat$iter), function(ii){
  trans_mat %>%
    filter(iter == ii) %>%
    dplyr::select(-iter) %>%
    pivot_wider(id_cols = p1,
                names_from = p2,
                values_from = val) %>%
    dplyr::select(-p1) %>%
    as.matrix() %>%
    return()
})
#' Save into array
trans_mat_array <- array(NA, dim = c(nrow(trans_mat_list[[1]]),
                  ncol(trans_mat_list[[1]]),
                  length(trans_mat_list)))
for (jj in 1:length(trans_mat_list)){
  trans_mat_array[,,jj] <- trans_mat_list[[jj]]
}

## Extract theta
theta <- fit$draws("pi_theta_1r") %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = "ct",
               values_to = "val") %>%
  filter(iter %in% iter_subset) %>%
  mutate(
    candidate_id = as.integer(str_match(ct, "(\\d+),")[,2]),
    time_id = as.integer(str_match(ct, ",(\\d+)")[,2])
  ) %>%
  filter(!is.na(time_id)) %>%
  filter(time_id == max(time_id)) %>%
  dplyr::select(-time_id, -ct) %>%
  pivot_wider(id_cols = candidate_id,
              names_from = iter,
              values_from = val) %>%
  dplyr::select(-candidate_id) %>%
  as.matrix()

#' subset vector
included <- candidate_subset_id
excluded <- seq(1, NCandidates)[!seq(1, NCandidates) %in% included]

df_out <- lapply(1:NIiter, function(j){
  theta_cond <- theta[included, j] + trans_mat_array[included, excluded, j] %*% solve(trans_mat_array[excluded,  excluded, j]) %*% (0 - theta[excluded,j])
  out <- data.frame(theta_cond = theta_cond,
             candidate_id = candidate_list[included],
             j = iter_subset[j]) %>%
  return(out)
}) %>%
  do.call("bind_rows", .)













pi_theta <- fit$draws("pi_theta_1r") %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = "var",
               values_to = "val") %>%
  mutate(
    candidate = supvec_names[as.integer(str_match(var, "(\\d+),")[,2])],
    time = supvec_time[as.integer(str_match(var, ",(\\d+)")[,2])]
  ) %>%
  filter(!is.na(candidate)) %>%
  left_join(supvec_bloc) %>%
  group_by(iter, bloc_politiques, time) %>%
  summarize(val = sum(val)) %>%
  group_by(bloc_politiques, time) %>%
  summarize(
    q50 = quantile(val, 0.5),
    q25 = quantile(val, 0.25),
    q75 = quantile(val, 0.75)
  )



ggplot(pi_theta %>% filter(candidate %in% c("Emmanuel Macron", "Marine Le Pen", "Anne Hidalgo")),
       aes(x = time, y = q50, color = candidate)) +
  geom_line()
ggplot(pi_theta,
       aes(x = time, y = q50, color = candidate)) +
  geom_line()








