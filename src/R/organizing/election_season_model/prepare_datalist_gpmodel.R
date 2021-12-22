################################################################################
## Title: Prepare and run model
################################################################################
## Libraries
library(tidyverse)
library(cmdstanr)
################################################################################
## Functions
source("src/R/functions/sim_random_walk_bloc.R")
source("src/R/functions/sim_polling_data_blocs.R")
source("src/R/functions/exp_softmax.R")
source("src/R/functions/create_variable_inclusion_input.R")
source("src/R/functions/create_y_first_round.R")
################################################################################
## Load data
df_cleaned <- read_rds(file = "dta/polls_dta/election_season_model/poll_input.rds")
## Polls
NSeasons <- 1
data_polls <- lapply(1:NSeasons, function(ii){
  return(df_cleaned[[ii]]$polls)
}) %>%
  do.call("bind_rows", .) %>%
  group_by(election_id, question_id, survey_id) %>%
  mutate(NCandidates = n()) %>%
  group_by(election_id) %>%
  mutate(NCandidates_max = max(NCandidates)) %>%
  filter(NCandidates == NCandidates_max) %>%
  ungroup() %>%
  #' Abstention
  group_by(question_id, election_id) %>%
  mutate(abstention = ifelse(candidate == "_Abstention", 1, 0),
         abstention_omitted = c(1, 0)[max(abstention) + 1]) %>%
  ungroup() %>%
  arrange(election_id, pollster_id) %>%
  group_by(election_id, pollster_id) %>%
  mutate(pollster_id = cur_group_id()) %>%
  ungroup() %>%
  group_by(election_id) %>%
  mutate(question_id = as.integer(factor(question_id))) %>%
  ungroup() %>%
  arrange(election_id, pollster_id, survey_id, question_id, candidate_id) %>%
  mutate(survey_id = as.integer(factor(paste(survey_id, election_id, question_id, sep = "_" ),
                                levels = unique(paste(survey_id, election_id, question_id, sep = "_" ))))) %>%
  distinct(question_id, election_id, survey_id, candidate_id, .keep_all = TRUE) %>%
  group_by(election_id) %>%
  mutate(question_id = as.integer(factor(paste(question_id, election_id, sep = "_" ),
                                       levels = unique(paste(question_id, election_id, sep = "_" ))))) %>%
  distinct(question_id, election_id, candidate_id, .keep_all = TRUE) %>% View()
  group_by(question_id, election_id, bloc_id, time_unit_id, abstention_omitted) %>%
  summarize(y = sum(y))




################################################################################
## Datalist
NBlocs <- 6
NPolls <- data_polls %>%
  distinct(election_id, question_id) %>%
  nrow()
NCandidates <- data_polls %>%
  distinct(election_id, candidate_id) %>%
  group_by(election_id) %>%
  summarize(N = max(candidate_id)) %>% # living on the edge
  pull(N)
NPollsters <- data_polls %>%
  distinct(election_id, pollster_id) %>%
  nrow()
t_unit_df <- data_polls %>%
  distinct(election_id, time_id) %>%
  arrange(election_id, time_id) %>%
  group_by(election_id) %>%
  mutate(t_unit = 1:n(),
         t_unit_skip = time_id - lag(time_id)) %>%
  ungroup()
NTime <- t_unit_df %>%
  group_by(election_id) %>%
  summarize(N = max(time_id)) %>%
  pull(N)





NPolls_Pollster <- data_polls %>%
  distinct(question_id, pollster_id) %>%
  group_by(pollster_id) %>%
  summarize(N = n()) %>%
  pull(N)
NPollsters_Season <- data_polls %>%
  distinct(election_id, pollster_id) %>%
  group_by(election_id) %>%
  summarize(N = n()) %>%
  pull(N)

id_C_blocs <- matrix(-99, nrow = NSeasons, ncol = max(NCandidates))
for (jj in 1:NSeasons){
  tmp <- data_polls %>%
    filter(election_id == jj) %>%
    distinct(candidate_id, bloc_id) %>%
    arrange(candidate_id) %>%
    pull(bloc_id)
  if (tmp[1] != 1){
    tmp <- c(1, tmp)
  }
  id_C_blocs[jj, 1:NCandidates[jj]] <- tmp
}


id_S_season <- data_polls %>%
  distinct(election_id, question_id) %>%
  arrange(election_id, question_id) %>%
  pull(election_id)
id_P_season <- data_polls %>%
  distinct(election_id, pollster_id) %>%
  arrange(pollster_id) %>%
  pull(election_id)
id_S_pollster <- data_polls %>%
  distinct(election_id, question_id, pollster_id) %>%
  arrange(election_id, question_id) %>%
  pull(pollster_id)


id_S_time <- data_polls %>%
  left_join(t_unit_df) %>%
  distinct(election_id, question_id, time_id) %>%
  arrange(election_id, question_id) %>%
  pull(time_id)

y <- data_polls %>%
  select(election_id, question_id, y, candidate_id) %>%
  pivot_wider(
    id_cols = c(election_id, question_id),
    values_from = y,
    names_from = candidate_id
  ) %>%
  ungroup() %>%
  select(-election_id, -question_id) %>%
  as.matrix()
y <- y[, order(as.integer(colnames(y)))]
y[is.na(y)] <- -99
y <- y %>% t()






abstention_omitted <- as.integer(y[1, ] == -99)

prior_sigma_alpha = 0.01
prior_sigma_tau = 0.005
prior_sigma_cov = 0.01
prior_sigma_xi = 0.1

################################################################################
## Datalist
data_list <- list(
  NPolls = NPolls,
  NBlocs = NBlocs,
  NSeasons = NSeasons,
  NCandidates = NCandidates %>% as.array(),
  NPollsters = NPollsters,
  NTime = NTime %>% as.array(),

  NPolls_Pollster = NSurveys_Pollster,
  NPollsters_Season = NPollsters_Season %>% array(),
  id_C_blocs = id_C_blocs,
  id_P_season = id_P_season,
  id_S_season = id_S_season,
  id_S_pollster = id_S_pollster,
  id_S_time = id_S_time,
  abstention_omitted = abstention_omitted,
  y = y,

  prior_sigma_xi = prior_sigma_xi,
  prior_sigma_alpha = prior_sigma_alpha,
  prior_sigma_tau = prior_sigma_tau,
  prior_sigma_cov = prior_sigma_cov
)
for (jj in 1:length(data_list)){
  if (any(is.na(data_list[[jj]]))){
    print(names(data_list)[[jj]])
  }
}
################################################################################
## Model
mod <- cmdstan_model("src/stan/models_election_season/main_ar_process.stan")
################################################################################
## Fit model
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 300,
  iter_warmup = 300,
  parallel_chains = 6,
  refresh = 25,
  init = 0
)
################################################################################
## Save model
fit$save_object(file = "dta/fits/2021_10_30_election_season_ar.Rds")

fit <- read_rds(file = "dta/fits/2021_10_30_election_season_ar.Rds")
write_rds(list(
  fit = fit,
  data_list = data_list,
  data_polls = data_polls
),
file = "dta/fits/2021_10_30_election_season_list_ar.Rds")




