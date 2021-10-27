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
df_cleaned <- read_rds(file = "dta/polls_dta/primary_model_input/poll_input.rds")
## Polls
NSeasons <- 1
data_polls <- lapply(1:NSeasons, function(ii){
  return(df_cleaned[[6 - ii]]$polls %>%
           mutate(election_id = ii))
}) %>%
  do.call("bind_rows", .) %>%
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
  distinct(question_id, election_id, survey_id, candidate_id, .keep_all = TRUE)


################################################################################
## Datalist
NSurveys <- data_polls %>%
  distinct(election_id, survey_id) %>%
  nrow()
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
  summarize(N = n()) %>%
  pull(N)
t_unit <- matrix(-99, nrow = NSeasons, ncol = max(NTime) - 1)
for (jj in 1:NSeasons){
  t_unit[jj,1:(NTime[jj] - 1)] <- t_unit_df %>% filter(election_id == jj, !is.na(t_unit_skip)) %>% pull(t_unit_skip)
}




NSurveys_Pollster <- data_polls %>%
  distinct(survey_id, pollster_id) %>%
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


id_P_survey <- data_polls %>%
  distinct(question_id, survey_id) %>%
  pull(survey_id)
id_S_season <-  data_polls %>%
  distinct(election_id, survey_id) %>%
  arrange(survey_id) %>%
  pull(election_id)
id_P_season <-  data_polls %>%
  distinct(election_id, pollster_id) %>%
  arrange(pollster_id) %>%
  pull(election_id)
id_S_pollster <- data_polls %>%
  distinct(survey_id, pollster_id) %>%
  arrange(survey_id) %>%
  pull(pollster_id)
id_S_pollster <- data_polls %>%
  distinct(survey_id, pollster_id) %>%
  arrange(survey_id) %>%
  pull(pollster_id)

id_S_time <- data_polls %>%
  left_join(t_unit_df) %>%
  distinct(election_id, survey_id, t_unit) %>%
  arrange(survey_id) %>%
  pull(t_unit)

inclusion_data <- list()
for (jj in 1:NSeasons){
  inclusion_data[[jj]] <- create_variable_inclusion_input(
    data_polls %>% filter(election_id == jj))
}

NCandidates_Poll <- c()
NCombinations <- rep(NA, NSeasons)
for (jj in 1:NSeasons){
  NCandidates_Poll <- c(NCandidates_Poll,
                             inclusion_data[[jj]]$NCandidates_Poll)
  NCombinations[jj] <- inclusion_data[[jj]]$NCombinations
}

NCandidate_Combinations <- matrix(-99, nrow = NSeasons, ncol = max(NCombinations))
for (jj in 1:NSeasons){
  NCandidate_Combinations[jj , 1:NCombinations[jj]] <- inclusion_data[[jj]]$NCandidate_Combinations
}


candidates_included <- candidates_excluded <- array(-99, dim = c(
  max(NCombinations),
  NSeasons,
  max(NCandidates)
))
for (jj in 1:NSeasons){
  candidates_included[1:NCombinations[jj],jj, 1:NCandidates[jj]] <- inclusion_data[[jj]]$candidates_included
  candidates_excluded[1:NCombinations[jj],jj, 1:NCandidates[jj]] <- inclusion_data[[jj]]$candidates_excluded
}

id_P_combinations <- c()
for (jj in 1:NSeasons){
  id_P_combinations <- c(id_P_combinations, inclusion_data[[jj]]$combination_df$combination_id)
}


y <- matrix(-99, nrow = max(NCandidates), ncol = NPolls)
for (jj in 1:NSeasons){
  start <- match(-99, y[2,])
  y[1:NCandidates[jj], start:(start + dim(inclusion_data[[jj]]$y %>% t())[2] - 1)] <- inclusion_data[[jj]]$y %>% t()
}


abstention_omitted <- as.integer(y[1, ] == -99)

prior_sigma_alpha = 0.04
prior_sigma_tau = 0.01
prior_sigma_cov = 0.1
################################################################################
## Datalist
data_list <- list(
  NSurveys = NSurveys,
  NBlocs = NBlocs,
  NPolls = NPolls,
  NSeasons = NSeasons,
  NCandidates = NCandidates %>% as.array(),
  NPollsters = NPollsters,
  NTime = NTime %>% as.array(),
  t_unit = t_unit,

  NSurveys_Pollster = NSurveys_Pollster,
  NPollsters_Season = NPollsters_Season %>% array(),
  id_C_blocs = id_C_blocs,
  id_P_survey = id_P_survey,
  id_P_season = id_P_season,
  id_S_season = id_S_season,
  id_S_pollster = id_S_pollster,
  id_S_time = id_S_time,
  abstention_omitted = abstention_omitted,

  NCandidates_Poll = NCandidates_Poll,
  NCombinations = NCombinations %>% array(),
  NCandidate_Combinations = NCandidate_Combinations,
  candidates_included = candidates_included,
  candidates_excluded = candidates_excluded,
  id_P_combinations = id_P_combinations,
  y = y,

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
mod <- cmdstanr::cmdstan_model("src/stan/primary_model.stan")
################################################################################
## Fit model
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 300,
  iter_warmup = 300,
  parallel_chains = 6,
  refresh = 25,
  init = 0.2
)
fit$save_object(file = "dta/fits/2021_10_23_primary.Rds")
