###############################################################################
## Title: Prepare data for model
###############################################################################
## libraries
library(tidyverse)
library(cmdstanr)
###############################################################################
## Load data
#' Past
df_past_polls <- read_csv("dta/polls_dta/model_input/polls_blosc_w_ids.csv")
df_elections <-
  read_csv("dta/polls_dta/model_input/election_results_blocs_cleaned.csv")
t_unit_past <- read_rds("dta/polls_dta/model_input/t_unit.rds")
#' Current
df_current_polls <-
  read_csv("dta/polls_dta/model_input/polls_current_clean.csv")
t_unit_current <- read_rds("dta/polls_dta/model_input/t_unit_current.rds")
candidate_blocs <- read_rds("dta/polls_dta/model_input/candidate_bloc_vector_current.rds")
candidate_vector <- read_rds("dta/polls_dta/model_input/candidate_vector_current.rds")
df_t_(df_t_unit, "dta/polls_dta/model_input/df_t_unit_current.csv")
df_t_unit_current <- read_csv("dta/polls_dta/model_input/df_t_unit_past.csv")

###############################################################################
## Load data
bloc_vector <- c("Abstention",
                 "Gauche radicale et extrême gauche",
                 "Gauche",
                 "Centre",
                 "Droite",
                 "Droite radicale et extreme droite")
###############################################################################
## Current data

NSurveys <- df_current_polls %>%
  distinct(survey_id) %>%
  nrow()
NPolls <- df_current_polls %>%
  distinct(question_id) %>%
  nrow()
NCandidates <- df_current_polls %>%
  distinct(candidate_id) %>%
  nrow()
NPollsters <- df_current_polls %>%
  distinct(pollster_id) %>%
  nrow()
NTime <- length(t_unit_current)
t_unit <- t_unit_current[2:NTime]
t_bloc_unit_prior <- t_unit_current[1]

NSurveys_Pollster <- df_current_polls %>%
  distinct(pollster_id, survey_id) %>%
  group_by(pollster_id) %>%
  summarize(N = n()) %>%
  pull(N)

id_P_survey <- df_current_polls %>%
  distinct(question_id, survey_id) %>%
  pull(survey_id)

id_S_time <- df_current_polls %>%
  distinct(survey_id, t_unit) %>%
  pull(t_unit)

id_S_pollster <- df_current_polls %>%
  distinct(survey_id, pollster_id) %>%
  pull(pollster_id)

id_C_blocs <- candidate_blocs


transition_probability_prior <- matrix(20,
                                       nrow = NCandidates,
                                       ncol = NCandidates - 1)


source("src/R/functions/create_variable_inclusion_input.R")
source("src/R/functions/create_y_first_round.R")
inclusion_input <- create_variable_inclusion_input(df_current_polls %>%
                                  mutate(abstention_omitted = 0))

NCandidates_Poll <- inclusion_input$NCandidates_Poll
NCombinations <- inclusion_input$NCombinations
NCandidate_Combinations <- inclusion_input$NCandidate_Combinations
candidates_included <- inclusion_input$candidates_included
candidates_excluded <- inclusion_input$candidates_excluded
id_P_combinations <- inclusion_input$combination_df$combination_id
y <- inclusion_input$y
abstention_omitted <- as.integer(y[, 1] == -99)

NBlocs <- 6
NElections <- df_elections %>%
  distinct(election_id) %>%
  nrow()
NPollsters_past <- df_past_polls %>%
  distinct(election_id, pollster_id) %>%
  group_by(election_id) %>%
  summarize(n = n()) %>%
  pull(n)
NPolls_Pollster_past <- df_past_polls %>%
  distinct(election_id, pollster_id, poll_id) %>%
  group_by(election_id, pollster_id) %>%
  summarize(n = n()) %>%
  pull(n)
NTime_past <- length(t_unit_past) + 1
t_unit_past <- t_unit_past
id_P_time_past <- df_past_polls %>%
  distinct(poll_id, t_unit) %>%
  pull(t_unit)
id_P_pollster_past <- df_past_polls %>%
  distinct(poll_id, pollster_id) %>%
  pull(pollster_id)
id_P_elections_past <- df_past_polls %>%
  distinct(poll_id, election_id) %>%
  pull(election_id)
election_results <- df_elections %>%
  select(election_id, bloc_id, percentage) %>%
  pivot_wider(id_cols = bloc_id,
              names_from = election_id,
              values_from = percentage) %>%
  select(-bloc_id) %>%
  as.matrix()
election_results[election_results == 0] <- 0.005
for (j in 1:ncol(election_results)){
  election_results[, j] <- election_results[,j]/sum(election_results[, j])
}

id_E_time <- df_elections %>%
  distinct(election_id, t_unit) %>%
  pull(t_unit)

y_past <- df_past_polls %>%
  select(bloc_id,y, poll_id) %>%
  pivot_wider(id_cols = poll_id,
              names_from = bloc_id,
              values_from = y) %>%
  select(-poll_id) %>%
  as.matrix()
y_past <- y_past[,order(colnames(y_past))]
y_past[is.na(y_past)] <- -99

abstention_omitted_past <- as.integer(y_past[, 1] == -99)


prior_sigma_xi <- 0.1
prior_sigma_alpha <- 0.05
prior_sigma_tau <- 0.02
prior_sigma_cov <- 0.02


data_list <- list(
  NSurveys = NSurveys,
  NBlocs = NBlocs,
  NPolls = NPolls,
  NCandidates = NCandidates,
  NPollsters = NPollsters,
  NTime = NTime,
  t_unit = t_unit,
  NSurveys_Pollster = NSurveys_Pollster,
  id_C_blocs = id_C_blocs,
  id_P_survey = id_P_survey,
  id_S_pollster = id_S_pollster,
  id_S_time = id_S_time,
  abstention_omitted = abstention_omitted,
  transition_probability_prior = transition_probability_prior,
  NCandidates_Poll = NCandidates_Poll,
  NCombinations = NCombinations,
  NCandidate_Combinations = NCandidate_Combinations,
  candidates_included = candidates_included,
  candidates_excluded = candidates_excluded,
  id_P_combinations = id_P_combinations,
  y = y %>% t(),
  NElections = NElections,
  NPollsters_past = NPollsters_past,
  NPolls_Pollster_past = NPolls_Pollster_past,
  NTime_past = NTime_past,
  t_bloc_unit_prior = t_bloc_unit_prior,
  t_unit_past = t_unit_past,
  id_P_time_past = id_P_time_past,
  id_P_pollster_past = id_P_pollster_past,
  id_P_elections_past = id_P_elections_past - 1,

  elections_results = election_results,
  id_E_time = id_E_time,
  y_past = y_past %>% t(),
  abstention_omitted_past = abstention_omitted_past,

  prior_sigma_xi = prior_sigma_xi,
  prior_sigma_alpha = prior_sigma_alpha,
  prior_sigma_tau = prior_sigma_tau,
  prior_sigma_cov = prior_sigma_cov
)

for (ii in 1:NCombinations){
  print(data_list$candidates_included[ii, 1:data_list$NCandidate_Combinations[ii]])
}

for (ii in 1:NCombinations){
  print(data_list$candidates_excluded[ii, 1:(data_list$NCandidates - data_list$NCandidate_Combinations[ii])])
}



mod <- cmdstan_model("src/stan/v2_main.stan")
## Fit model
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 300,
  iter_warmup = 300,
  parallel_chains = 6,
  refresh = 100,
  init = 1
)
fit$save_object(file = "dta/fits/2021_10_22.Rds")
# -- Diagnostics
fit <- read_rds("dta/fits/2021_09_28.Rds")


theta_blocs <- fit$summary("prob_theta_blocs", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9)))
colnames(theta_blocs) <- c("variable", "q10", "q25", "q50", "q75", "q90")
theta_blocs <- theta_blocs %>%
  mutate(
    bloc_id = as.integer(str_match(variable, "([\\d]+),")[,2]),
    time_unit_id = as.integer(str_match(variable, ",([\\d]+)")[,2]),
    bloc = bloc_vector[bloc_id],
    time_id = df_t_unit_current$date[time_unit_id]
  )

polls_past_plot <- df_past_polls %>%
  mutate(
    time_id = df_t_unit_current$date[t_unit],
    bloc = bloc_vector[bloc_id]
  ) %>%
  group_by(poll_id) %>%
  mutate(share = y/sum(y)) %>%
  ungroup()


ggplot(theta_blocs, aes(x = time_id, y = q50)) +
  geom_line() +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.3) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.1) +
  geom_point(data = polls_past_plot, aes(x = time_id, y = share), alpha = 0.2, size = 0.8) +
  facet_wrap(bloc ~ .)











