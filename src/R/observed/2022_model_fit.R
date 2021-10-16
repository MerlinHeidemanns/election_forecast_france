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
NSurveys <- df %>%
  distinct(survey_id) %>%
  nrow()
NPolls <- df %>%
  distinct(question_id) %>%
  nrow()
NCandidates <- df %>%
  distinct(candidate_id) %>%
  nrow()
NPollsters <- df %>%
  distinct(pollster_id) %>%
  nrow()
NTime <- df %>%
  distinct(t_unit) %>%
  nrow()
t_unit <- t_diff

id_P_survey <- df %>%
  distinct(survey_id, question_id) %>%
  pull(survey_id)
id_S_pollster <- df %>%
  distinct(survey_id, pollster_id) %>%
  pull(pollster_id)
id_S_time <- df %>%
  distinct(survey_id, t_unit) %>%
  pull(t_unit)

inclusion_input <- obs_create_variable_inclusion_input(df)

NCandidates_Poll <- inclusion_input$P_first_round
NCombinations <- inclusion_input$N_combinations
NCandidate_Combinations <- inclusion_input$P_N_combinations
candidates_included <- inclusion_input$p_first_round_included
candidates_excluded <- inclusion_input$p_first_round_excluded
id_P_combinations <- inclusion_input$p_id
y <- inclusion_input$y_first_round

## Load past data
candidate_id_df <- read_csv("dta/polls_dta/candidate_identifiers_long.csv")


df_past <- read.csv(file = "dta/polls_dta/past_polls_clean.csv")
election_dates <- read_csv("dta/polls_dta/election_dates.csv")
df_past <- df_past %>%
  left_join(election_dates, by = c("election_year" = "year"))
df_past <- df_past %>%
  mutate(diff_time = difftime(date_first_round, end_day, units = "days")) %>%
  filter(diff_time < 21,
         diff_time > 0) %>%
  left_join(candidate_id_df %>%
              group_by(year) %>%
              mutate(NCandidates = n(),
                     included = 1), by =
              c("election_year" = "year",
                "candidate_long_id" = "candidate_long_id")) %>%
  group_by(poll_id) %>%
  filter(n() > 3,
         sum(!is.na(included)) == NCandidates |
         (sum(!is.na(included)) + 1) == NCandidates) %>%
  ungroup()

df_past <- df_past %>%
  arrange(election_year, poll_id, pollster_id, candidate_long_id)

NElections_past <- df_past %>%
  distinct(election_year) %>%
  nrow()

NCandidates_past <- df_past %>%
  distinct(election_year, candidate_long_id) %>%
  group_by(election_year) %>%
  summarize(n = max(candidate_long_id)) %>%
  pull(n) %>%
  array()

NPolls_past <- df_past %>%
  distinct(election_year, poll_id) %>%
  group_by(election_year) %>%
  summarize(n = n()) %>%
  pull(n) %>%
  array()
NPollsters_past <- df_past %>%
  distinct(election_year, pollster_id) %>%
  group_by(election_year) %>%
  summarize(n = n()) %>%
  pull(n) %>%
  array()

id_r_past <- df_past %>%
  distinct(election_year, poll_id, pollster_id) %>%
  group_by(election_year, pollster_id) %>%
  mutate(pollster_id_model = cur_group_id()) %>%
  pull(pollster_id_model)

id_rt_past <- df_past %>%
  distinct(election_year, pollster_id) %>%
  group_by(election_year) %>%
  mutate(i = cur_group_id()) %>%
  pull(i)

id_t_past <- df_past %>%
  distinct(election_year, poll_id) %>%
  group_by(election_year) %>%
  mutate(i = cur_group_id()) %>%
  pull(i)

results <- matrix(-999, nrow = NElections_past,
                  ncol = max(NCandidates_past))
years <- c(2002, 2007, 2012, 2017)
for (jj in 1:length(years)){
   tmp <- read_csv("dta/polls_dta/election_results_clean.csv") %>%
    filter(year == years[jj]) %>%
    left_join(read_csv("dta/polls_dta/candidate_identifiers_long.csv") %>%
                filter(year == years[jj]),
              by = c("candidate" = "long_name",
                     "year" = "year")) %>%
    arrange(candidate_long_id) %>%
    pull(percentage)
  print(tmp)
  results[jj,1:NCandidates_past[jj]] <- tmp
}

y_past <- df_past %>%
  arrange(poll_id, candidate_long_id) %>%
  dplyr::select(poll_id, candidate_long_id, y) %>%
  pivot_wider(id_cols = poll_id,
              names_from = candidate_long_id,
              names_prefix = "c",
              values_from = y,
              values_fill = -999) %>%
  dplyr::select(-poll_id)
y_past <- y_past[, paste0("c", seq(1, ncol(y_past)))]

abstention_omitted <- inclusion_input$abstention_omitted
abstention_omitted_past <- as.integer(-999 == y_past[,1])


## Transition probability prior
#' Get bloc data
bloc_vector <- c("Abstention",
                 "Extreme gauche a gauche",
                 "Gauche a centre gauche",
                 "Centre gauche a centre droit",
                 "Centre droit a droite",
                 "Droite a extreme droite")
horseshoe <- matrix(
       rbind(c(2,2,2,2,2,2),
       c(2,5,3,1,3,5),
       c(2,3,5,3,1,3),
       c(2,1,3,5,3,1),
       c(2,3,1,3,5,5),
       c(2,5,3,1,3,5)),
       ncol = 6,
       nrow = 6)
candidate_party_id <- read_csv("dta/polls_dta/candidate_party_identifiers.csv") %>%
  mutate(bloc_politiques = as.integer(factor(bloc_politiques, levels = bloc_vector)))
candidate_vector <- candidate_party_id %>% pull(long_name)
bloc <- candidate_party_id %>% pull(bloc_politiques)
transition_probability_prior <- matrix(NA, nrow = NCandidates,
                                       ncol = NCandidates - 1)
for (jj in 1:NCandidates){
  blow_own <- bloc[jj]
  bloc_other <- bloc[seq(1, NCandidates) != jj]
  transition_probability_prior[jj,] <- horseshoe[blow_own, bloc_other]
}


## data list
data_list <- list(
  NSurveys = NSurveys,
  NPolls = NPolls,
  NCandidates = NCandidates,
  NPollsters = NPollsters,
  NTime = NTime,
  t_unit = t_unit,
  id_P_survey = id_P_survey,
  id_S_pollster = id_S_pollster,
  id_S_time = id_S_time,

  transition_probability_prior = transition_probability_prior,

  NCandidates_Poll = NCandidates_Poll,
  NCombinations = NCombinations,
  NCandidate_Combinations = NCandidate_Combinations,
  candidates_included = candidates_included,
  candidates_excluded = candidates_excluded,
  id_P_combinations = id_P_combinations,
  y = y %>% t(),

  NElections_past = NElections_past,
  NPolls_past = NPolls_past,
  NCandidates_past = NCandidates_past,
  NPollsters_past = NPollsters_past,
  id_r_past = id_r_past,
  id_rt_past  = id_rt_past,
  id_t_past = id_t_past,
  results = results %>% t(),
  y_past = y_past %>% t(),

  abstention_omitted = abstention_omitted,
  abstention_omitted_past = abstention_omitted_past

)
for (j in 1:length(data_list)){
  if (data_list[[j]] %>% is.na() %>% any()){
    print(names(data_list)[j])
  }
}

## Model
# -- Load
mod <- cmdstan_model("src/stan/v1_with_past.stan")
# -- Fit
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 100,
  iter_warmup = 200,
  parallel_chains = 6,
  refresh = 25,
  init = 0.2
)
fit$save_object(file = "dta/fits/2021_09_28.Rds")
# -- Diagnostics
fit <- read_rds("dta/fits/2021_09_28.Rds")
fit$profiles()

fit$cmdstan_diagnose()

fit$sampler_diagnostics() %>%
  posterior::as_draws_df() %>%
  pull(treedepth__) %>%
  table()
fit <- read_rds("dta/fits/2021_09_28.Rds")
# -- Posterior Predictive Checks
source("src/R/functions/ppc_obs_alpha.R")
source("src/R/functions/ppc_obs_xi.R")
source("src/R/functions/ppc_obs_theta_mway_election_day.R")
source("src/R/functions/ppc_obs_theta_plt_hist.R")
supvec_names <- read.csv("dta/polls_dta/candidate_identifiers.csv") %>%
  pull(candidate)
supvec_time <- read_csv("dta/polls_dta/time_identifiers.csv") %>%
  pull(end_date)
supvec_bloc <- read.csv("dta/polls_dta/candidate_party_identifiers.csv")
## Obs theta by bloc
source("src/R/functions/ppc_obs_theta_bloc_politiques.R")
ppc_obs_theta_bloc_politiques(fit)
## Obs three-way Bertrand, Macron, Le Pen
source("src/R/functions/ppc_obs_theta_mway_election_day.R")
source("src/R/functions/ppc_obs_theta_plt_hist.R")
df_out <- ppc_obs_theta_mway_election_day(fit, c("_Abstention",
                                                 "Anne Hidalgo",
                                                 "Xavier Bertrand"), 500)
ppc_obs_theta_plt_hist(df_out)
## Polling error
source("src/R/functions/ppc_obs_xi.R")
ppc_obs_xi(fit, supvec_names = supvec_names)
## Polling house deviation
source("src/R/functions/ppc_obs_alpha.R")
supvec_pollster <- read_csv("dta/polls_dta/pollster_identifiers.csv") %>%
  pull(pollName)
ppc_obs_alpha(fit, supvec_names = supvec_names, supvec_pollster)
## Prob sigma cov
source("src/R/functions/ppc_obs_prob_sigma_cov.R")
ppc_obs_prob_sigma_cov(fit, supvec_names)
## Transition matrix
source("src/R/functions/ppc_obs_transition_matrix.R")
ppc_obs_transition_matrix(fit, supvec_names)
## two-way win probability second round
source("src/R/functions/ppc_obs_theta_twoway_plt.R")
ppc_obs_theta_twoway_plt(fit, NIter = 300)
## Sigma
source("src/R/functions/ppc_obs_sigma.R")
ppc_obs_sigma(fit)
## Polling error past
source("src/R/functions/ppc_obs_prob_xi_past.R")
ppc_obs_prob_xi_past(fit)
## Polling error by bloc
source("src/R/functions/ppc_obs_prob_xi_past.")
ppc_obs_prob_xi_past_bloc(fit)


candidate_subset <- c("_Abstention",
                      "Arnaud Montebourg",
                      "Anne Hidalgo",
                      "Emmanuel Macron")
ppc_obs_theta_mway_time_plt <- function(fit, candidate_subset, NIter = 500){
  candidate_list <- read.csv("dta/polls_dta/candidate_identifiers.csv") %>%
    pull(candidate)
  if (!all(candidate_subset %in% candidate_list)){
    stop("Please check the spelling of the candidates' names.")
  }
  candidate_subset_id <- match(candidate_subset, candidate_list)
  NCandidates <- length(candidate_list)

  #' Determine number of iterations
  iterations <- fit$metadata()$iter_sampling * length(fit$metadata()$id)

  #' Sample iterations
  iter_subset <- sort(sample(1:iterations, NIter))

  ## Transition matrix
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
  theta <- fit$draws("prob_theta") %>%
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
    dplyr::select(-ct) %>%
    pivot_wider(id_cols = c(time_id, candidate_id),
                names_from = iter,
                values_from = val)

  #' subset vector
  included <- candidate_subset_id
  excluded <- seq(1, NCandidates)[!seq(1, NCandidates) %in% included]

  theta_time <- lapply(1:max(theta$time_id), function(ii){
    theta_subset <- theta %>%
      filter(time_id == ii) %>%
      dplyr::select(-time_id, -candidate_id) %>%
      as.matrix()
    #' Conditioning
    df_out <- lapply(1:NIter, function(j){
      theta_cond <- theta_subset[included, j] + trans_mat_array[included, excluded, j] %*% solve(trans_mat_array[excluded,  excluded, j]) %*% (0 - theta_subset[excluded,j])
      out <- data.frame(theta_cond = theta_cond,
                        candidate_id = candidate_list[included],
                        j = iter_subset[j],
                        time_id = ii) %>%
        return(out)
    }) %>%
      do.call("bind_rows", .) %>%
    return(df_out)
  }) %>%
    do.call("bind_rows", .)

  ## Load time ids
  time <- read_csv("dta/polls_dta/time_identifiers.csv")

  ## Summarize and merge time ids in
  theta_time <- theta_time %>%
    group_by(candidate_id, time_id) %>%
    summarize(
      q50 = quantile(theta_cond, 0.5),
      q25 = quantile(theta_cond, 0.25),
      q75 = quantile(theta_cond, 0.75),
      q10 = quantile(theta_cond, 0.1),
      q90 = quantile(theta_cond, 0.9),
    ) %>%
    left_join(time, by = c("time_id" = "t_unit"))

  ## Return plt
  plt <- ggplot(theta_time, aes(x = end_date, y = q50)) +
    geom_line(aes(color = candidate_id)) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = candidate_id), alpha = 0.3) +
    theme_light() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.x = element_blank()) +
    labs(y = "Support",
         caption = "Median, 50%")

  return(plt)
}


ppc_obs_theta_mway_time_plt(fit, candidate_subset, 400)


