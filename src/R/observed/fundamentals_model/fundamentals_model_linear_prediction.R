###############################################################################
## Title: Prepare datalist for bloc fundamentals model
###############################################################################
## Description
#'
###############################################################################
## Notes
#' * the ordinal model is neat but expected to fail because
#' the counts are too big and we would face the same issue
#' with the national level election results as the model can't
#' sample properly and will throw things around
###############################################################################
## Clean
rm(list = ls())
###############################################################################
## Libraries
library(tidyverse)
library(cmdstanr)
library(bayesplot)
###############################################################################
## Vectors
bloc_vector <- c("Abstention",
                 "Gauche radicale et extreme gauche",
                 "Gauche",
                 "Ecologisme",
                 "Centre",
                 "Droite",
                 "Droite radicale et extreme droite")
bloc_vector <- bloc_vector[bloc_vector != "Abstention"]
election_years <- c(1988, 1995, 2002, 2007, 2012, 2017)
###############################################################################
## Load data
df <- read_csv(file = "dta/election_results/department_candidate_votes_blocs_clean.csv") %>%
  filter(year %in% election_years,
         departement != "Mayotte")
df_election_results <- read_csv(file = "dta/election_results/department_candidate_votes_blocs_clean.csv") %>%
  filter(departement != "Mayotte")
df_incumbency <- read_csv("dta/fundamentals_dta/cleaned_input/incumbency.csv")
df_input_department <-
  read_csv("dta/fundamentals_dta/cleaned_input/input_data.csv")
df_approval <- read_csv("dta/fundamentals_dta/approval.csv")

###############################################################################
## Election year and departement id
department_vector <- df %>%
  distinct(departement) %>%
  pull(departement) %>%
  sort()
###############################################################################
## Mangle incumbency
national_incumbency <- read_csv("dta/fundamentals_dta/cleaned_input/incumbency.csv") %>%
  mutate(bloc_id = match(incumbent_bloc, bloc_vector)) %>%
  select(bloc_id, election_year) %>%
  filter(election_year %in% election_years) %>%
  mutate(i = 1) %>%
  pivot_wider(id_cols = election_year,
              names_from = bloc_id,
              values_from = i,
              values_fill = 0) %>%
  select(-election_year)
###############################################################################
## Mangle reelection
national_reelection <- read_csv("dta/fundamentals_dta/cleaned_input/incumbency.csv") %>%
  mutate(bloc_id = match(incumbent_bloc, bloc_vector)) %>%
  select(bloc_id, election_year, reelection) %>%
  filter(election_year %in% election_years) %>%
  add_row(bloc_id = 1, reelection = 0, election_year  = 2002) %>%
  add_row(bloc_id = 3, reelection = 0, election_year  = 2002) %>%
  add_row(bloc_id = 4, reelection = 0, election_year  = 2002) %>%
  pivot_wider(id_cols = election_year,
              names_from = bloc_id,
              values_from = reelection,
              values_fill = 0) %>%
  select(-election_year)
national_reelection <- national_reelection[, sort(colnames(national_reelection))]
###############################################################################
## Mangle approval
df_approval <- df_approval %>%
  mutate(election_year = lubridate::year(date_next_first_round)#,
         #president = str_replace_all(president, "\\d", "")
         ) %>%
  filter(election_year %in% election_years) %>%
  arrange(election_year) %>%
  mutate(start_date = as.Date(paste(
    lubridate::year(date_next_first_round) - 1,
    "-08-01", sep = ""))) %>%
  filter(date >= (date_next_first_round - 360)) %>%
  mutate(time_id = 1 + floor((1 + as.integer(difftime(date, date_next_first_round - 360, units = "days")))/7)) %>%
  group_by(pollster_id, president_id) %>%
  mutate(pollster_president_id = cur_group_id()) %>%
  ungroup() %>%
  mutate(president_id = president_id - min(president_id) + 1,
         pollster_id = as.integer(factor(pollster))) %>%
  select(election_year, p_approve, N, pollster_president_id, president,
         pollster, pollster_id, president_id, method_id, method, time_id) %>%
  arrange(president_id, pollster_id) %>%
  mutate(pollster_president_id = as.integer(factor(paste(president_id, pollster))))

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
###############################################################################
## National predictors
XNation <- cbind(national_incumbency[, 1])
K <- ncol(XNation)
###############################################################################
## Mangle df
#' Complete
#' * The Green bloc supported the Left in
#' 2017 so no Green party challenger existed
df <- df %>%
  full_join(
    df %>%
      distinct(bloc, departement) %>%
      full_join(expand.grid(election_years,bloc_vector) %>%
                  rename(year = Var1,
                         bloc = Var2),
                by = c("bloc" = "bloc"))
  ) %>%
  mutate(percentage = ifelse(is.na(percentage), 0, percentage),
         votes = ifelse(is.na(votes), 0, votes))

#' Add ids
df <- df %>%
  mutate(department_id = match(departement, department_vector),
         election_id = match(year, election_years),
         bloc_id = match(bloc, bloc_vector))

#' Arrange and widen
df_wide <- df %>%
  arrange(election_id, department_id, bloc_id) %>%
  select(election_id, department_id, bloc_id, percentage) %>%
  filter(!is.na(bloc_id)) %>%
  group_by(election_id, department_id) %>%
  mutate(percentage = percentage/sum(percentage)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(election_id, department_id),
              names_from = bloc_id,
              values_from = percentage) %>%
  arrange(election_id, department_id)
###############################################################################
## Cut point priors
df <- read_csv(file = "dta/election_results/department_candidate_votes_blocs_clean.csv") %>%
  filter(departement != "Mayotte") %>%
  full_join(
    df %>%
      distinct(bloc, departement) %>%
      full_join(expand.grid(election_years,bloc_vector) %>%
                  rename(year = Var1,
                         bloc = Var2),
                by = c("bloc" = "bloc"))
  ) %>%
  mutate(percentage = ifelse(is.na(percentage), 0, percentage),
         votes = ifelse(is.na(votes), 0, votes))

cutpoint_priors <- df %>%
  group_by(year, bloc) %>%
  summarize(percentage = sum(percentage * votes/sum(votes))) %>%
  mutate(percentage = ifelse(is.nan(percentage), 0.05, percentage)) %>%
  pivot_wider(id_cols = year,
              names_from = bloc,
              values_from = percentage,
              values_fill =  0.05) %>%
  filter(year > 1980, year < 2017)
cutpoint_priors <- cutpoint_priors[, bloc_vector]
cutpoint_priors <- as.matrix(cutpoint_priors)
###############################################################################
## Mangle department level input
unemployment <- df_input_department %>%
  mutate(election_id = match(election_year, election_years),
         department_id = match(departement, department_vector)) %>%
  arrange(election_id, department_id) %>%
  select(q2)
lagged_share <- df_election_results %>%
  select(-votes) %>%
  pivot_wider(id_cols = c(year, departement),
              names_from = bloc,
              values_from = percentage,
              values_fill = 0) %>%
  mutate(share_droite = Droite + `Droite radicale et extreme droite`,
         share_gauche = Gauche + `Gauche radicale et extreme gauche`) %>%
  select(year, departement, share_droite, share_gauche) %>%
  group_by(departement) %>%
  arrange(year) %>%
  mutate(lag_share_droite = lag(share_droite),
         lag_share_gauche = lag(share_gauche)) %>%
  select(year, departement, contains("lag_")) %>%
  filter(year %in% election_years) %>%
  mutate(election_id = match(year, election_years),
         department_id = match(departement, department_vector)) %>%
  arrange(election_id, department_id) %>%
  ungroup() %>%
  group_by(election_id) %>%
  mutate(
    lag_share_droite = (lag_share_droite - mean(lag_share_droite))/sd(lag_share_droite),
    lag_share_gauche = (lag_share_gauche - mean(lag_share_gauche))/sd(lag_share_gauche)
  ) %>%
  ungroup() %>%
  select(lag_share_droite, lag_share_gauche)

mean(log(unemployment[!is.na(unemployment)]))
std_log_unemployment <- (log(unemployment) - mean(log(unemployment[!is.na(unemployment)])))/sd(log(unemployment[!is.na(unemployment)]))
XDepartments <- cbind(std_log_unemployment, lagged_share)
M <- ncol(XDepartments)
XDepartments[is.na(XDepartments)] <- -99
NMiss_X <- as.integer(sum(XDepartments[,1] == -99))
id_X_miss <- seq(1:nrow(XDepartments))[XDepartments[,1] == -99]
###############################################################################
## Datalist preparation
NObs <- df_wide %>%
  nrow()
NBlocs <- df_wide %>%
  select(!contains("id")) %>%
  ncol()
NElections <- df_wide %>%
  distinct(election_id) %>%
  nrow()
NDepartments <- df_wide %>%
  distinct(department_id) %>%
  nrow()
id_Obs_elections <- df_wide %>%
  pull(election_id)
id_Obs_departments <- df_wide %>%
  pull(department_id)
YVoteshare <- df_wide %>%
  select(!contains("id")) %>%
  as.matrix()
NMiss <- apply(YVoteshare, 1, function(x){sum(as.logical(x != 0))})
miss <- matrix(0, nrow = NObs, ncol = NBlocs)
for (j in 1:NObs){
  miss[j,1:NMiss[j]] <- seq(1, NBlocs)[YVoteshare[j,] != 0]
}


NParticipated <- length(participated)
NBlocs_Elections <- df_wide %>%
  select(-department_id) %>%
  pivot_longer(c(-election_id),
               names_to = "bloc_id",
               values_to = "participating") %>%
  mutate(participating = as.integer(participating == 0)) %>%
  group_by(election_id) %>%
  summarize(N = mean(participating)) %>%
  mutate(N = round(length(bloc_vector) - N * length(bloc_vector))) %>%
  pull(N)
tmp0 <- df_wide %>%
  select(-department_id) %>%
  pivot_longer(c(-election_id),
               names_to = "bloc_id",
               values_to = "participating") %>%
  filter(participating != 0) %>%
  distinct(election_id, bloc_id) %>%
  mutate(z = 1) %>%
  pivot_wider(id_cols = c(election_id),
              names_from = bloc_id,
              values_from = z,
              values_fill = 0) %>%
  select(-election_id) %>%
  as.matrix()
included_blocs <- matrix(0, nrow = NElections, ncol = NBlocs)
for (j in 1:NElections){
  tmp1 <- seq(1,length(bloc_vector))[as.logical(tmp0[j, ])]
  included_blocs[j, 1:length(tmp1)] <- tmp1
}
cutpoint_priors_normalized <- array(0, dim = dim(cutpoint_priors))
for (j in 1:NElections){
  cutpoint_priors_normalized[j,included_blocs[j, 1:NBlocs_Elections[j]]] <- cutpoint_priors[j,included_blocs[j, 1:NBlocs_Elections[j]]]/sum(cutpoint_priors[j,included_blocs[j, 1:NBlocs_Elections[j]]])
}
apply(cutpoint_priors[c(1,3,5),],2,mean)


###############################################################################
## Datalist
data_list <- list(
  NObs = NObs,
  NBlocs = NBlocs,
  NElections = NElections,
  NDepartments = NDepartments,
  id_Obs_elections = id_Obs_elections,
  id_Obs_departments = id_Obs_departments,
  YVoteshare = round(YVoteshare * 10000),
  NMiss = NMiss,
  miss = miss,

  lag_YVoteshare_national = apply(cutpoint_priors[c(1,3,5),],2,mean),

  NBlocs_Elections = NBlocs_Elections,
  included_blocs = included_blocs,

  XDepartment = XDepartments,
  K = K,
  incumbency = XNation[,1] + 1,
  XNation = XNation,
  M = M,
  NMiss_X = NMiss_X,
  id_X_miss = id_X_miss,

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
  n_approval = n_approval
)
data_list$XNation <- matrix(1, nrow = 6, ncol = 1)
###############################################################################
## Model
#' Load
mod <- cmdstan_model("src/stan/models_fundamentals/ordinal/linear_prediction.stan")
#' Fit
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 300,
  iter_warmup = 300,
  parallel_chains = 6,
  refresh = 100,
  init = 0.2
)
###############################################################################
fit$summary(c("bias_error_y_ppc", "bias_error_pred"))
fit$summary("y_star") %>%
  add_column(election_id = election_years[data_list$id_Obs_elections]) %>%
  ggplot(aes(x = mean)) +
    geom_histogram() +
    facet_wrap(election_id ~ .)

fit$summary("error_pred")



fit$summary("c", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(election_id = as.integer(str_match(variable, "\\[([\\d]+)")[, 2]),
         party_id = as.integer(str_match(variable, "([\\d]+)\\]")[, 2])
         ) %>%
  filter(`50%` < 15) %>%
  ggplot(aes(x = party_id, y = `50%`)) +
    geom_point() +
    facet_wrap(election_id ~ .)
mcmc_pairs(fit$draws("c"))
###############################################################################
## PPC
bayesplot::mcmc_pairs(fit$draws("b"))
bayesplot::ppc_dens_overlay(c(data_list$YVoteshare), fit$draws("y_ppc") %>%
                      posterior::as_draws_df() %>%
                      filter(1:n() < 100) %>%
                      select(contains("y_ppc")) %>%
                      as.matrix())

y_pred <- fit$summary("y_ppc",
                          ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(
    obs_id = as.integer(str_match(variable, "\\[([\\d]+)")[, 2]),
    party_id = bloc_vector[as.integer(str_match(variable, "([\\d]+)\\]")[, 2])],
    election_id = election_years[data_list$id_Obs_elections[obs_id]],
    department_id = data_list$id_Obs_departments[obs_id]
  ) %>%
  add_column(true = c(data_list$YVoteshare))
ggplot(y_pred, aes(x = true, y = `50%`, color = party_id)) +
  geom_point() +
  geom_abline(aes(intercept = 0, slope = 1)) +
  facet_wrap(election_id ~ .) +
  scale_color_manual(values = cols)

cols <- c("Gauche radicale et extreme gauche" = "brown4",
          "Gauche" ="brown2",
          "Ecologisme" = "limegreen",
          "Centre" ="gold",
          "Droite" ="blue",
          "Droite radicale et extreme droite" ="navyblue")



ppc_dens_overlay_grouped(c(data_list$YVoteshare), fit$draws("y_ppc") %>%
                           posterior::as_draws_df() %>%
                           filter(1:n() < 100) %>%
                           select(contains("y_ppc")) %>%
                           as.matrix(),
                         rep(data_list$id_Obs_elections, 6))
mcmc_pairs(fit$draws(c("b_national", "b_department")))




