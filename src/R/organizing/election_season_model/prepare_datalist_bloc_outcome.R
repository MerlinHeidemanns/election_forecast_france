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
  select(lag_share_droite, lag_share_gauche) %>%
  mutate(
    lag_share_droite = (lag_share_droite - mean(lag_share_droite))/sd(lag_share_droite),
    lag_share_gauche = (lag_share_gauche - mean(lag_share_gauche))/sd(lag_share_gauche)
  )

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
participated <- seq(1,length(c(YVoteshare)))[c(YVoteshare) != 0]
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

###############################################################################
## Datalist
data_list <- list(
  NObs = NObs,
  NBlocs = NBlocs,
  NElections = NElections,
  NDepartments = NDepartments,
  id_Obs_elections = id_Obs_elections,
  id_Obs_departments = id_Obs_departments,
  YVoteshare = YVoteshare,

  lag_YVoteshare_national = cutpoint_priors_normalized,

  NBlocs_Elections = NBlocs_Elections,
  included_blocs = included_blocs,
  NParticipated = NParticipated,
  participated = participated,

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
###############################################################################
## Model
#' Load
mod <- cmdstan_model("src/stan/models_fundamentals/ordinal/ordinal.stan")
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




mcmc_pairs(fit$draws(c("y_star[1]", "theta[1,1]", "theta[1,2]", "c[1,1]", "c[1,2]")))
###############################################################################
## PPC
bayesplot::mcmc_pairs(fit$draws("b"))
bayesplot::ppc_dens_overlay(c(data_list$YVoteshare), fit$draws("hat_YVoteshare") %>%
                      posterior::as_draws_df() %>%
                      filter(1:n() < 100) %>%
                      select(contains("hat_YVot")) %>%
                      as.matrix())
ppc_dens_overlay_grouped(c(data_list$YVoteshare), fit$draws("hat_YVoteshare") %>%
                           posterior::as_draws_df() %>%
                           filter(1:n() < 100) %>%
                           select(contains("hat_YVot")) %>%
                           as.matrix(),
                         rep(data_list$id_Obs_elections, 5))
mcmc_pairs(fit$draws(c("b_national", "b_department")))
###############################################################################
## PPC
#' Party positions
ppc_party_positions_density <- function(fit, bloc_vector, data_list){
  ## Parameters
  mu <- fit$draws("mu_parties") %>%
    posterior::as_draws_df() %>%
    select(contains("mu")) %>%
    as.matrix()

  sigma <- fit$draws("sigma_parties") %>%
    posterior::as_draws_df() %>%
    select(contains("sigma")) %>%
    as.matrix()

  ## Densities and vote share
  out <- lapply(sample(1:1800, 250), function(jj){
    out0 <- lapply(seq(-3, 3, 0.2), function(qq){
      out1 <- lapply(1:5, function(ii){
        out2 <- data.frame(p = bloc_vector[ii],
                           prob = dnorm(qq, mu[jj,ii], sigma[jj, ii]),
                           mu = qq)
        return(out2)
      }) %>%
        do.call("bind_rows", .) %>%
        mutate(prob = prob/sum(prob))
      return(out1)
    }) %>%
      do.call("bind_rows", .)
    return(out0)
  }) %>%
    do.call("bind_rows", .)
  out <- out %>%
    group_by(mu, p) %>%
    summarize(
      q50 = quantile(prob, 0.5),
      q25 = quantile(prob, 0.25),
      q75 = quantile(prob, 0.75),
      q10 = quantile(prob, 0.1),
      q90 = quantile(prob, 0.9)
    )

  ## Density for latent outcome
  mu_pos <- fit$summary("mu", ~quantile(., 0.5))
  data_frame_outcome <- as.data.frame(data_list$YVoteshare)
  colnames(data_frame_outcome) <- bloc_vector
  mu_pos <- cbind(mu_pos, data_frame_outcome)
  mu_pos <- mu_pos %>%
    select(-variable) %>%
    pivot_longer(c(-`50%`),
                 names_to = "p",
                 values_to = "percentage") %>%
    rename(q50 = `50%`)

  ## Set colors
  cols <- c("Gauche radicale et extreme gauche" = "brown4",
            "Gauche" ="brown2",
            "Ecologisme" = "limegreen",
            "Centre" ="gold",
            "Droite" ="blue",
            "Droite radicale et extreme droite" ="navyblue")
  ## Plot
  plt <- ggplot(out, aes(x = mu, y = q50)) +
    geom_line(aes(color = p)) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = p), alpha = 0.3) +
    geom_ribbon(aes(ymin = q10, ymax = q90, fill = p), alpha = 0.15) +
    geom_histogram(data = mu_pos, aes(x = q50, after_stat(ndensity)/2), alpha = 0.3) +
    theme_light() +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    theme(legend.title = element_blank()) +
    labs(x = "Left-Right position of departement",
         y = "Expected voteshare")

  return(plt)
}
ppc_party_positions_density(fit, bloc_vector, data_list)
ppc_party_positions_density_multi <- function(fit, bloc_vector, data_list){
  ## Parameters
  mu <- fit$draws("mu_parties") %>%
    posterior::as_draws_df() %>%
    select(contains("mu")) %>%
    as.matrix()

  sigma <- fit$draws("sigma_parties") %>%
    posterior::as_draws_df() %>%
    select(contains("sigma")) %>%
    as.matrix()

  ## Densities and vote share
  out <- lapply(sample(1:1800, 250), function(jj){
    out0 <- lapply(seq(-3, 3, 0.2), function(qq){
      out1 <- lapply(1:6, function(kk){
        out2 <- lapply(1:5, function(ii){
          index_mu <- (ii - 1) * 6 + kk
          out3 <- data.frame(p = bloc_vector[ii],
                             prob = dnorm(qq, mu[jj,ii], sigma[jj, index_mu]),
                             mu = qq,
                             election = election_years[kk])
        }) %>%
          do.call("bind_rows", .) %>%
          mutate(prob = prob/sum(prob))
        return(out2)
      }) %>%
        do.call("bind_rows", .)
      return(out1)
    }) %>%
      do.call("bind_rows", .)
    return(out0)
  }) %>%
    do.call("bind_rows", .)
  out <- out %>%
    group_by(mu, p, election) %>%
    summarize(
      q50 = quantile(prob, 0.5),
      q25 = quantile(prob, 0.25),
      q75 = quantile(prob, 0.75),
      q10 = quantile(prob, 0.1),
      q90 = quantile(prob, 0.9)
    )

  ## Density for latent outcome
  mu_pos <- fit$summary("mu", ~quantile(., 0.5))
  data_frame_outcome <- as.data.frame(data_list$YVoteshare)
  colnames(data_frame_outcome) <- bloc_vector
  mu_pos <- cbind(mu_pos, data_frame_outcome) %>%
    add_column(election = election_years[data_list$id_Obs_elections])
  mu_pos <- mu_pos %>%
    select(-variable) %>%
    pivot_longer(c(-`50%`, -election),
                 names_to = "p",
                 values_to = "percentage") %>%
    rename(q50 = `50%`)

  ## Set colors
  cols <- c("Gauche radicale et extreme gauche" = "brown4",
            "Gauche" ="brown2",
            "Centre" ="gold",
            "Droite" ="blue",
            "Droite radicale et extreme droite" ="navyblue")
  ## Plot
  plt <- ggplot(out, aes(x = mu, y = q50)) +
    geom_line(aes(color = p)) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = p), alpha = 0.3) +
    geom_ribbon(aes(ymin = q10, ymax = q90, fill = p), alpha = 0.15) +
    geom_histogram(data = mu_pos, aes(x = q50, after_stat(ndensity)/2), alpha = 0.3) +
    theme_light() +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    theme(legend.title = element_blank()) +
    facet_wrap(election ~ .) +
    labs(x = "Left-Right position of departement",
         y = "Expected voteshare")

  return(plt)
}
ppc_party_positions_density_multi(fit, bloc_vector, data_list)
source("src/R/functions/ppc_fundamentals_observed_v_predicted.R")
ppc_fundamentals_observed_v_predicted(fit, df, bloc_vector, election_years)


###############################################################################
## PPC to save
source("src/R/functions/ppc_fundamentals_rmse.R")
fit_evaluation <- read_rds("dta/fundamentals_dta/fit_evaluations.Rds")
number <- 23
if (number > nrow(fit_evaluation$rmse_global)){
  description <- "Party position model without national predictors."
  model_code <- "N23"
  fit_evaluation[["rmse_global"]] <- bind_rows(
    fit_evaluation[["rmse_global"]],
    output_rmse_global(fit, number, description)
  )
  #' rmse by bloc and election
  fit_evaluation[["rmse_bloc_election"]] <- bind_rows(
    fit_evaluation[["rmse_bloc_election"]],
    output_rmse_bloc_election(
      fit, bloc_vector, election_years, df, number,
      description
    ))
  #' Error by department
  fit_evaluation[["error_department"]] <- bind_rows(
    fit_evaluation[["error_department"]],
    output_error_department(
    fit, bloc_vector, election_years, df, number,
    description
  ))
  fit_evaluation[["model_code"]][[model_code]] = mod$code()
  write_rds(fit_evaluation, "dta/fundamentals_dta/fit_evaluations.Rds")
}
###############################################################################
## Comparing fit evaluations
fit_evaluation_test <- read_rds("dta/fundamentals_dta/fit_evaluations.Rds")
rmse <- fit_evaluation_test$rmse_global


ggplot(rmse, aes(x = model_number, y = q50)) +
  geom_point()

ggplot(fit_evaluation_test$rmse_bloc_election %>%
         mutate(last = as.integer(max(model_number) == model_number)),
       aes(x = year, y = rmse,
                                                   color = as.factor(last),
                                                   group = model_number)) +
  geom_line() +
  facet_wrap(bloc ~ .)


ggplot(fit_evaluation_test$error_department %>%
         filter(model_number == max(model_number)),
       aes(x = percentage, y = q50, color = as.factor(year))) +
  geom_point() +
  facet_wrap(bloc ~.)




