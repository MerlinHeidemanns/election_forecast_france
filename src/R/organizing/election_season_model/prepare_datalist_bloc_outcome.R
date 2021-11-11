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
df_incumbency <- read_csv("dta/fundamentals_dta/cleaned_input/incumbency.csv")
df_input_department <-
  read_csv("dta/fundamentals_dta/cleaned_input/input_data.csv")
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
  mutate(percentage = ifelse(is.na(percentage), 0.001, percentage),
         votes = ifelse(is.na(votes), 0, votes)) %>%
  group_by(departement, year) %>%
  mutate(percentage = percentage/sum(percentage)) %>%
  ungroup()

#' Add ids
df <- df %>%
  mutate(department_id = match(departement, department_vector),
         election_id = match(year, election_years),
         bloc_id = match(bloc, bloc_vector))

#' Arrange
df <- df %>%
  arrange(election_id, department_id, bloc_id) %>%
  select(election_id, department_id, bloc_id, percentage)
#' Widen
df_wide <- df %>%
  pivot_wider(id_cols = c(election_id, department_id),
              names_from = bloc_id,
              values_from = percentage) %>%
  arrange(election_id, department_id)
###############################################################################
## Mangle department level input
XDepartments <- df_input_department %>%
  mutate(election_id = match(election_year, election_years),
         department_id = match(departement, department_vector)) %>%
  arrange(election_id, department_id) %>%
  select(q2)
M <- ncol(XDepartments)
XDepartments[is.na(XDepartments)] <- -99
NMiss_X <- as.integer(apply(XDepartments == -99, 2, sum))
id_X_miss <- matrix(-99, ncol = M, nrow = max(NMiss_X))
for (j in 1:M){
  tmp <- seq(1:nrow(XDepartments))[XDepartments[,j] == -99]
  id_X_miss[1:length(tmp), j] <- tmp
}
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
  XDepartment = XDepartments,
  K = K,
  incumbency = XNation[,1] + 1,
  XNation = XNation,
  M = M,
  NMiss_X = NMiss_X %>% as.array(),
  id_X_miss = id_X_miss
)
###############################################################################
## Model
#' Load
mod <- cmdstan_model("src/stan/models_fundamentals/fundamentals_blocs_ordinal.stan")
#' Fit
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 500,
  iter_warmup = 500,
  parallel_chains = 6,
  refresh = 250
)
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
                         df %>% pull(bloc_id))
mcmc_pairs(fit$draws(c("b_national", "b_department")))
mcmc_pairs(fit$draws(c("b_department", "thresholds")))

###############################################################################
## PPC to save
source("src/R/functions/ppc_fundamentals_rmse.R")
fit_evaluation <- read_rds("dta/fundamentals_dta/fit_evaluations.Rds")
number <- 10
if (number > nrow(fit_evaluation$rmse_global)){
  description <- "Vary effect of unemployment by incumbent."
  model_code <- "N10"
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
fit_evaluation_test <- read_rds("dta/fundamentals_dta/fit_evaluations.Rds")
rmse <- fit_evaluation_test$rmse_global


ggplot(rmse, aes(x = model_number, y = q50)) +
  geom_point()









