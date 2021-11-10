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
  filter(year %in% election_years)
df_incumbency <-
###############################################################################
## Election year and departement id
department_vector <- df %>%
  distinct(departement) %>%
  pull(departement) %>%
  sort()
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
  YVoteshare = YVoteshare
)
###############################################################################
## Model
#' Load
mod <- cmdstan_model("src/stan/models_fundamentals/fundamentals_blocs.stan")
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
source("src/R/functions/ppc_fundamentals_rmse.R")
if (FALSE){
  fit_evaluation <- read_rds("dta/fundamentals_dta/fit_evaluations.Rds")
  number <- 1
  description <- "Base model with one value per bloc for all observations."
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
  fit_evaluation[["model_code"]][["N01"]] = mod$code()
  write_rds(fit_evaluation, "dta/fundamentals_dta/fit_evaluations.Rds")
}
###############################################################################