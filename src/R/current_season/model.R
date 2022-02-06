################################################################################
## Title: Prepare polls for election season model
################################################################################
## Empty environment
rm(list = ls())
################################################################################
## Load libraries
library(tidyverse)
library(cmdstanr)
################################################################################
## Vectors
bloc_vector <- c("Abstention",
                 "Autre",
                 "Gauche radicale et extreme gauche",
                 "Gauche",
                 "Ecologisme",
                 "Centre",
                 "Droite",
                 "Droite radicale et extreme droite")
cols <- c("Autre" = "brown",
          "Abstention" = "grey",
          "Gauche radicale et extreme gauche" = "brown4",
          "Gauche" ="brown2",
          "Ecologisme" = "limegreen",
          "Centre" ="gold",
          "Droite" ="blue",
          "Droite radicale et extreme droite" ="navyblue")
count_weeks <- 18
################################################################################
## Data
df <- read_csv(file = "dta/polls_dta/election_season_model/poll_input.csv")
df_current <- read_csv(file = "dta/polls_dta/election_season_model/poll_input_current.csv")
df_le_pen_zemmour <- read_csv(
  file = "dta/polls_dta/election_season_model/poll_input_le_pen_zemmour.csv")

election_results <- read_csv("dta/polls_dta/election_results_clean.csv")
candidates_blocs <- read_csv("dta/france_classification/candidate_bloc_cross_walk.csv")

################################################################################
election_results <- election_results %>%
  select(-party, -bloc) %>%
  left_join(candidates_blocs %>%
              distinct(bloc, party,
                       candidate, election_year),
            by = c("candidate" = "candidate",
                   "year" = "election_year"))
election_results_all <- election_results %>%
  group_by(year, bloc) %>%
  mutate(bloc = ifelse(percentage == max(percentage), bloc, "Autre"),
         bloc = ifelse(candidate == "_Abstention", "Abstention", bloc)) %>%
  summarize(percentage = sum(percentage))
election_results <- election_results_all %>%
  filter(year %in% c(2002, 2007, 2012, 2017))
result_matrix <- election_results %>%
  mutate(bloc_id = match(bloc, bloc_vector)) %>%
  select(-bloc) %>%
  pivot_wider(id_cols = year,
              names_from = bloc_id,
              values_from = percentage,
              values_fill = 0.0) %>%
  ungroup() %>%
  select(-year)
result_matrix <- result_matrix[, order(colnames(result_matrix))]
result_abstention <- result_matrix$`1`
result_matrix <- result_matrix[,2:8]
miss01_results <- ifelse(as.matrix(result_matrix) != 0,1, 0)
miss_results <- array(0, dim = dim(miss01_results))
for (j in 1:nrow(miss_results)){
  miss_results[j, 1:sum(miss01_results[j, ])] <- seq(1, ncol(miss_results))[as.logical(miss01_results[j, ])]
}
miss_results <- miss_results %>%
  rbind(c(1,2,3,4, 5,6,7))
NMiss_results <- apply(miss_results, 1, function(x) sum(as.integer(x != 0)))

################################################################################
## Historical averages
m2_df_election_results <- read_csv(file = "dta/election_results/department_candidate_votes_blocs_clean.csv") %>%
  filter(departement != "Mayotte")
###############################################################################
#' Mangle data
m2_df_results <- m2_df_election_results %>%
  filter(year < 2022) %>%
  group_by(year, bloc) %>%
  filter(!is.na(votes), !is.na(percentage)) %>%
  summarize(percentage = sum(votes * percentage / votes)) %>%
  group_by(year) %>%
  mutate(percentage = percentage/sum(percentage)) %>%
  mutate(bloc_id = match(bloc, bloc_vector))
m2_df <- m2_df_results %>%
  select(-bloc) %>%
  pivot_wider(id_cols = year,
              names_from = bloc_id,
              values_from = percentage,
              values_fill = 0) %>%
  ungroup()
m2_year <- m2_df$year
m2_df <- m2_df %>%
  select(-year)
m2_df <- m2_df[, order(colnames(m2_df))]
m2_df <- m2_df[, 2:8]
m2_miss01 <- ifelse(as.matrix(m2_df) != 0,1, 0)
m2_miss <- array(0, dim = dim(m2_miss01))
for (j in 1:nrow(m2_miss)){
  m2_miss[j, 1:sum(m2_miss01[j,])] <- seq(1, ncol(m2_miss))[as.logical(m2_miss01[j, ])]
}
m2_miss <- rbind(m2_miss, c(1, 2, 3, 4, 5, 6, 7))
m2_NMiss <- apply(m2_miss, 1, function(x) sum(as.integer(x != 0)))
################################################################################
df <- df %>%
  mutate(bloc_id = match(bloc, bloc_vector),
         pollName = ifelse(grepl("Sofres", pollName), "Sofres",
                    ifelse(grepl("Harris", pollName), "Harris", pollName))) %>%
  arrange(election_id) %>%
  group_by(pollName) %>%
  mutate(pollster_id = cur_group_id(),
         pollster_id = 100 * election_id + pollster_id) %>%
  ungroup() %>%
  mutate(pollster_id = as.integer(factor(pollster_id))) %>%
  ungroup()
pollster_vector <- df %>%
  distinct(pollName, election_year, pollster_id)
df <- df %>%
  select(t, election_id, bloc_id, y,poll_id,pollster_id) %>%
  group_by(poll_id) %>%
  pivot_wider(id_cols = c(t, election_id, poll_id,pollster_id),
              names_from = bloc_id,
              values_from = y,
              values_fill = 0) %>%
  ungroup() %>%
  arrange(election_id, pollster_id)

df <- df[, order(colnames(df))]
df <- df %>% mutate(`8` = ifelse(`8` == 0, 1, `8`))
results <- df[, grepl("\\d",colnames(df))]
abstention_share <- results[,1]/apply(results, 1, sum)
results <- results[,2:8]
miss <- array(0, dim = dim(results))
tmp <- miss01_results %>% rbind(c(1,1,1,1,1,1,1))
for (j in 1:nrow(miss)){
  miss[j, 1:sum(tmp[df$election_id[j],])] <-
    seq(1, ncol(tmp))[as.logical(tmp[df$election_id[j],])]
}
NMiss <- apply(miss, 1, function(x) sum(as.integer(x != 0)))
indicators <- expand.grid(t1 = 1:count_weeks, t2 = seq(0, 5 * 52 * 4, 5 * 52)) %>%
  mutate(i = 1:n(),
         t_long = t1 + t2,
         t2 = 1 + t2 / (5 * 52))
df <- df %>%
  left_join(indicators,
            by = c("t" = "t1",
                   "election_id" = "t2"))
miss_pred <-  miss[sort(rep(match(c(1, 2, 3, 4, 5), df$election_id), count_weeks)), ]
NMiss_pred <- c(NMiss[sort(rep(match(c(1, 2, 3, 4, 5), df$election_id), count_weeks))])
################################################################################
## Le Pen v Zemmour
df_le_pen_zemmour <- df_le_pen_zemmour %>%
  group_by(poll_id) %>%
  mutate(n = sum(y)) %>%
  ungroup() %>%
  filter(candidate == "Le Pen")
m1_N_ex_droite <- nrow(df_le_pen_zemmour)
m1_ix_t_ex_droite <- df_le_pen_zemmour %>% pull(t)
m1_n_ex_droite <- df_le_pen_zemmour %>% pull(n)
m1_y_ex_droite <- df_le_pen_zemmour %>% pull(y)
################################################################################
m3_election_years <- election_years <- c(1988, 1995, 2002, 2007, 2012, 2017, 2022)
m3_df <- read_csv(file = "dta/election_results/department_candidate_votes_blocs_clean.csv") %>%
  filter(year %in% m3_election_years,
         departement != "Mayotte") %>%
  filter(!(bloc %in% c("Abstention")))
m3_df_election_results <- read_csv(file = "dta/election_results/department_candidate_votes_blocs_clean.csv") %>%
  filter(departement != "Mayotte")
m3_df_incumbency <- read_csv("dta/fundamentals_dta/cleaned_input/incumbency.csv")
m3_df_input_department <-
  read_csv("dta/fundamentals_dta/cleaned_input/input_data.csv")
m3_df_approval <- read_csv("dta/fundamentals_dta/approval.csv")
## Election year and departement id
m3_department_vector <- m3_df %>%
  distinct(departement) %>%
  pull(departement) %>%
  sort()
###############################################################################
## Mangle incumbency
m3_national_incumbency <- read_csv("dta/fundamentals_dta/cleaned_input/incumbency.csv") %>%
  mutate(bloc_id = match(incumbent_bloc, bloc_vector)) %>%
  select(bloc_id, election_year) %>%
  filter(election_year %in% m3_election_years) %>%
  mutate(i = 1) %>%
  pivot_wider(id_cols = election_year,
              names_from = bloc_id,
              values_from = i,
              values_fill = 0) %>%
  select(-election_year)
###############################################################################
## Mangle approval
m3_df_approval <- m3_df_approval %>%
  mutate(election_year = lubridate::year(date_next_first_round)#,
         #president = str_replace_all(president, "\\d", "")
  ) %>%
  filter(election_year %in% m3_election_years) %>%
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

m3_NPolls <- m3_df_approval %>%
  nrow()
m3_NPollsters <- m3_df_approval %>%
  distinct(pollster_id) %>%
  nrow()
m3_NPresidents <- m3_df_approval %>%
  distinct(president_id) %>%
  nrow()
m3_NPollsters_Presidents <- m3_df_approval %>%
  distinct(president_id, pollster_president_id) %>%
  group_by(president_id) %>%
  summarize(N = n()) %>%
  pull(N)
m3_NPolls_Presidents <- m3_df_approval %>%
  group_by(president_id) %>%
  summarize(N = n()) %>%
  pull(N)
m3_NTime <- max(m3_df_approval$time_id)
m3_id_Polls_time <- m3_df_approval %>%
  mutate(t = time_id + (president_id - 1) * m3_NTime) %>%
  pull(t)
m3_NTime_max <- m3_df_approval %>%
  mutate(t = time_id + (president_id - 1) * m3_NTime) %>%
  filter(election_year == 2022) %>%
  pull(time_id) %>%
  max()

m3_id_Polls_president <- m3_df_approval %>%
  pull(president_id)
m3_id_Polls_pollster <- m3_df_approval %>%
  pull(pollster_id)
m3_id_Polls_pollster_president <- m3_df_approval %>%
  pull(pollster_president_id)
m3_y_approval <- m3_df_approval %>%
  mutate(y = floor(p_approve * N)) %>%
  pull(y)
m3_n_approval <- m3_df_approval %>%
  pull(N)
###############################################################################
## National predictors
m3_XNation <- cbind(m3_national_incumbency[, 1])
m3_K <- ncol(m3_XNation)
m3_incumbency <- cbind(m3_national_incumbency, `3` = 0, `5` = 0, `8` = 0)
m3_incumbency <- m3_incumbency[, order(colnames(m3_incumbency))]
###############################################################################
## Mangle df
#' Complete
#' * The Green bloc supported the Left in
#' 2017 so no Green party challenger existed
m3_df <- m3_df %>%
  full_join(
    m3_df %>%
      distinct(bloc, departement) %>%
      full_join(expand.grid(m3_election_years,bloc_vector[2:8]) %>%
                  rename(year = Var1,
                         bloc = Var2),
                by = c("bloc" = "bloc"))
  ) %>%
  mutate(percentage = ifelse(is.na(percentage), 0, percentage),
         votes = ifelse(is.na(votes), 0, votes)) %>%
  group_by(year, departement) %>%
  mutate(percentage = votes/sum(votes)) %>%
  ungroup()

#' Add ids
m3_df <- m3_df %>%
  mutate(department_id = match(departement, m3_department_vector),
         election_id = match(year, m3_election_years),
         bloc_id = match(bloc, bloc_vector))

#' Arrange and widen
m3_df_wide <- m3_df %>%
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
m3_df_wide[is.na(m3_df_wide)] <- -1
###############################################################################
## Mangle department level input
m3_unemployment <- m3_df_input_department %>%
  mutate(election_id = match(election_year, m3_election_years),
         department_id = match(departement, m3_department_vector)) %>%
  arrange(election_id, department_id) %>%
  select(q3)
m3_lagged_share <- m3_df_election_results %>%
  select(-votes) %>%
  pivot_wider(id_cols = c(year, departement),
              names_from = bloc,
              values_from = percentage,
              values_fill = 0) %>%
  select(-Abstention) %>%
  bind_rows(data.frame(departement = unique(m3_df_election_results$departement),
                       year = 2022)) %>%
  arrange(year) %>%
  group_by(departement) %>%
  filter(lead(year) %in% election_years) %>%
  mutate(election_id = match(lead(year), m3_election_years),
         department_id = match(departement, m3_department_vector)) %>%
  arrange(election_id, department_id) %>%
  ungroup()

m3_std_log_unemployment <- (log(m3_unemployment) - mean(log(m3_unemployment[!is.na(m3_unemployment)])))/sd(log(m3_unemployment[!is.na(m3_unemployment)]))
m3_XDepartments <- cbind(m3_std_log_unemployment)
m3_M <- ncol(m3_XDepartments)
m3_XDepartments[is.na(m3_XDepartments)] <- -99
m3_NMiss_X <- as.integer(sum(m3_XDepartments[,1] == -99))
m3_id_X_miss <- seq(1:nrow(m3_XDepartments))[m3_XDepartments[,1] == -99]
################################################################################
## Weights
m3_w <- m3_df %>%
  filter(year == 2017, !(bloc %in% c("Abstention"))) %>%
  group_by(department_id) %>%
  summarize(votes = sum(votes)) %>%
  ungroup() %>%
  mutate(w = votes/sum(votes)) %>%
  arrange(department_id) %>%
  pull(w)
################################################################################
## Main contender
tmp <- m2_df_results %>%
  filter(bloc != "Autre", bloc != "Abstention") %>%
  group_by(bloc) %>%
  arrange(year) %>%
  mutate(lead_year = lead(year),
         lead_year = ifelse(is.na(lead_year), 2022, lead_year)) %>%
  filter(lead_year %in% election_years) %>%
  ungroup() %>%
  select(percentage, bloc_id, lead_year) %>%
  pivot_wider(id_cols = lead_year,
              names_from = bloc_id,
              values_from = percentage) %>%
  select(-lead_year)
tmp <- tmp[, order(colnames(tmp))]
tmp[is.na(tmp)] <- 0
main_contender <- array(NA, dim = dim(tmp))
for (j in 1:nrow(tmp)){
  for (i in 1:ncol(tmp)){
    main_contender[j, i] <- as.integer(sum(as.numeric(tmp[j, i]) > c(tmp[j, -i])) > 2)
  }
}
################################################################################
m3_NObs <- m3_df_wide %>%
  nrow()
m3_NBlocs <- m3_df_wide %>%
  select(!contains("id")) %>%
  ncol()
m3_NElections <- m3_df_wide %>%
  distinct(election_id) %>%
  nrow()
m3_NDepartments <- m3_df_wide %>%
  distinct(department_id) %>%
  nrow()
m3_id_Obs_elections <- m3_df_wide %>%
  pull(election_id)
m3_id_Obs_departments <- m3_df_wide %>%
  pull(department_id)
m3_YVoteshare <- m3_df_wide %>%
  select(!contains("id")) %>%
  as.matrix()
m3_NMiss <- apply(m3_YVoteshare, 1, function(x){sum(as.logical(x != 0))})
m3_miss <- matrix(0, nrow = m3_NObs, ncol = m3_NBlocs)
for (j in 1:m3_NObs){
  m3_miss[j,1:m3_NMiss[j]] <- seq(1, m3_NBlocs)[m3_YVoteshare[j,] != 0]
}




################################################################################
## datalist
data_list <- list(
  m1_N = nrow(results),
  m1_NBlocs = ncol(results),
  m1_T1 = count_weeks,
  m1_T2 = 5,
  m1_t1 = seq(1, count_weeks),
  m1_ix_time = df$i,
  m1_ix_election = df$election_id,
  m1_count_weeks = count_weeks,
  m1_ix_week = df$t,
  m1_abstention_share = abstention_share[,1],
  m1_abstention_share_included = as.integer(abstention_share[,1] > 0),
  m1_y_results = t(round(1000 * result_matrix)),
  m1_ix_results = indicators %>% filter(t1 == count_weeks) %>% pull(i),
  m1_NMiss_results = NMiss_results,
  m1_miss_results = miss_results,
  m1_NPollsters = df %>% distinct(election_id, pollster_id) %>%
    group_by(election_id) %>% summarize(n = n()) %>%
    pull(n),
  m1_ix_pollster = df %>% pull(pollster_id),
  m1_NPolls_Pollster = df %>% group_by(election_id, pollster_id) %>% summarize(n = n()) %>% pull(n),
  m1_y = t(results),
  m1_NMiss = NMiss,
  m1_miss = miss,
  m1_NMiss_pred = NMiss_pred,
  m1_miss_pred = miss_pred,
  m1_election = indicators$t2,

  m1_N_ex_droite = m1_N_ex_droite,
  m1_ix_t_ex_droite = m1_ix_t_ex_droite,
  m1_n_ex_droite = m1_n_ex_droite,
  m1_y_ex_droite = m1_y_ex_droite,

  m2_N1 = length(m2_year),
  m2_N2 = 1,
  m2_x1 = m2_year,
  m2_x2 = array(2022),
  m2_y = m2_df,
  m2_prior_sigma_quality = 0.15,
  m2_NMiss = m2_NMiss,
  m2_miss = m2_miss,

  m3_NObs = m3_NObs,
  m3_NBlocs = m3_NBlocs,
  m3_NElections = m3_NElections,
  m3_NDepartments = m3_NDepartments,
  m3_id_Obs_elections = m3_id_Obs_elections,
  m3_id_Obs_departments = m3_id_Obs_departments,
  m3_YVoteshare = m3_YVoteshare,
  m3_NMiss = m3_NMiss,
  m3_miss = m3_miss,
  m3_main_contender = main_contender,
  m3_XDepartment = m3_XDepartments,
  m3_K = m3_K,
  m3_incumbency = m3_incumbency,
  m3_XNation = m3_XNation,
  m3_M = m3_M,
  m3_NMiss_X = m3_NMiss_X,
  m3_id_X_miss = m3_id_X_miss,

  m3_NPolls = m3_NPolls,
  m3_NPollsters = m3_NPollsters,
  m3_NPresidents = m3_NPresidents,
  m3_NPollsters_Presidents = m3_NPollsters_Presidents,
  m3_NPolls_Presidents = m3_NPolls_Presidents,
  m3_NTime = m3_NTime,
  m3_id_Polls_time = m3_id_Polls_time,
  m3_id_Polls_pollster = m3_id_Polls_pollster,
  m3_id_Polls_president = m3_id_Polls_president,
  m3_id_Polls_pollster_president = m3_id_Polls_pollster_president,
  m3_y_approval = m3_y_approval,
  m3_n_approval = m3_n_approval,
  m3_w = m3_w,
  m3_NTime_max = m3_NTime_max
)
################################################################################
## Model
mod <- cmdstan_model("src/stan/models_joint/model_w_fundamentals.stan")
################################################################################
## Run
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 600,
  iter_warmup = 600,
  parallel_chains = 6,
  refresh = 100,
  init = 0.01
)
################################################################################
## Plot
fit$save_object("dta/fit/m2022.Rds")
write_rds(data_list, "dta/fit/m2022_data_list.Rds")
write_rds(indicators, "dta/fit/m2022_indicator.Rds")
write_rds(df, "dta/fit/m2022_polls.rds")
write_rds(election_results_all, "dta/fit/m2022_election_results_all.rds")
write_rds(pollster_vector, "dta/fit/m2022_pollster_vector.rds")
write_rds(m2_year, "dta/fit/m2022_m2_year.rds")
write_rds(m2_df_results, "dta/fit/m2022_m2_df_results.rds")
fit <- read_rds("dta/fit/m2022.Rds")
fit_summary <- fit$summary("m1_y2")
post_pred <- lapply(1:data_list$m1_NBlocs, function(ii){
  data.frame(t1 = indicators$t1,
             t2 = indicators$t2,
             pred_mu = fit_summary %>%
               filter(grepl(paste(ii, "\\]", sep = ""), variable)) %>%
               pull(mean)) %>%
    mutate(d = ii) %>%
    return(.)
}) %>%
  do.call("bind_rows", .) %>%
  mutate(bloc = factor(bloc_vector[d + 1], bloc_vector),
         year = c(2002, 2007, 2012, 2017, 2022)[t2])  %>%
  as.data.frame()
plt_df_rt_melt = fit$draws("m1_y2") %>%
  posterior::as_draws_df() %>%
  select(!contains(".")) %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = c("n", "d"),
               values_to = "draws",
               names_pattern = "([\\d]+),([\\d]+)") %>%
  mutate(n = as.integer(n),
         bloc = factor(bloc_vector[as.integer(d) + 1], levels = bloc_vector)) %>%
  left_join(data.frame(
    n = 1:nrow(indicators),
    t1 = indicators$t1,
    t2 = indicators$t2
  )) %>%
  mutate(
    year = c(2002, 2007, 2012, 2017, 2022)[t2]
  )


polls <- df %>%
  select(-`1`) %>%
  pivot_longer(c(-election_id, -poll_id, -t_long, -t, -i, -pollster_id),
               names_to = "d",
               values_to = "prob") %>%
  rename(t1 = t,
         t2 = election_id) %>%
  filter(prob != 0) %>%
  mutate(bloc = factor(bloc_vector[as.integer(d)], levels = bloc_vector),
         year = c(2002, 2007, 2012, 2017,2022)[t2])

election_results_all <- election_results_all %>%
  mutate(d = match(bloc, bloc_vector),
         bloc = factor(bloc, levels = bloc_vector),
         t2 = match(year, c(2002, 2007, 2012, 2017))) %>%
  filter(year %in% c(2002, 2007, 2012, 2017)) %>%
  filter(bloc != "Abstention") %>%
  group_by(year) %>%
  mutate(percentage = percentage/sum(percentage))

prediction <- fit$summary("prediction", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(
    bloc = 1 + as.integer(str_match(variable, "(\\d)")[,2]),
    bloc = factor(bloc_vector[bloc], levels = bloc_vector)
  ) %>%
  mutate(t1 = count_weeks, year = 2022)

p <- ggplot() +
  geom_line(data = plt_df_rt_melt %>%
              filter(iter %in% sample(1:1800, 100)), aes(x = t1, y = draws,
                                                         group = iter,
                                                         colour = 'Posterior mean functions'), alpha = 0.1) +
  geom_line(data = post_pred,
            aes(x = t1, y = pred_mu,
                colour = 'Posterior mean function')) +
  geom_point(data = polls %>%
               group_by(poll_id) %>%
               mutate(prob = prob/sum(prob)), aes(x = t1, y = prob)) +
  geom_point(data = prediction, aes(x = t1, y = `50%`), color = "black") +
  geom_errorbar(data = prediction, aes(x = t1, ymin = `25%`, ymax = `75%`),
             color = "black", size = 0.5, width = 0) +
  geom_errorbar(data = prediction, aes(x = t1, ymin = `10%`, ymax = `90%`),
                color = "black", size = 0.25, width = 0) +
  geom_point(data = prediction, aes(x = t1, y = `50%`), color = "black") +
  theme_bw() + theme(legend.position="bottom") +
  geom_hline(data = election_results_all, aes(yintercept = percentage)) +
  xlab('X') +
  ylab('y') +
  facet_grid(bloc ~ year)
p
###############################################################################
##
fit_summary <- fit$summary("m2_y2")
post_pred <- lapply(1:data_list$m1_NBlocs, function(ii){
  data.frame(x = c(m2_year, 2022),
             pred_mu = fit_summary %>%
               filter(grepl(paste(ii, "\\]", sep = ""), variable)) %>%
               pull(mean)) %>%
    mutate(d = ii) %>%
    return(.)
}) %>%
  do.call("bind_rows", .)
plt_df_rt_melt = fit$draws("m2_y2") %>%
  posterior::as_draws_df() %>%
  select(!contains(".")) %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = c("n", "d"),
               values_to = "draws",
               names_pattern = "([\\d]+),([\\d]+)") %>%
  mutate(n = as.integer(n),
         d = as.integer(d)) %>%
  left_join(data.frame(
    n = 1:(length(m2_year) + 1),
    x2 = c(m2_year, 2022)
  ))

plt_df_rt_melt <- plt_df_rt_melt %>%
  mutate(bloc = factor(bloc_vector[d + 1], levels = bloc_vector))
post_pred <- post_pred %>%
  mutate(bloc = factor(bloc_vector[d + 1], levels = bloc_vector))
m2_df_results <- m2_df_results %>%
  filter(bloc != "Abstention") %>%
  group_by(year) %>%
  mutate(percentage = percentage/sum(percentage)) %>%
  mutate(bloc = factor(bloc, bloc_vector))


prediction <- fit$summary("prediction", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(
    d = as.integer(str_match(variable, "(\\d)")[,2]) + 1,
    bloc = factor(bloc_vector[d], levels = bloc_vector)
  ) %>%
  mutate(year = 2022)

p <- ggplot() +
  geom_line(data = plt_df_rt_melt %>%
              filter(iter %in% sample(1:1800, 400)), aes(x = x2, y = draws,
                                                         group = iter,
                                                         colour = bloc), alpha = 0.05) +
  geom_line(data = post_pred, aes(x = x, y = pred_mu), colour = "black", linetype = 2) +
  geom_point(data = m2_df_results, aes(x = year, y = percentage), color = "black") +
  geom_point(data = prediction, aes(x = year, y = `50%`), color = "black") +
  geom_errorbar(data = prediction, aes(x = year, ymin = `25%`, ymax = `75%`),
                color = "black", size = 0.5, width = 0) +
  geom_errorbar(data = prediction, aes(x = year, ymin = `10%`, ymax = `90%`),
                color = "black", size = 0.25, width = 0) +
  theme_bw() + theme(legend.position="bottom") +
  xlab('X') +
  ylab('y') +
  facet_wrap(bloc ~ .) +
  theme(legend.position = "none") +
  scale_color_manual(values = cols)
p
################################################################################
## Run-off prediction
prediction <- fit$draws("prediction_adjusted") %>%
  posterior::as_draws_df()
out <- matrix(NA, 7, 7)
for (j in 2:7){
  for (i in (j + 1):8){
    tmp <- matrix(NA, nrow = nrow(prediction), ncol = 5)
    count <- 0
    print(c(i, j))
    print(seq(2,8)[c(-i, -j)])
    for (r in seq(2,8)[c(-(i - 1), -(j - 1))]){
      count <- count + 1
      tmp[, count] <- (prediction[,j] > prediction[,r]) & (prediction[,i] > prediction[,r])
    }
    out[j - 1, i - 1] <- mean(apply(tmp, 1, all))
    out[i - 1, j - 1] <- mean(apply(tmp, 1, all))
  }
}
main_candidates_list <- c("Melenchon", "Hidalgo", "Jadot", "Macron", "Pecresse", "Le Pen",
                          "Zemmour")

runoff_prob <- data.frame(
  candidates = main_candidates_list,
  prob = out %>%
    apply(., 2, function(x) sum(x, na.rm = TRUE))
)
ggplot(data = runoff_prob, aes(x = candidates, y = prob)) +
  geom_point()
################################################################################
## Polling house effects
fit$draws("m1_mu_pollster") %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = "variable",
               values_to = "draws") %>%
  mutate(
    bloc_id = as.integer(str_match(variable, "(\\d),")[,2]),
    pollster_id = as.integer(str_match(variable, ",(\\d+)")[,2])
  ) %>%
  filter(!is.na(pollster_id), !is.na(draws)) %>%
  group_by(iter, pollster_id) %>%
  mutate(draws = exp(draws)/sum(exp(draws), na.rm = TRUE) -
           1/n()) %>%
  ungroup() %>%
  group_by(pollster_id, bloc_id) %>%
  summarize(
    q10 = quantile(draws, 0.1),
    q25 = quantile(draws, 0.25),
    q50 = quantile(draws, 0.5),
    q75 = quantile(draws, 0.75),
    q90 = quantile(draws, 0.9)
  ) %>%
  mutate(bloc = factor(bloc_vector[1 + bloc_id], bloc_vector)) %>%
  filter(bloc != "Autre") %>%
  arrange(bloc) %>%
  mutate(bloc = str_wrap(bloc, width = 20),
         bloc = factor(bloc, levels = bloc)) %>%
  left_join(pollster_vector) %>%
  ggplot(., aes(x = bloc, y = q50, color = as.factor(election_year))) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = q25, ymax = q75),
                width = 0, size = 0.5,
                position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = q10, ymax = q90),
                width = 0, size = 0.25,
                position = position_dodge(width = 0.5)) +
  facet_wrap(pollName ~.) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "bottom") +
    labs(y = "Pollster deviation from average")

################################################################################
fit$summary("m2_sigma_quality", ~quantile(. * 100, c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(bloc = factor(bloc_vector[1 + 1:n()], levels = bloc_vector)) %>%
  arrange(bloc) %>%
  mutate(bloc = str_wrap(bloc, width = 20),
         bloc = factor(bloc, levels = bloc)) %>%
  ggplot(aes(x = bloc, y = `50%`)) +
    geom_point() +
    geom_errorbar(aes(ymin = `25%`, ymax = `75%`), width = 0, size = 0.75) +
    geom_errorbar(aes(ymin = `10%`, ymax = `90%`), width = 0, size = 0.5) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "bottom") +
    labs(y = "Candidate variation (percentage)")



################################################################################
fit$summary("m1_Omega") %>%
  mutate(
    bloc1 = factor(
      bloc_vector[2 + as.integer(str_match(variable, "(\\d+),")[, 2])],
      levels = bloc_vector),
    bloc2 = factor(bloc_vector[2 + as.integer(str_match(variable, ",(\\d+)")[, 2])],
    levels = bloc_vector)
  ) %>%
  select(bloc1, bloc2, median) %>%
  ggplot(aes(x = bloc1, y = bloc2, fill = median)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)



################################################################################
y_rep <- fit$summary("y_rep") %>%
  mutate(
    d = as.integer(str_match(variable, "([\\d]+),")[, 2]),
    i = as.integer(str_match(variable, ",([\\d]+)")[, 2]),
  ) %>%
  select(d, i, mean)
y <- data_list$y %>%
  as.data.frame() %>%
  mutate(d = 1:n()) %>%
  pivot_longer(c(-d), names_to = "i", values_to = "y", names_prefix = "V") %>%
  mutate(i = as.integer(i)) %>%
  group_by(i) %>%
  mutate(prob = y/sum(y),
         n = sum(y))

y_rep_y <- left_join(y, y_rep)
ggplot(y_rep_y, aes(x = prob, y = mean, color = n)) +
  geom_point()






