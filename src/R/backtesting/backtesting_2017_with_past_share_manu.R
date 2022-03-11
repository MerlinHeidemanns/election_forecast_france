################################################################################
## Title: Backtest model for 2017
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
this_year <- 2017
election_years <- c(2002, 2007, 2012, 2017)
election_years <- election_years[election_years < this_year]
participating_blocs <- c(1,2,3, 5,6,7, 0)
number_of_elections <- length(election_years) + 1
start_day <- as.Date("2016-12-18 ")
start_times <- c(start_day, start_day + seq(1, 17) * 7)

dta <- read_rds("2017_dta.rds")

for (start_indicator in seq(3, 17)){
  #start_indicator <- 10
  ################################################################################
  ## Data
  df <- dta$df
  df_current <- dta$df_current
  df_le_pen_zemmour <- dta$df_le_pen_zemmour
  election_results <- dta$election_results
  candidates_blocs <- dta$candidates_blocs

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
    filter(year %in% election_years)
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
    rbind(participating_blocs)
  NMiss_results <- apply(miss_results, 1, function(x) sum(as.integer(x != 0)))

  ################################################################################
  ## Historical averages
  m2_df_election_results <- dta$m2_df_election_results
  ###############################################################################
  #' Mangle data
  m2_df_results <- m2_df_election_results %>%
    filter(year < this_year) %>%
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
  m2_miss <- rbind(m2_miss, participating_blocs)
  m2_NMiss <- apply(m2_miss, 1, function(x) sum(as.integer(x != 0)))
  ################################################################################
  df <- df %>%
    filter(election_year <= this_year) %>%
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
    arrange(election_id, t, pollster_id)

  m1_NLimit <- df %>%
    mutate(n = 1:n()) %>%
    filter(election_id == 4, t == start_indicator) %>%
    pull(n) %>%
    max()


  df <- df[, order(colnames(df))]
  df <- df %>% mutate(`8` = ifelse(`8` == 0, 1, `8`))
  results <- df[, grepl("\\d",colnames(df))]
  abstention_share <- results[,1]/apply(results, 1, sum)
  results <- results[,2:8]
  miss <- array(0, dim = dim(results))
  tmp <- miss01_results %>% rbind(c(1,1,1,0,1,1,1))
  for (j in 1:nrow(miss)){
    miss[j, 1:sum(tmp[df$election_id[j],])] <-
      seq(1, ncol(tmp))[as.logical(tmp[df$election_id[j],])]
  }
  NMiss <- apply(miss, 1, function(x) sum(as.integer(x != 0)))
  indicators <- expand.grid(t1 = 1:count_weeks, t2 = seq(0, number_of_elections * 52 * 4, 5 * 52)) %>%
    mutate(i = 1:n(),
           t_long = t1 + t2,
           t2 = 1 + t2 / (5 * 52))
  df <- df %>%
    left_join(indicators,
              by = c("t" = "t1",
                     "election_id" = "t2"))
  miss_pred <-  miss[sort(rep(match(c(1, 2, 3, 4), df$election_id), count_weeks)), ]
  NMiss_pred <- c(NMiss[sort(rep(match(c(1, 2, 3, 4), df$election_id), count_weeks))])
  ################################################################################
  m3_election_years <- c(1988, 1995, 2002, 2007, 2012, 2017, 2022)
  m3_election_years <- m3_election_years[m3_election_years <= this_year]
  m3_df <- dta$m3_df
  m3_df_election_results <- dta$m3_df_election_results
  m3_df_incumbency <- dta$m3_df_incumbency
  m3_df_input_department <- dta$m3_df_input_department
  m3_df_approval <- dta$m3_df_approval
  ## Election year and departement id
  m3_department_vector <- m3_df %>%
    distinct(departement) %>%
    pull(departement) %>%
    sort()
  ###############################################################################
  ## Mangle incumbency
  m3_national_incumbency <- m3_df_incumbency %>%
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
           pollster, pollster_id, president_id, method_id, method, time_id, date) %>%
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
    filter(election_year == this_year) %>%
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

  m3_NInclusion <- sum(m3_df_approval$date < start_times[start_indicator])

  ###############################################################################
  ## National predictors
  m3_XNation <- cbind(m3_national_incumbency[, 1])
  m3_K <- ncol(m3_XNation)
  m3_incumbency <- cbind(m3_national_incumbency, `3` = 0, `5` = 0, `8` = 0, `6` = 0)
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
    filter(election_year %in% m3_election_years)
  if (start_indicator < 4){
    m3_unemployment <- m3_unemployment %>%
      select(q3)
  } else if (start_indicator < 12){
    m3_unemployment <- m3_unemployment %>%
      select(q2)
  } else {
    m3_unemployment <- m3_unemployment %>%
      select(q1)
  }
  m3_lagged_share <- m3_df_election_results %>%
    select(-votes) %>%
    pivot_wider(id_cols = c(year, departement),
                names_from = bloc,
                values_from = percentage,
                values_fill = 0) %>%
    select(-Abstention) %>%
    # bind_rows(data.frame(departement = unique(m3_df_election_results$departement),
    #                      year = 2012)) %>%
    arrange(year) %>%
    group_by(departement) %>%
    mutate(Centre =
             ifelse(Centre == 0,
                    lag(Centre), Centre),
           Ecologisme =
             ifelse(Ecologisme == 0,
                    lag(Ecologisme), Ecologisme)) %>%
    filter(lead(year) %in% m3_election_years) %>%
    mutate(election_id = match(lead(year), m3_election_years),
           department_id = match(departement, m3_department_vector)) %>%
    arrange(election_id, department_id) %>%
    ungroup() %>%
    select(-year, -departement)
  m3_lagged_share <- m3_lagged_share[, bloc_vector[3:8]]

  m3_std_log_unemployment <- (log(m3_unemployment) - mean(log(m3_unemployment[!is.na(m3_unemployment)])))/sd(log(m3_unemployment[!is.na(m3_unemployment)]))
  m3_XDepartments <- cbind(m3_std_log_unemployment)
  m3_M <- ncol(m3_XDepartments)
  m3_XDepartments[is.na(m3_XDepartments)] <- -99
  m3_NMiss_X <- as.integer(sum(m3_XDepartments[,1] == -99))
  m3_id_X_miss <- seq(1:nrow(m3_XDepartments))[m3_XDepartments[,1] == -99]
  ################################################################################
  ## Weights
  m3_w <- m3_df %>%
    filter(year == this_year - 5, !(bloc %in% c("Abstention"))) %>%
    group_by(department_id) %>%
    summarize(votes = sum(votes)) %>%
    ungroup() %>%
    mutate(w = votes/sum(votes)) %>%
    arrange(department_id) %>%
    pull(w)
  ################################################################################
  ## Main contender
  tmp <- m2_df_results %>%
    filter(bloc != "Autre", bloc != "Abstention", year < this_year) %>%
    group_by(bloc) %>%
    arrange(year) %>%
    mutate(lead_year = lead(year),
           lead_year = ifelse(is.na(lead_year), this_year, lead_year)) %>%
    filter(lead_year %in% m3_election_years) %>%
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
    m1_NLimit = m1_NLimit,
    m1_NBlocs = ncol(results),
    m1_T1 = count_weeks,
    m1_T2 = 4,
    m1_t1 = seq(1, count_weeks),
    m1_ix_time = df$i,
    m1_ix_election = df$election_id,
    m1_count_weeks = count_weeks,
    m1_ix_week = df$t,
    m1_abstention_share = abstention_share[,1],
    m1_abstention_share_included = as.integer(abstention_share[,1] > 0),
    m1_y_results = t(round(1000 * result_matrix)),
    m1_ix_results = c(indicators %>% filter(t1 == count_weeks) %>% pull(i))[1:4],
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

    m2_N1 = length(m2_year),
    m2_N2 = 1,
    m2_x1 = m2_year,
    m2_x2 = array(2017),
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
    m3_shares = m3_lagged_share,
    m3_K = m3_K,
    m3_incumbency = m3_incumbency,
    m3_XNation = m3_XNation,
    m3_M = m3_M,
    m3_NMiss_X = m3_NMiss_X,
    m3_id_X_miss = m3_id_X_miss,

    m3_NInclusion = m3_NInclusion,
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
  for (j in 1:length(data_list)){
    if (any(is.na(data_list[[j]]))){
      print(names(data_list)[j])
    }
  }
  ################################################################################
  ## Model
  mod <- cmdstan_model("model_w_fundamentals_wo_adjustment_wo_alpha_w_shares.stan")
  ################################################################################
  ## Run
  fit <- mod$sample(
    data = data_list,
    chains = 6,
    iter_sampling = 300,
    iter_warmup = 300,
    parallel_chains = 6,
    refresh = 100,
    init = 0
  )
  ################################################################################
  ## Poll model
  fit$save_object(paste0("m2017_w_shares_", start_indicator,".Rds", sep = ""))
}

