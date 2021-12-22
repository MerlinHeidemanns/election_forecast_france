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
################################################################################
## Data
df <- read_csv(file = "dta/polls_dta/election_season_model/poll_input.csv")
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
  filter(year %in% c(2002, 2007, 2012))
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
miss01_results <- ifelse(as.matrix(result_matrix) != 0,1, 0)
miss_results <- array(0, dim = dim(miss01_results))
for (j in 1:nrow(miss_results)){
  miss_results[j, 1:sum(miss01_results[j, ])] <- seq(1, ncol(miss_results))[as.logical(miss01_results[j, ])]
}
miss_results <- miss_results %>%
  rbind(c(1,2,3,4,6,7,8,0))
NMiss_results <- apply(miss_results, 1, function(x) sum(as.integer(x != 0)))


################################################################################
df <- df %>%
  mutate(bloc_id = match(bloc, bloc_vector)) %>%
  mutate(pollName = ifelse(grepl("Sofres", pollName), "Sofres", pollName)) %>%
  arrange(election_id) %>%
  filter(t < 1 | election_id != 4) %>%
  group_by(pollName) %>%
  mutate(pollster_id = cur_group_id(),
         pollster_id = 100 * election_id + pollster_id) %>%
  ungroup() %>%
  mutate(pollster_id = as.integer(factor(pollster_id))) %>%
  ungroup() %>%
  select(t, election_id, bloc_id, y,poll_id,pollster_id) %>%
  group_by(poll_id) %>%
  filter(sum(y) < 2000) %>%
  pivot_wider(id_cols = c(t, election_id, poll_id,pollster_id),
              names_from = bloc_id,
              values_from = y,
              values_fill = 0) %>%
  ungroup() %>%
  arrange(election_id, pollster_id)

df <- df[, order(colnames(df))]
results <- df[, grepl("\\d",colnames(df))]
miss01 <- ifelse(as.matrix(results) != 0,1, 0)
miss <- array(0, dim = dim(miss01))
abs_inc <- rep(0, nrow(miss))
for (j in 1:nrow(miss)){
  tmp <- as.logical(miss01[j, ])
  if (tmp[1] == FALSE){
    abs_inc[j] = 0
    tmp[1] <- TRUE
    miss[j, 1:sum(tmp)] <- seq(1, ncol(miss))[tmp]
  } else {
    abs_inc[j] = 1
    miss[j, 1:sum(tmp)] <- seq(1, ncol(miss))[as.logical(miss01[j, ])]
  }
}
NMiss <- apply(miss, 1, function(x) sum(as.integer(x != 0)))

#indicators <- expand.grid(t1 = 1:16, t2 = 1:4) %>%
indicators <- expand.grid(t1 = 1:16, t2 = seq(0, 5 * 52 * 3, 5 * 52)) %>%
  #indicators <- expand.grid(t1 = 1:16, t2 = 1:5) %>%
  mutate(i = 1:n(),
         t_long = t1 + t2,
         t2 = 1 + t2 / (5 * 52))
df <- df %>%
  left_join(indicators,
            by = c("t" = "t1",
                   "election_id" = "t2"))
NMiss_pred <- c(c(NMiss[sort(rep(match(c(1, 2, 3, 4), df$election_id), 16))]),
                rep(7, 16))
miss_pred <-  miss[sort(rep(match(c(1, 2, 3, 4), df$election_id), 16)), ]
for (i in 1:16){
  miss_pred <- miss_pred %>%
    rbind(c(1,2,3,4,6,7,8,0))
}
# NMiss_pred <- c(NMiss[sort(rep(match(c(1, 2, 3, 4), df$election_id), 16))],
#                 rep(8, 16))
# miss_pred <-  miss[c(sort(rep(match(c(1, 2, 3, 4), df$election_id), 16)),
#                      rep(match(3, df$election_id), 16)), ]
################################################################################
## Historical averages
m2_df_election_results <- read_csv(file = "dta/election_results/department_candidate_votes_blocs_clean.csv") %>%
  filter(departement != "Mayotte")
###############################################################################
#' Mangle data
m2_df_results <- m2_df_election_results %>%
  filter(year < 2017) %>%
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
m2_miss01 <- ifelse(as.matrix(m2_df) != 0,1, 0)
m2_miss <- array(0, dim = dim(m2_miss01))
for (j in 1:nrow(m2_miss)){
  m2_miss[j, 1:sum(m2_miss01[j,])] <- seq(1, ncol(m2_miss))[as.logical(m2_miss01[j, ])]
}
m2_miss <- rbind(m2_miss, c(1,2,3,4,6,7, 8, 0))
#m2_miss <- rbind(m2_miss, c(1,2,3,4,5,6,7, 8))
m2_NMiss <- apply(m2_miss, 1, function(x) sum(as.integer(x != 0)))
################################################################################
## datalist
data_list <- list(
  N1 = nrow(results),
  D = ncol(results),
  T1 = 16,
  T2 = 4,
  t1 = seq(1, 16),#indicators$t_long,
  indicator = df$i,
  index_election = df$election_id,

  y_results = t(round(result_matrix * 1000)),
  result_indicator = indicators %>% filter(t2 < 5, t1 == 16) %>% pull(i),
  NMiss_results = NMiss_results,
  miss_results = miss_results,

  P = c(df %>% distinct(election_id, pollster_id) %>%
    group_by(election_id) %>% summarize(n = n()) %>%
    pull(n), 3),
  p = df %>% pull(pollster_id),
  NPolls_Pollster = df %>% group_by(election_id, pollster_id) %>% summarize(n = n()) %>% pull(n),
  abs_inc = abs_inc,
  y = t(results),
  NMiss = NMiss,
  miss = miss,
  NMiss_pred = NMiss_pred,
  miss_pred = miss_pred,
  election = indicators$t2,

  m2_N1 = length(m2_year),
  m2_N2 = 1,
  m2_x1 = m2_year,
  m2_x2 = array(2017),
  m2_y = as.matrix(round(m2_df * 10000)),
  m2_prior_sigma_quality = 1,
  m2_NMiss = m2_NMiss,
  m2_miss = m2_miss
)
################################################################################
## Model
mod <- cmdstan_model("src/stan/testing/gp/gp_multi_election_season_hist_prior_fast2.stan")
################################################################################
## Run
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 100,
  iter_warmup = 500,
  parallel_chains = 6,
  refresh = 25,
  init = 0
)
################################################################################
## Plot
fit_summary <- fit$summary("m1_y2")
post_pred <- lapply(1:data_list$D, function(ii){
  data.frame(t1 = indicators$t1,
             t2 = indicators$t2,
             pred_mu = fit_summary %>%
               filter(grepl(paste(ii, "\\]", sep = ""), variable)) %>%
               pull(mean)) %>%
    mutate(d = ii) %>%
    return(.)
}) %>%
  do.call("bind_rows", .) %>%
  mutate(bloc = factor(bloc_vector[d], bloc_vector),
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
         bloc = factor(bloc_vector[as.integer(d)], levels = bloc_vector)) %>%
  left_join(data.frame(
    n = 1:nrow(indicators),
    t1 = indicators$t1,
    t2 = indicators$t2
  )) %>%
  mutate(
    year = c(2002, 2007, 2012, 2017, 2022)[t2]
  )


polls <- df %>%
  pivot_longer(c(-election_id, -poll_id, -t_long, -t, -i, -pollster_id),
               names_to = "d",
               values_to = "prob") %>%
  rename(t1 = t,
         t2 = election_id) %>%
  filter(prob != 0) %>%
  mutate(bloc = factor(bloc_vector[as.integer(d)], levels = bloc_vector),
         year = c(2002, 2007, 2012, 2017, 2022)[t2])

election_results_all <- election_results_all %>%
  mutate(d = match(bloc, bloc_vector),
         bloc = factor(bloc, levels = bloc_vector),
         t2 = match(year, c(2002, 2007, 2012, 2017))) %>%
  filter(year %in% c(2002, 2007, 2012, 2017))

prediction <- fit$summary("prediction", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(
    bloc = as.integer(str_match(variable, "(\\d)")[,2]),
    bloc = ifelse(bloc > 4, bloc + 1, bloc),
    bloc = factor(bloc_vector[bloc], levels = bloc_vector)
  ) %>%
  mutate(t1 = 16, year = 2017)

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
  #geom_point(data = prediction, aes(x = t1, y = `50%`), color = "black") +
  # geom_errorbar(data = prediction, aes(x = t1, ymin = `25%`, ymax = `75%`),
  #            color = "black", size = 0.5, width = 0) +
  # geom_errorbar(data = prediction, aes(x = t1, ymin = `10%`, ymax = `90%`),
  #               color = "black", size = 0.25, width = 0) +
  #geom_point(data = prediction, aes(x = t1, y = `50%`), color = "black") +
  theme_bw() + theme(legend.position="bottom") +
  scale_color_manual(name = '',
                     values = c('Realized data'='black',
                                'Latent mean function'='red',
                                "Posterior mean functions" = "blue",
                                "Posterior mean function" = "green")) +
  geom_hline(data = election_results_all, aes(yintercept = percentage)) +
  xlab('X') +
  ylab('y') +
  facet_grid(bloc ~ year)
p
###############################################################################
##
fit_summary <- fit$summary("m2_y2")
post_pred <- lapply(1:data_list$D, function(ii){
  print(ii)
  data.frame(x = c(m2_year, 2017),
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
    x2 = c(m2_year, 2017)
  ))

plt_df_rt_melt <- plt_df_rt_melt %>%
  mutate(bloc = factor(bloc_vector[d], levels = bloc_vector))
post_pred <- post_pred %>%
  mutate(bloc = factor(bloc_vector[d], levels = bloc_vector))
m2_df_results <- m2_df_results %>%
  mutate(bloc = factor(bloc, bloc_vector))


prediction <- fit$summary("prediction", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(
    bloc = factor(bloc_vector[as.integer(str_match(variable, "(\\d)")[,2])], levels = bloc_vector)
  ) %>%
  mutate(year = 2017)

p <- ggplot() +
  geom_line(data = plt_df_rt_melt %>%
              filter(iter %in% sample(1:1800, 400)), aes(x = x2, y = draws,
                                                         group = iter,
                                                         colour = bloc), alpha = 0.05) +
  geom_line(data = post_pred, aes(x = x, y = pred_mu), colour = "black", linetype = 2) +
  geom_point(data = m2_df_results, aes(x = year, y = percentage), color = "black") +
  geom_point(data = prediction, aes(x = year, y = `50%`), color = "black") +
  theme_bw() + theme(legend.position="bottom") +
  xlab('X') +
  ylab('y') +
  facet_wrap(bloc ~ .) +
  scale_color_manual(values = cols)
p
################################################################################

bayesplot::mcmc_hist(fit$draws(c("sigma_xi",
                                 "sigma_tau",
                                 "sigma_mu_pollster")))

bayesplot::mcmc_hist(fit$draws("alpha"))











bayesplot::mcmc_pairs(fit$draws(c("xi[1,1]","xi[2,1]","xi[3,1]","xi[4,1]",
                                   "xi[5,1]","xi[6,1]","xi[7,1]","xi[8,1]")))

bayesplot::mcmc_pairs(fit$draws(c("f[1,1]","f[2,1]","f[3,1]","f[4,1]",
                                  "f[5,1]","f[6,1]","f[7,1]","f[8,1]")))


bayesplot::mcmc_pairs(fit$draws(c("eta[1,1]","eta[2,1]","eta[3,1]","eta[4,1]",
                                  "eta[5,1]","eta[6,1]","eta[7,1]")))


fit$summary(c("mu_pollster[1,1]","mu_pollster[1,2]","mu_pollster[1,3]","mu_pollster[1,4]",
              "mu_pollster[1,5]","mu_pollster[1,6]","mu_pollster[1,7]","mu_pollster[1,8]",
              "mu_pollster[1,9]","mu_pollster[1,10]","mu_pollster[1,11]","mu_pollster[1,12]",
              "mu_pollster[1,13]","mu_pollster[1,17]","mu_pollster[1,15]","mu_pollster[1,16]")) %>%
  pull(mean) %>% sum()

fit$summary(c("mu_pollster[1,1]","mu_pollster[1,2]","mu_pollster[1,3]","mu_pollster[1,4]",
              "mu_pollster[1,5]","mu_pollster[1,6]")) %>%
  pull(mean) %>% sum()

fit$summary(c("mu_pollster[1,7]","mu_pollster[1,8]","mu_pollster[1,9]","mu_pollster[1,10]",
              "mu_pollster[1,11]","mu_pollster[1,12]")) %>%
  pull(mean) %>% sum()

fit$summary(c("mu_pollster[1,11]","mu_pollster[1,12]",
              "mu_pollster[1,13]","mu_pollster[1,14]","mu_pollster[1,15]","mu_pollster[1,16]",
              "mu_pollster[1,17]","mu_pollster[1,18]")) %>%
  pull(mean) %>% sum()

bayesplot::mcmc_trace(fit$draws("f[4,3]"))
bayesplot::mcmc_scatter(fit$draws(c("f[4,3]", "f[4,2]")))



bayesplot::mcmc_pairs(fit$draws(c("mu_pollster[1,5]","mu_pollster[2,5]",
                                  "mu_pollster[3,5]","mu_pollster[4,5]",
                                  "mu_pollster[5,5]","mu_pollster[6,5]",
                                  "mu_pollster[7,5]","mu_pollster[8,5]")))

bayesplot::mcmc_pairs(fit$draws(c("mu_pollster[1,1]","mu_pollster[1,2]",
                                  "mu_pollster[1,3]","mu_pollster[1,4]",
                                  "mu_pollster[1,5]","mu_pollster[1,6]",
                                  "mu_pollster[1,7]","mu_pollster[1,8]")))


bayesplot::mcmc_pairs(fit$draws(c("eta[1,1]","eta[2,1]","eta[3,1]","eta[4,1]",
                                  "eta[5,1]","eta[6,1]","eta[7,1]","eta[8,1]")))

bayesplot::mcmc_pairs(fit$draws(c("alpha[1]","alpha[2]","alpha[3]","alpha[4]",
                                  "alpha[5]","alpha[6]","alpha[7]","alpha[8]")))
bayesplot::mcmc_pairs(fit$draws(c("xi[1,1]","xi[2,1]","xi[3,1]","xi[4,1]",
                                  "xi[5,1]","xi[6,1]","xi[7,1]","xi[8,1]")))

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

y_rep <- fit$summary("y_rep") %>%
  mutate(
    d = as.integer(str_match(variable, "([\\d]+),")[, 2]),
    i = as.integer(str_match(variable, ",([\\d]+)")[, 2]),
  ) %>%
  filter(rhat > 1.05)
  select(d, i, mean)



