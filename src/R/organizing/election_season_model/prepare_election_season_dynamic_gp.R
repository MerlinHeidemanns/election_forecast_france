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
election_results <- election_results %>%
  group_by(year, bloc) %>%
  mutate(bloc = ifelse(percentage == max(percentage), bloc, "Autre"),
         bloc = ifelse(candidate == "_Abstention", "Abstention", bloc)) %>%
  summarize(percentage = sum(percentage)) %>%
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
miss01_results <- ifelse(as.matrix(result_matrix) != 0,1, 0)
miss_results <- array(0, dim = dim(miss01_results))
for (j in 1:nrow(miss_results)){
  miss_results[j, 1:sum(miss01_results[j, ])] <- seq(1, ncol(miss_results))[as.logical(miss01_results[j, ])]
}
NMiss_results <- apply(miss_results, 1, function(x) sum(as.integer(x != 0)))


################################################################################
df <- df %>%
  mutate(bloc_id = match(bloc, bloc_vector)) %>%
  mutate(pollName = ifelse(grepl("Sofres", pollName), "Sofres", pollName)) %>%
  group_by(pollName) %>%
  mutate(pollster_id = cur_group_id()) %>%
  ungroup() %>%
  select(t, election_id, bloc_id, y,poll_id,pollster_id) %>%
  group_by(poll_id) %>%
  filter(sum(y) < 2000) %>%
  pivot_wider(id_cols = c(t, election_id, poll_id,pollster_id),
              names_from = bloc_id,
              values_from = y,
              values_fill = 0) %>%
  ungroup() %>%
  filter(t < 14 | election_id != 4)

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

indicators <- expand.grid(t1 = 1:16, t2 = 1:5) %>%
  mutate(i = 1:n())
df <- df %>%
  left_join(indicators,
            by = c("t" = "t1",
                   "election_id" = "t2"))
NMiss_pred <- c(NMiss[sort(rep(match(c(1, 2, 3, 4), df$election_id), 16))],
                rep(8, 16))
miss_pred <-  miss[c(sort(rep(match(c(1, 2, 3, 4), df$election_id), 16)),
                     rep(match(3, df$election_id), 16)), ]

################################################################################
## Historical averages



################################################################################
## datalist
data_list <- list(
  N1 = nrow(results),
  D = ncol(results),
  T1 = 16,
  T2 = 5,
  t1 = indicators$t1,
  t2 = indicators$t2,
  indicator = df$i,

  y_results = t(round(result_matrix * 100)),
  result_indicator = indicators %>% filter(t2 < 5, t1 == 16) %>% pull(i),
  NMiss_results = NMiss_results,
  miss_results = miss_results,

  P = df %>% pull(pollster_id) %>% max(),
  p = df %>% pull(pollster_id),
  abs_inc = abs_inc,
  y = t(results),
  NMiss = NMiss,
  miss = miss,
  NMiss_pred = NMiss_pred,
  miss_pred = miss_pred,
  election = indicators$t2
)
################################################################################
## Model
mod <- cmdstan_model("src/stan/testing/gp/gp_multi_election_season.stan")
################################################################################
## Run
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 200,
  iter_warmup = 400,
  parallel_chains = 6,
  refresh = 25,
  init = 0
)
################################################################################
## Plot
post_pred <- lapply(1:data_list$D, function(ii){
  data.frame(t1 = indicators$t1,
             t2 = indicators$t2,
             pred_mu = fit$summary("y2") %>%
               filter(grepl(paste(ii, "\\]", sep = ""), variable)) %>%
               pull(mean)) %>%
    mutate(d = ii) %>%
    return(.)
}) %>%
  do.call("bind_rows", .)
plt_df_rt_melt = fit$draws("y2") %>%
  posterior::as_draws_df() %>%
  select(!contains(".")) %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = c("n", "d"),
               values_to = "draws",
               names_pattern = "([\\d]+),([\\d]+)") %>%
  mutate(n = as.integer(n)) %>%
  left_join(data.frame(
    n = 1:nrow(indicators),
    t1 = indicators$t1,
    t2 = indicators$t2
  ))


polls <- df %>%
  pivot_longer(c(-election_id, -poll_id, -t, -i, -pollster_id),
               names_to = "d",
               values_to = "prob") %>%
  rename(t1 = t,
         t2 = election_id) %>%
  filter(prob != 0)

election_results <- election_results %>%
  mutate(d = match(bloc, bloc_vector),
         t2 = match(year, c(2002, 2007, 2012, 2017)))

p <- ggplot() +
  geom_line(data = plt_df_rt_melt %>%
              filter(iter %in% sample(1:1800, 400)), aes(x = t1, y = draws,
                                                         group = iter,
                                                         colour = 'Posterior mean functions'), alpha = 0.1) +
  geom_line(data = post_pred,
            aes(x = t1, y = pred_mu,
                colour = 'Posterior mean function')) +
  geom_point(data = polls %>%
               group_by(poll_id) %>%
               mutate(prob = prob/sum(prob)), aes(x = t1, y = prob)) +
  theme_bw() + theme(legend.position="bottom") +
  scale_color_manual(name = '',
                     values = c('Realized data'='black',
                                'Latent mean function'='red',
                                "Posterior mean functions" = "blue",
                                "Posterior mean function" = "green")) +
  geom_hline(data = election_results, aes(yintercept = percentage)) +
  xlab('X') +
  ylab('y') +
  facet_grid(d ~ t2, scales = "free")
p














