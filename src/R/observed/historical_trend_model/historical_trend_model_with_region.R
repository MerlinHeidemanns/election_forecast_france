###############################################################################
## Title: Prepare datalist for gp prediction
###############################################################################
## Description
#'
###############################################################################
## Notes
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
###############################################################################
df_election_results <- read_csv(file = "dta/election_results/department_candidate_votes_blocs_clean.csv") %>%
  filter(departement != "Mayotte")
df_regional_results <- read_csv(file = "dta/election_results/election_results_regional.csv") %>%
  mutate(bloc = ifelse(bloc == "Regionalisme", "Autre", bloc))

###############################################################################
#' Mangle data
df_results <- df_election_results %>%
  group_by(year, bloc) %>%
  filter(!is.na(votes), !is.na(percentage), bloc != "Abstention") %>%
  summarize(percentage = sum(votes * percentage / votes)) %>%
  group_by(year) %>%
  mutate(percentage = percentage/sum(percentage)) %>%
  mutate(bloc_id = match(bloc, bloc_vector))
df <- df_results %>%
  select(-bloc) %>%
  pivot_wider(id_cols = year,
              names_from = bloc_id,
              values_from = percentage,
              values_fill = 0) %>%
  ungroup() %>%
  mutate(id = 1)

df_regional_results <- df_regional_results %>%
  filter(bloc != "Abstention") %>%
  group_by(year, bloc) %>%
  summarize(votes = sum(votes, na.rm = TRUE)) %>%
  group_by(year) %>%
  mutate(percentage = votes/sum(votes),
         bloc_id = match(bloc, bloc_vector)) %>%
  select(-votes, -bloc) %>%
  pivot_wider(id_cols = year,
              names_from = bloc_id,
              values_from = percentage,
              values_fill = 0) %>%
  mutate(id = 2)

df <- bind_rows(
  df,
  df_regional_results
)


year <- df$year
id <- df$id
df <- df %>%
  select(-year, -id)
df <- df[, order(colnames(df))]
miss01 <- ifelse(as.matrix(df) != 0,1, 0)
miss <- array(0, dim = dim(miss01))
for (j in 1:nrow(miss)){
  miss[j, 1:sum(miss01[j,])] <- seq(1, ncol(miss))[as.logical(miss01[j, ])]
}
miss <- rbind(miss, c(1,2,3,4,5,6,7))
NMiss <- apply(miss, 1, function(x) sum(as.integer(x != 0)))
###############################################################################
## Model
mod <- cmdstan_model("src/stan/testing/gp/gp_results_plus_regional.stan")
data_list <- list(
  N1 = c(10, 7),
  N2 = 1,
  D = 7,
  x1 = year,
  x2 = array(2022),
  type1 = id,
  type2 = array(1),
  y = as.matrix(round(df * 10000)),
  prior_sigma_quality = 0.25,
  NMiss = NMiss,
  miss = miss
)
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 500,
  iter_warmup = 500,
  parallel_chains = 6,
  refresh = 250,
  init = 0
)
fit_summary <- fit$summary("y2")
post_pred <- lapply(1:data_list$D, function(ii){
  print(ii)
  data.frame(x = c(year, 2022),
             pred_mu = fit_summary %>%
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
  mutate(n = as.integer(n),
         d = as.integer(d)) %>%
  left_join(data.frame(
    n = 1:(length(year) + 1),
    x2 = c(year, 2022)
  ))

plt_df_rt_melt <- plt_df_rt_melt %>%
  mutate(bloc = factor(bloc_vector[d + 1], levels = bloc_vector))
post_pred <- post_pred %>%
  mutate(bloc = factor(bloc_vector[d + 1], levels = bloc_vector))
df_results <- df_results %>%
  mutate(bloc = factor(bloc, bloc_vector))

p <- ggplot() +
  geom_line(data = plt_df_rt_melt %>%
              filter(iter %in% sample(1:1800, 400)), aes(x = x2, y = draws,
                                                         group = iter,
                                                         colour = bloc), alpha = 0.05) +
  geom_line(data = post_pred, aes(x = x, y = pred_mu), colour = "black", linetype = 2) +
  geom_point(data = df_results, aes(x = year, y = percentage), color = "black") +
  theme_bw() + theme(legend.position="bottom") +
  xlab('X') +
  ylab('y') +
  facet_wrap(bloc ~ .) +
  scale_color_manual(values = cols)
p











