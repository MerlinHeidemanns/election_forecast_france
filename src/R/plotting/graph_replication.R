################################################################################
## Plots
################################################################################
## Libraries
library(tidyverse)
library(cmdstanr)
library(posterior)
################################################################################
start_day_2017 <- as.Date("2016-12-18 ")
start_times_2017 <- c(start_day_2017, start_day_2017 + seq(1, 17) * 7)
################################################################################
## 1) Change in prediction over time for 2017
# The models have to be rerun because the code didn't update the unemployment data.
prediction <- lapply(c(3,5,7,9,11,13,15,17), function(j){
  fit <- read_rds(paste0("dta/fit/m2017_", j,".Rds", sep = ""))
  prediction_results <- fit$summary("prediction",
                                    ~ quantile(., c(0.1,0.25, 0.5, 0.75, 0.9))) %>%
    mutate(
      bloc = as.integer(str_match(variable, "(\\d+)")[,2]),
      bloc = bloc_vector[1 + ifelse(bloc > 3, bloc + 1, bloc)],
      year = 2017,
      t = start_times_2017[j]
    ) %>%
    left_join(
      election_results_all %>%
        filter(bloc != "Abstention") %>%
        group_by(year) %>%
        mutate(percentage = percentage/sum(percentage, na.rm = TRUE))
    )
  return(prediction_results)
}) %>%
  do.call("bind_rows", .)

plt <- ggplot(prediction, aes(x = t, y = `50%`)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = "red", alpha = 0.25) +
  geom_ribbon(aes(ymin = `10%`, ymax = `90%`), fill = "red", alpha = 0.125) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_hline(aes(yintercept = percentage)) +
  theme_light() +
  facet_wrap(bloc ~.) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Vote share")
ggsave("plt/paper/2017_vote_share_prediction_joint.png", plt, width = 10, height = 6)

## 2) Change in prediction over time for 2017 model 1
# The models have to be rerun because the code didn't update the unemployment data.
prediction <- lapply(c(3,5,7,9,11,13,15,17), function(j){
  fit <- read_rds(paste0("dta/fit/m2017_", j,".Rds", sep = ""))
  prediction_results <- fit$summary("m1_prediction_new",
                                    ~ quantile(., c(0.1,0.25, 0.5, 0.75, 0.9))) %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + ifelse(bloc > 3, bloc + 1, bloc)],
      year = 2017,
      t = start_times_2017[j]
    ) %>%
    left_join(
      election_results_all %>%
        filter(bloc != "Abstention") %>%
        group_by(year) %>%
        mutate(percentage = percentage/sum(percentage, na.rm = TRUE))
    )
  return(prediction_results)
}) %>%
  do.call("bind_rows", .)

plt <- ggplot(prediction, aes(x = t, y = `50%`)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = "red", alpha = 0.25) +
  geom_ribbon(aes(ymin = `10%`, ymax = `90%`), fill = "red", alpha = 0.125) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_hline(aes(yintercept = percentage)) +
  theme_light() +
  facet_wrap(bloc ~.) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Vote share")
ggsave("plt/paper/2017_vote_share_prediction_m1.png", plt, width = 10, height = 6)


## 3) Change in prediction over time for 2017 model 2
# The models have to be rerun because the code didn't update the unemployment data.
prediction <- lapply(c(3,5,7,9,11,13,15,17), function(j){
  fit <- read_rds(paste0("dta/fit/m2017_", j,".Rds", sep = ""))
  prediction_results <- fit$summary("m2_prediction_new",
                                    ~ quantile(., c(0.1,0.25, 0.5, 0.75, 0.9))) %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + ifelse(bloc > 3, bloc + 1, bloc)],
      year = 2017,
      t = start_times_2017[j]
    ) %>%
    left_join(
      election_results_all %>%
        filter(bloc != "Abstention") %>%
        group_by(year) %>%
        mutate(percentage = percentage/sum(percentage, na.rm = TRUE))
    )
  return(prediction_results)
}) %>%
  do.call("bind_rows", .)

plt <- ggplot(prediction, aes(x = t, y = `50%`)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = "red", alpha = 0.25) +
  geom_ribbon(aes(ymin = `10%`, ymax = `90%`), fill = "red", alpha = 0.125) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_hline(aes(yintercept = percentage)) +
  theme_light() +
  facet_wrap(bloc ~.) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Vote share")
ggsave("plt/paper/2017_vote_share_prediction_m2.png", plt, width = 10, height = 6)

## 3) Change in prediction over time for 2017 model 2
# The models have to be rerun because the code didn't update the unemployment data.
prediction <- lapply(c(3,5,7,9,11,13,15,17), function(j){
  fit <- read_rds(paste0("dta/fit/m2017_", j,".Rds", sep = ""))
  prediction_results <- fit$summary("m3_prediction_new",
                                    ~ quantile(., c(0.1,0.25, 0.5, 0.75, 0.9))) %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + ifelse(bloc > 3, bloc + 1, bloc)],
      year = 2017,
      t = start_times_2017[j]
    ) %>%
    left_join(
      election_results_all %>%
        filter(bloc != "Abstention") %>%
        group_by(year) %>%
        mutate(percentage = percentage/sum(percentage, na.rm = TRUE))
    )
  return(prediction_results)
}) %>%
  do.call("bind_rows", .)

plt <- ggplot(prediction, aes(x = t, y = `50%`)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = "red", alpha = 0.25) +
  geom_ribbon(aes(ymin = `10%`, ymax = `90%`), fill = "red", alpha = 0.125) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_hline(aes(yintercept = percentage)) +
  theme_light() +
  facet_wrap(bloc ~.) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Vote share")
ggsave("plt/paper/2017_vote_share_prediction_m3.png", plt, width = 10, height = 6)

################################################################################
## Prediction 2012
prediction <- lapply(c(4,5,6,7,8,9,10,11,12,13,14,15,16,17), function(j){
  fit <- read_rds(paste0("dta/fit/m2012_", j,".Rds", sep = ""))
  prediction_results <- fit$summary("prediction",
                                    ~ quantile(., c(0.025, 0.1,0.25, 0.5, 0.75, 0.9, 0.975))) %>%
    mutate(
      bloc = as.integer(str_match(variable, "(\\d+)")[,2]),
      bloc = bloc_vector[1 + bloc],
      year = 2012,
      t = start_times[j]
    ) %>%
    left_join(
      election_results_all %>%
        filter(bloc != "Abstention") %>%
        group_by(year) %>%
        mutate(percentage = percentage/sum(percentage, na.rm = TRUE))
    )
  return(prediction_results)
}) %>%
  do.call("bind_rows", .)

plt <- ggplot(prediction, aes(x = t, y = `50%`)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = "red", alpha = 0.25) +
  geom_ribbon(aes(ymin = `10%`, ymax = `90%`), fill = "red", alpha = 0.125) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), fill = "red", alpha = 0.05) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_hline(aes(yintercept = percentage)) +
  theme_light() +
  facet_wrap(bloc ~.) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Vote share")
ggsave("plt/paper/2012_vote_share_prediction_joint.png", plt, width = 10, height = 6)


## Prediction 2012
prediction <- lapply(c(4,5,6,7,8,9,10,11,12,13,14,15,16,17), function(j){
  fit <- read_rds(paste0("dta/fit/m2012_", j,".Rds", sep = ""))
  prediction_results <- fit$summary("m1_prediction_new",
                                    ~ quantile(., c(0.025, 0.1,0.25, 0.5, 0.75, 0.9, 0.975))) %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + bloc],
      year = 2012,
      t = start_times[j]
    ) %>%
    left_join(
      election_results_all %>%
        filter(bloc != "Abstention") %>%
        group_by(year) %>%
        mutate(percentage = percentage/sum(percentage, na.rm = TRUE))
    )
  return(prediction_results)
}) %>%
  do.call("bind_rows", .)

plt <- ggplot(prediction, aes(x = t, y = `50%`)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = "red", alpha = 0.25) +
  geom_ribbon(aes(ymin = `10%`, ymax = `90%`), fill = "red", alpha = 0.125) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), fill = "red", alpha = 0.05) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_hline(aes(yintercept = percentage)) +
  theme_light() +
  facet_wrap(bloc ~.) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Vote share")
ggsave("plt/paper/2012_vote_share_prediction_m1.png", plt, width = 10, height = 6)

## Prediction 2012
prediction <- lapply(c(4,5,6,7,8,9,10,11,12,13,14,15,16,17), function(j){
  fit <- read_rds(paste0("dta/fit/m2012_", j,".Rds", sep = ""))
  prediction_results <- fit$summary("m2_prediction_new",
                                    ~ quantile(., c(0.025, 0.1,0.25, 0.5, 0.75, 0.9, 0.975))) %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + bloc],
      year = 2012,
      t = start_times[j]
    ) %>%
    left_join(
      election_results_all %>%
        filter(bloc != "Abstention") %>%
        group_by(year) %>%
        mutate(percentage = percentage/sum(percentage, na.rm = TRUE))
    )
  return(prediction_results)
}) %>%
  do.call("bind_rows", .)

plt <- ggplot(prediction, aes(x = t, y = `50%`)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = "red", alpha = 0.25) +
  geom_ribbon(aes(ymin = `10%`, ymax = `90%`), fill = "red", alpha = 0.125) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), fill = "red", alpha = 0.05) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_hline(aes(yintercept = percentage)) +
  theme_light() +
  facet_wrap(bloc ~.) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Vote share")
ggsave("plt/paper/2012_vote_share_prediction_m2.png", plt, width = 10, height = 6)


## Prediction 2012
prediction <- lapply(c(4,5,6,7,8,9,10,11,12,13,14,15,16,17), function(j){
  fit <- read_rds(paste0("dta/fit/m2012_", j,".Rds", sep = ""))
  prediction_results <- fit$summary("m3_prediction_new",
                                    ~ quantile(., c(0.025, 0.1,0.25, 0.5, 0.75, 0.9, 0.975))) %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + bloc],
      year = 2012,
      t = start_times[j]
    ) %>%
    left_join(
      election_results_all %>%
        filter(bloc != "Abstention") %>%
        group_by(year) %>%
        mutate(percentage = percentage/sum(percentage, na.rm = TRUE))
    )
  return(prediction_results)
}) %>%
  do.call("bind_rows", .)

plt <- ggplot(prediction, aes(x = t, y = `50%`)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = "red", alpha = 0.25) +
  geom_ribbon(aes(ymin = `10%`, ymax = `90%`), fill = "red", alpha = 0.125) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), fill = "red", alpha = 0.05) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_hline(aes(yintercept = percentage)) +
  theme_light() +
  facet_wrap(bloc ~.) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Vote share")
ggsave("plt/paper/2012_vote_share_prediction_m3.png", plt, width = 10, height = 6)


## Prediction 2012
prediction <- lapply(c(4,5,6,7,8,9,10,11,12,13,14,15,16,17), function(j){
  fit <- read_rds(paste0("dta/fit/m2012_", j,".Rds", sep = ""))
  ## Joint
  prediction_results_joint <- fit$draws("prediction") %>%
    posterior::as_draws_df() %>%
    pivot_longer(everything(),
                 names_to = "variable",
                 values_to = "draws") %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + bloc],
      year = 2012,
      t = start_times[j]
    ) %>%
    filter(!is.na(bloc)) %>%
    left_join(
      election_results_all %>%
        filter(bloc != "Abstention") %>%
        group_by(year) %>%
        mutate(percentage = percentage/sum(percentage, na.rm = TRUE))
    ) %>%
    mutate(error = percentage - draws) %>%
    group_by(bloc, year, t) %>%
    summarize(
      q025 = quantile(error, 0.025),
      q10 = quantile(error, 0.10),
      q25 = quantile(error, 0.25),
      q50 = quantile(error, 0.5),
      q75 = quantile(error, 0.75),
      q90 = quantile(error, 0.9),
      q975 = quantile(error, 0.975),
    ) %>%
    mutate(kind = "joint")
  ## M1
  prediction_results_m1 <- fit$draws("m1_prediction_new") %>%
    posterior::as_draws_df() %>%
    pivot_longer(everything(),
                 names_to = "variable",
                 values_to = "draws") %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + bloc],
      year = 2012,
      t = start_times[j]
    ) %>%
    filter(!is.na(bloc)) %>%
    left_join(
      election_results_all %>%
        filter(bloc != "Abstention") %>%
        group_by(year) %>%
        mutate(percentage = percentage/sum(percentage, na.rm = TRUE))
    ) %>%
    mutate(error = percentage - draws) %>%
    group_by(bloc, year, t) %>%
    summarize(
      q025 = quantile(error, 0.025),
      q10 = quantile(error, 0.10),
      q25 = quantile(error, 0.25),
      q50 = quantile(error, 0.5),
      q75 = quantile(error, 0.75),
      q90 = quantile(error, 0.9),
      q975 = quantile(error, 0.975),
    ) %>%
    mutate(kind = "m1")
  # M2
  prediction_results_m2 <- fit$draws("m2_prediction_new") %>%
    posterior::as_draws_df() %>%
    pivot_longer(everything(),
                 names_to = "variable",
                 values_to = "draws") %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + bloc],
      year = 2012,
      t = start_times[j]
    ) %>%
    filter(!is.na(bloc)) %>%
    left_join(
      election_results_all %>%
        filter(bloc != "Abstention") %>%
        group_by(year) %>%
        mutate(percentage = percentage/sum(percentage, na.rm = TRUE))
    ) %>%
    mutate(error = percentage - draws) %>%
    group_by(bloc, year, t) %>%
    summarize(
      q025 = quantile(error, 0.025),
      q10 = quantile(error, 0.10),
      q25 = quantile(error, 0.25),
      q50 = quantile(error, 0.5),
      q75 = quantile(error, 0.75),
      q90 = quantile(error, 0.9),
      q975 = quantile(error, 0.975),
    ) %>%
    mutate(kind = "m2")
  # M3
  prediction_results_m3 <- fit$draws("m3_prediction_new") %>%
    posterior::as_draws_df() %>%
    pivot_longer(everything(),
                 names_to = "variable",
                 values_to = "draws") %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + bloc],
      year = 2012,
      t = start_times[j]
    ) %>%
    filter(!is.na(bloc)) %>%
    left_join(
      election_results_all %>%
        filter(bloc != "Abstention") %>%
        group_by(year) %>%
        mutate(percentage = percentage/sum(percentage, na.rm = TRUE))
    ) %>%
    mutate(error = percentage - draws) %>%
    group_by(bloc, year, t) %>%
    summarize(
      q025 = quantile(error, 0.025),
      q10 = quantile(error, 0.10),
      q25 = quantile(error, 0.25),
      q50 = quantile(error, 0.5),
      q75 = quantile(error, 0.75),
      q90 = quantile(error, 0.9),
      q975 = quantile(error, 0.975),
    ) %>%
    mutate(kind = "m3")
  return(bind_rows(
    prediction_results_joint,
    prediction_results_m1,
    prediction_results_m2,
    prediction_results_m3
  ))
}) %>%
  do.call("bind_rows", .)

plt <- ggplot(prediction, aes(x = t, y = q50)) +
  geom_line(aes(color = kind)) +
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = kind), alpha = 0.25) +
  #geom_ribbon(aes(ymin = q10, ymax = q90, fill = kind), alpha = 0.125) +
  #geom_ribbon(aes(ymin = q025, ymax = q975, fill = kind), alpha = 0.05) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  theme_light() +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(bloc ~.) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Vote share")
ggsave("plt/paper/2012_prediction_error_m3.png", plt, width = 10, height = 6)






################################################################################
## Error in poll prediction
start <- 366
end <- 578
start_indicators <- c(3,9,15)
top_line <- c(381, 389, 404, 432, 465, 495, 524)
weeks <- data_list$m1_ix_week
fit <- read_rds(paste0("dta/fit/m2017_", 7,".Rds", sep = ""))


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
  mutate(week = weeks[1:n()]) %>%
  pivot_longer(c(-election_id, -poll_id, -t_long, -t, -i, -pollster_id, -week),
               names_to = "d",
               values_to = "prob") %>%
  rename(t1 = t,
         t2 = election_id) %>%
  filter(prob != 0) %>%
  mutate(bloc = factor(bloc_vector[as.integer(d)], levels = bloc_vector),
         year = c(2002, 2007, 2012, 2017,2022)[t2],
         used = ifelse(week > 5, "Excluded", "Included"))

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
    bloc = ifelse(bloc > 4, bloc + 1, bloc),
    bloc = factor(bloc_vector[bloc], levels = bloc_vector)
  ) %>%
  mutate(t1 = count_weeks, year = 2017)

p <- ggplot() +
  geom_line(data = plt_df_rt_melt %>%
              filter(iter %in% sample(1:1800, 100),
                     year == 2017,
                     bloc != "Ecologisme"), aes(x = t1, y = draws,
                                                         group = iter,
                                                         colour = 'Posterior mean functions'), alpha = 0.1) +
  geom_line(data = post_pred %>%
              filter(year == 2017,
                     bloc != "Ecologisme"),
            aes(x = t1, y = pred_mu,
                colour = 'Posterior mean function')) +
  geom_point(data = polls %>%
               group_by(poll_id) %>%
               mutate(prob = prob/sum(prob)) %>%
               filter(year == 2017,
                      bloc != "Ecologisme"), aes(x = t1, y = prob, shape = used)) +
  geom_point(data = prediction, aes(x = t1, y = `50%`), color = "black") +
  geom_errorbar(data = prediction, aes(x = t1, ymin = `25%`, ymax = `75%`),
                color = "black", size = 0.5, width = 0) +
  geom_errorbar(data = prediction, aes(x = t1, ymin = `10%`, ymax = `90%`),
                color = "black", size = 0.25, width = 0) +
  theme_bw() + theme(legend.position="bottom") +
  geom_hline(data = election_results_all %>%
               filter(year == 2017,
                      bloc != "Ecologisme"), aes(yintercept = percentage)) +
  xlab('X') +
  ylab('y') +
  facet_grid(bloc~.)
p
##########
start <- 366
end <- 578
start_indicators <- c(3,9,15)
top_line <- c(381, 389, 404, 432, 465, 495, 524)
weeks <- data_list$m1_ix_week
fit <- read_rds(paste0("dta/fit/m2017_", 3,".Rds", sep = ""))
y_rep <- fit$draws("m1_y_rep") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "draws") %>%
  mutate(
    bloc_id = bloc_vector[1 + as.integer(str_match(variable, "(\\d+),")[, 2])],
    obs_id = as.integer(str_match(variable, ",(\\d+)")[, 2]),
    week_id = weeks[obs_id]
  ) %>%
  filter(!is.na(bloc_id)) %>%
  filter(obs_id > top_line[1]) %>%
  group_by(bloc_id, week_id) %>%
  summarize(
    q50 = quantile(draws, 0.5, na.rm = TRUE),
    q25 = quantile(draws, 0.25, na.rm = TRUE),
    q75 = quantile(draws, 0.75, na.rm = TRUE),
    q10 = quantile(draws, 0.1, na.rm = TRUE),
    q90 = quantile(draws, 0.9, na.rm = TRUE)
  )
ggplot(y_rep, aes(x = week_id, y = q50)) +
  geom_line(aes(color = bloc_id))


################################################################################
## 2022
library(tidyverse)
fit <- read_rds("dta/fit/m2022.Rds")
data_list <- read_rds("dta/fit/m2022_data_list.Rds")
indicators <- read_rds("dta/fit/m2022_indicator.Rds")
df <- read_rds("dta/fit/m2022_polls.rds")
election_results_all <- read_rds("dta/fit/m2022_election_results_all.rds")
pollster_vector <- read_rds("dta/fit/m2022_pollster_vector.rds")
m2_year <- read_rds("dta/fit/m2022_m2_year.rds")
m2_df_results <- read_rds("dta/fit/m2022_m2_df_results.rds")

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
days <- as.Date("2021-12-05") + seq(0, 7 * 17, 7)

## Matchup matrix
prediction <- fit$draws("prediction_adjusted") %>%
  posterior::as_draws_df()
main_candidates_list <- c("Melenchon", "Hidalgo", "Jadot", "Macron", "Pecresse", "Le Pen",
                          "Zemmour")
out <- matrix(NA, 7, 7)
out2 <- data.frame()
for (j in 2:7){
  for (i in (j + 1):8){
    tmp <- matrix(NA, nrow = nrow(prediction), ncol = 5)
    count <- 0
    for (r in seq(2,8)[c(-(i - 1), -(j - 1))]){
      count <- count + 1
      tmp[, count] <- (prediction[,j] > prediction[,r]) & (prediction[,i] > prediction[,r])
    }
    out[j - 1, i - 1] <- mean(apply(tmp, 1, all))
    out[i - 1, j - 1] <- mean(apply(tmp, 1, all))
    out2 <- bind_rows(out2,
                      data.frame(
                        pair = paste(main_candidates_list[j - 1],main_candidates_list[i - 1], sep = "-"),
                        prob = mean(apply(tmp, 1, all))))
  }
}


runoff_prob <- data.frame(
  candidates = main_candidates_list,
  prob = out %>%
    apply(., 2, function(x) sum(x, na.rm = TRUE))
) %>%
  arrange(prob) %>%
  mutate(candidates = factor(candidates, levels = candidates))
matchup_prob <- out2 %>%
  arrange(prob) %>%
  mutate(pair = factor(pair, levels = pair))
plt <- ggplot(data = matchup_prob, aes(x = pair, y = prob)) +
  geom_point() +
  coord_flip() +
  theme(axis.title.y = element_blank()) +
  labs(y = "Match up probability", x ="") +
  theme_light()
ggsave("plt/paper/2022_matchup_probability.png", plot = plt)

## Prediction uncertainty
main_candidates_list <- c("Melenchon", "Hidalgo", "Jadot", "Macron", "Pecresse", "Le Pen",
                          "Zemmour")
prediction <- fit$draws("prediction_adjusted") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "candidate_id",
               values_to = "draws",
               names_pattern = "(\\d+)") %>%
  mutate(candidate = main_candidates_list[as.integer(candidate_id)]) %>%
  filter(!is.na(candidate)) %>%
  group_by(candidate) %>%
  summarize(
    q10 = quantile(draws, 0.1),
    q25 = quantile(draws, 0.25),
    q50 = quantile(draws, 0.5),
    q75 = quantile(draws, 0.75),
    q90 = quantile(draws, 0.9)
  ) %>%
  mutate(candidate = factor(candidate, levels = main_candidates_list))

ggplot(prediction, aes(x = candidate, y = q50)) +
  geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size = 0.5) +
  geom_errorbar(aes(ymin = q10, ymax = q90), width = 0, size = 0.25) +
  theme_light() +
  theme(axis.title.x = element_blank()) +
  labs(x = "Predicted vote share") +
  coord_flip()




