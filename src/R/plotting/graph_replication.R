################################################################################
## Plots
################################################################################
## English
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_TIME", "C")
################################################################################
## Libraries
library(tidyverse)
library(cmdstanr)
library(posterior)
################################################################################
start_day_2022 <- as.Date("2021-12-05")
start_day_2017 <- as.Date("2016-12-18")
start_day_2012 <- as.Date("2011-12-18")
start_times_2017 <- c(start_day_2017, start_day_2017 + seq(1, 17) * 7)
start_times_2012 <- c(start_day_2012, start_day_2012 + seq(1, 17) * 7)
start_times_2022 <- c(start_day_2022, start_day_2022 + seq(1, 17) * 7)
################################################################################
## Figure 1: Change in predicted vote share by bloc for 2012

## Prediction 2012
prediction <- lapply(seq(3, 17, 1), function(j){
  fit <- read_rds(paste0("dta/fit/m2012_w_shares_", j,".Rds", sep = ""))
  prediction_results <- fit$summary("prediction",
                                    ~ quantile(., c(0.025, 0.1,0.25, 0.5, 0.75, 0.9, 0.975))) %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + bloc],
      year = 2012,
      t = start_times_2012[j]
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
  labs(y = "Predicted vote share")
ggsave("plt/paper/2012_vote_share_prediction_w_shares.png", plt, width = 10, height = 6)

################################################################################
## Figure 2: Left: MAE over time, Right: Left-Right run-off probability
prediction <- lapply(seq(3, 17, 1), function(j){
  fit <- read_rds(paste0("dta/fit/m2012_w_shares_", j,".Rds", sep = ""))
  prediction_results <- fit$draws("prediction") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "variable",
                 values_to = "draws",
                 names_pattern = "([\\d+])") %>%
    filter(!is.na(variable)) %>%
    mutate(
      variable = as.integer(variable),
      bloc = bloc_vector[1 + variable],
      year = 2012
    ) %>%
    left_join(
      election_results_all %>%
        filter(bloc != "Abstention") %>%
        group_by(year) %>%
        mutate(percentage = percentage/sum(percentage, na.rm = TRUE))
    ) %>%
    group_by(iter) %>%
    summarize(mae = mean(abs(percentage - draws))) %>%
    ungroup() %>%
    summarize(
      q50 = quantile(mae, 0.5),
      q25 = quantile(mae, 0.25),
      q75 = quantile(mae, 0.75),
      q10 = quantile(mae, 0.10),
      q90 = quantile(mae, 0.90)
    ) %>%
    mutate(
      t = start_times_2012[j]
    )
  return(prediction_results)
}) %>%
  do.call("bind_rows", .)

plt1 <- ggplot(prediction, aes(x = t, y = q50)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.25, fill = "red") +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.125, fill = "red") +
  theme_light() +
  theme(axis.title.x = element_blank()) +
  labs(y = "Mean absolute error (prediction)",
       caption = "Thick line: Median
                  Deeper/lighter shade: 50/80% quantile") +
  theme(axis.title.x = element_blank())

## Probability of run-off between Sarkozy and Hollande
pr_gauche_droite <- lapply(seq(3, 17, 1), function(j){
  fit <- read_rds(paste0("dta/fit/m2012_w_shares_", j,".Rds", sep = ""))
  pr_gauche_droite <- fit$draws("prediction") %>%
    posterior::as_draws_df() %>%
    mutate(pr_gauche_droite =
             as.integer(
               `prediction[3]` > `prediction[4]` &
                 `prediction[3]` > `prediction[5]` &
                 `prediction[3]` > `prediction[7]` &
                 `prediction[6]` > `prediction[4]` &
                 `prediction[6]` > `prediction[5]` &
                 `prediction[6]` > `prediction[7]`
             )) %>%
    summarize(pr_gauche_droite = mean(pr_gauche_droite)) %>%
    mutate(
      t = start_times_2012[j]
    )
  return(pr_gauche_droite)
}) %>%
  do.call("bind_rows", .)
plt2 <- ggplot(pr_gauche_droite, aes(x = t, y = pr_gauche_droite)) +
  geom_line() +
  theme_light() +
  theme(axis.title.x = element_blank()) +
  labs(y = "Pr(Left-right runoff)")
plt <- grid.arrange(plt1, plt2, layout_matrix = rbind(c(1,1,2,2),
                                                      c(1,1,2,2)))
ggsave("plt/paper/2012_mae_prob_runoff_w_shares.png", plt, width = 10, height = 6)

################################################################################
# Figure 3: Forecast by model for the right-wing vote share in 2012

## Separate models
#m1
m1_prediction_2012 <- lapply(seq(3, 17, 1), function(j){
  fit <- read_rds(paste0("dta/fit/m2012_w_shares_", j,".Rds", sep = ""))
  prediction_results <- fit$summary("m1_prediction_new",
                                    ~ quantile(., c(0.025, 0.1,0.25, 0.5, 0.75, 0.9, 0.975))) %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + bloc],
      year = 2012,
      t = start_times_2012[j]
    ) %>%
    left_join(
      election_results_all %>%
        filter(bloc != "Abstention") %>%
        group_by(year) %>%
        mutate(percentage = percentage/sum(percentage, na.rm = TRUE)),
      by = c("bloc", "year")
    )
  return(prediction_results)
}) %>%
  do.call("bind_rows", .) %>%
  mutate(kind = "Polls")

#m2
m2_prediction_2012 <- lapply(seq(3, 17, 1), function(j){
  print(j)
  fit <- read_rds(paste0("dta/fit/m2012_w_shares_", j,".Rds", sep = ""))
  prediction_results <- fit$summary("m2_prediction_new",
                                    ~ quantile(., c(0.025, 0.1,0.25, 0.5, 0.75, 0.9, 0.975))) %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + bloc],
      year = 2012,
      t = start_times_2012[j]
    ) %>%
    left_join(
      election_results_all %>%
        filter(bloc != "Abstention") %>%
        group_by(year) %>%
        mutate(percentage = percentage/sum(percentage, na.rm = TRUE)),
      by = c("bloc", "year")
    )
  return(prediction_results)
}) %>%
  do.call("bind_rows", .) %>%
  mutate(kind = "Historical trend")

#m3
m3_prediction_2012 <- lapply(seq(3, 17, 1), function(j){
  print(j)
  fit <- read_rds(paste0("dta/fit/m2012_w_shares_", j,".Rds", sep = ""))
  prediction_results <- fit$summary("m3_prediction_new",
                                    ~ quantile(., c(0.025, 0.1,0.25, 0.5, 0.75, 0.9, 0.975))) %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + bloc],
      year = 2012,
      t = start_times_2012[j]
    ) %>%
    left_join(
      election_results_all %>%
        filter(bloc != "Abstention") %>%
        group_by(year) %>%
        mutate(percentage = percentage/sum(percentage, na.rm = TRUE)),
      by = c("bloc", "year")
    )
  return(prediction_results)
}) %>%
  do.call("bind_rows", .) %>%
  mutate(kind = "Fundamentals")

predictions_2012 <- bind_rows(
  m1_prediction_2012,
  m2_prediction_2012,
  m3_prediction_2012
)
plt <- ggplot(predictions_2012 %>%
                filter(bloc == "Droite"), aes(x = t, y = `50%`)) +
  geom_line(aes(color = kind)) +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`, fill = kind), alpha = 0.25) +
  #geom_ribbon(aes(ymin = `10%`, ymax = `90%`, fill = kind), width = 0, size = 0.5) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_hline(aes(yintercept = percentage)) +
  theme_light() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank()) +
  labs(y = "Predicted vote share (Right)",
       caption = "Thick line: Median
                  Shaded: 50% quantile")

ggsave("plt/paper/2012_separate_predictions_droite_w_shares.png", plt, width = 10, height = 6)

################################################################################
## Figure 4
prediction <- lapply(seq(3, 17), function(j){
  fit <- read_rds(paste0("dta/fit/m2017_w_shares_", j,".Rds", sep = ""))
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

plt <- ggplot(prediction %>%
                mutate(t = as.Date(t)), aes(x = t, y = `50%`)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = "red", alpha = 0.25) +
  geom_ribbon(aes(ymin = `10%`, ymax = `90%`), fill = "red", alpha = 0.125) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_hline(aes(yintercept = percentage)) +
  theme_light() +
  facet_wrap(bloc ~.) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Predicted vote share",
       caption = "Thick line: Median
                  Deeper/lighter shade: 50/80% quantile")
ggsave("plt/paper/2017_vote_share_prediction_joint.png", plt, width = 10, height = 6)



################################################################################
## Figure 5
start <- 366
end <- 578
start_indicators <- c(3,9,15)
top_line <- c(381, 389, 404, 432, 465, 495, 524)
weeks <- data_list$m1_ix_week
plt_list <- list()
time_points <- c(4, 9, 14)

post_pred_joint <- data.frame()
plt_df_rt_melt_joint <- data.frame()
polls_joint <- data.frame()
election_results_all_joint <- data.frame()
prediction_joint <- data.frame()
for (j in seq(1, 3, 1)){
  print(j)
  fit <- read_rds(paste0("dta/fit/m2017_w_shares_", time_points[j],".Rds", sep = ""))

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
    as.data.frame() %>%
    mutate(date = start_times_2017[time_points[j]])
  post_pred_joint <- bind_rows(post_pred_joint,
                               post_pred)

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
    ) %>%
    mutate(date = start_times_2017[time_points[j]])
  plt_df_rt_melt_joint <- bind_rows(plt_df_rt_melt_joint,
                                    plt_df_rt_melt)

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
           used = ifelse(week > time_points[j], "Excluded", "Included")) %>%
    mutate(date = start_times_2017[time_points[j]])
  polls_joint <- bind_rows(polls_joint,
                           polls)

  election_results_all <- election_results_all %>%
    mutate(d = match(bloc, bloc_vector),
           bloc = factor(bloc, levels = bloc_vector),
           t2 = match(year, c(2002, 2007, 2012, 2017))) %>%
    filter(year %in% c(2002, 2007, 2012, 2017)) %>%
    filter(bloc != "Abstention") %>%
    group_by(year) %>%
    mutate(percentage = percentage/sum(percentage)) %>%
    mutate(date = start_times_2017[time_points[j]])
  election_results_all_joint <- bind_rows(election_results_all_joint,
                                          election_results_all)
  prediction <- fit$summary("prediction", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
    mutate(
      bloc = 1 + as.integer(str_match(variable, "(\\d)")[,2]),
      bloc = ifelse(bloc > 4, bloc + 1, bloc),
      bloc = factor(bloc_vector[bloc], levels = bloc_vector)
    ) %>%
    mutate(t1 = count_weeks, year = 2017) %>%
    mutate(date = start_times_2017[time_points[j]])
  prediction_joint <- bind_rows(prediction_joint,
                                prediction)
}


bloc_choice <- "Droite radicale et extreme droite"
plt <- ggplot() +
  geom_line(data = plt_df_rt_melt_joint %>%
              filter(iter %in% sample(1:1800, 400),
                     year == 2017,
                     bloc != "Ecologisme",
                     bloc == bloc_choice) %>%
              mutate(t1 = start_times_2017[t1]), aes(x = t1, y = draws,
                                                     group = iter,
                                                     colour = 'Sampled model iterations'), alpha = 0.1) +
  geom_line(data = post_pred_joint %>%
              filter(year == 2017,
                     bloc != "Ecologisme",
                     bloc == bloc_choice)%>%
              mutate(t1 = start_times_2017[t1]),
            aes(x = t1, y = pred_mu,
                colour = 'Best estimate')) +
  geom_point(data = polls_joint %>%
               group_by(poll_id, date) %>%
               mutate(prob = prob/sum(prob)) %>%
               filter(year == 2017,
                      bloc != "Ecologisme",
                      bloc == bloc_choice) %>%
               mutate(t1 = start_times_2017[t1] + sample(-2:2, n(), replace = TRUE)),
             aes(x = t1, y = prob, shape = used)) +
  geom_point(data = prediction_joint %>%
               filter(bloc == bloc_choice) %>%
               mutate(t1 = start_times_2017[t1]),
             aes(x = t1, y = `50%`), color = "black", size = 0.7) +
  geom_errorbar(data = prediction_joint %>%
                  filter(bloc == bloc_choice) %>%
                  mutate(t1 = start_times_2017[t1]), aes(x = t1, ymin = `25%`, ymax = `75%`),
                color = "black", size = 0.5, width = 0) +
  geom_errorbar(data = prediction_joint %>%
                  filter(bloc == bloc_choice) %>%
                  mutate(t1 = start_times_2017[t1]), aes(x = t1, ymin = `10%`, ymax = `90%`),
                color = "black", size = 0.25, width = 0) +
  theme_bw() + theme(legend.position="bottom") +
  geom_hline(data = election_results_all_joint %>%
               filter(year == 2017,
                      bloc != "Ecologisme",
                      bloc == bloc_choice), aes(yintercept = percentage)) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank()) +
  ylab('Predicted vote share') +
  geom_vline(data = data.frame(date = start_times_2017[time_points],
                               intercept = start_times_2017[time_points]),
             aes(xintercept = intercept), linetype = 2) +
  facet_grid(. ~ date) +
  lims(y = c(0.13, 0.35)) +
  scale_shape_manual(values = c(3, 16))

ggsave("plt/paper/2017_droite_radical_included_excluded.png", plt)

################################################################################
## Figure 6

## Prediction according to the fundamentals model
m1_prediction_2017 <- lapply(seq(3, 17), function(j){
  fit <- read_rds(paste0("dta/fit/m2017_w_shares_", j,".Rds", sep = ""))
  prediction_results <- fit$summary("m1_prediction_new",
                                    ~ quantile(., c(0.1,0.25, 0.5, 0.75, 0.9))) %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + ifelse(bloc > 3, bloc + 1, bloc)],
      year = 2017,
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
m2_prediction_2017 <- lapply(seq(3, 17), function(j){
  fit <- read_rds(paste0("dta/fit/m2017_w_shares_", j,".Rds", sep = ""))
  prediction_results <- fit$summary("m2_prediction_new",
                                    ~ quantile(., c(0.1,0.25, 0.5, 0.75, 0.9))) %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + ifelse(bloc > 3, bloc + 1, bloc)],
      year = 2017,
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
m3_prediction_2017 <- lapply(seq(3, 17), function(j){
  fit <- read_rds(paste0("dta/fit/m2017_w_shares_", j,".Rds", sep = ""))
  prediction_results <- fit$summary("m3_prediction_new",
                                    ~ quantile(., c(0.1,0.25, 0.5, 0.75, 0.9))) %>%
    mutate(
      bloc = as.integer(str_match(variable, "\\[(\\d+)")[,2]),
      bloc = bloc_vector[1 + ifelse(bloc > 3, bloc + 1, bloc)],
      year = 2017,
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

prediction_2017 <- bind_rows(
  m1_prediction_2017 %>%
    mutate(kind = "Polling"),
  m2_prediction_2017 %>%
    mutate(kind = "Historical"),
  m3_prediction_2017 %>%
    mutate(kind = "Fundamentals")
)


plt <- ggplot(prediction_2017 %>%
                filter(bloc == "Droite"), aes(x = t, y = `50%`)) +
  geom_line(aes(color = kind)) +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`, fill = kind), alpha = 0.25) +
  #geom_ribbon(aes(ymin = `10%`, ymax = `90%`, fill = kind), width = 0, size = 0.5) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_hline(aes(yintercept = percentage)) +
  theme_light() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank()) +
  labs(y = "Predicted vote share (Right)",
       caption = "Thick line: Median
                  Shaded: 50% quantile")

ggsave("plt/paper/2017_separate_predictions_droite_w_shares.png", plt, width = 10, height = 6)


################################################################################
## Figure 7: 2022_matchup_probability
## 2022
library(tidyverse)
fit <- read_rds("dta/fit/m2022_w_shares.Rds")
data_list <- read_rds("dta/fit/m2022_data_list_w_shares.Rds")
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




################################################################################
## Figure 8: Predicted change in vote intentions for the 2022 election eight weeks out
fit <- read_rds("dta/fit/m2022_w_shares.Rds")
data_list <- read_rds("dta/fit/m2022_data_list_w_shares.Rds")
indicators <- read_rds("dta/fit/m2022_indicator.Rds")
df <- read_rds("dta/fit/m2022_polls.rds")
election_results_all <- read_rds("dta/fit/m2022_election_results_all.rds")
pollster_vector <- read_rds("dta/fit/m2022_pollster_vector.rds")
m2_year <- read_rds("dta/fit/m2022_m2_year.rds")
m2_df_results <- read_rds("dta/fit/m2022_m2_df_results.rds")

candidate_vector <- c("Melenchon", "Hidalgo", "Jadot", "Macron", "Pecresse", "Le Pen",
  "Zemmour")

plt_df_rt_melt <- fit$draws("m1_y2") %>%
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
  ) %>%
  filter(year == 2022)

m1_ex_droite <- fit$draws("m1_ex_droite") %>%
  posterior::as_draws_df() %>%
  select(!contains(".")) %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = c("t1"),
               values_to = "draws",
               names_pattern = "\\[([\\d]+)") %>%
  mutate(prob_le_pen = inv.logit(draws)) %>%
  select(iter, t1, prob_le_pen) %>%
  mutate(kind = "le pen")
m1_ex_droite <- m1_ex_droite %>%
  bind_rows(
    m1_ex_droite %>%
      mutate(kind = "zemmour",
             prob_le_pen = 1 - prob_le_pen)
  ) %>%
  mutate(d = "7", t1 = as.integer(t1))

plt_df_rt_melt <- plt_df_rt_melt %>%
  left_join(m1_ex_droite,
            by = c("d", "iter", "t1")) %>%
  mutate(d = ifelse(is.na(kind), d, ifelse(kind == "zemmour", 8, 7)),
         draws = ifelse(!is.na(kind), draws * prob_le_pen, draws),
         bloc = ifelse(is.na(kind), as.character(bloc),
                ifelse(kind == "le pen", paste(bloc, "-", "Le Pen"),
                       paste(bloc, "-", "Zemmour")))) %>%
  filter(bloc != "Autre") %>%
  mutate(candidates = candidate_vector[as.integer(d) - 1])

post_pred <- plt_df_rt_melt %>%
  group_by(t1, t2, year, candidates, d) %>%
  summarize(pred_mu = mean(draws))


df <- read_csv(file = "dta/polls_dta/election_season_model/poll_input.csv")
df_le_pen_zemmour <- read_csv(
  file = "dta/polls_dta/election_season_model/poll_input_le_pen_zemmour.csv")

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
  ungroup() %>%
  filter(election_year == 2022)

polls <- df %>%
  left_join(
    df_le_pen_zemmour %>%
      mutate(bloc_id = 8) %>%
      select(bloc_id, poll_id, candidate, y) %>%
      rename(y_ex_droite = y),
    by = c("poll_id", "bloc_id")
  ) %>%
  mutate(
    bloc_id = ifelse(is.na(candidate), bloc_id,
              ifelse(candidate == "Zemmour", 9, 8)),
    candidates = candidate_vector[ifelse(bloc_id > 2, bloc_id - 2, 10)]
  ) %>%
  mutate(y = ifelse(is.na(y_ex_droite), y, y_ex_droite)) %>%
  rename(t1 = start_day) %>%
  select(t1, candidates, y, poll_id) %>%
  group_by(poll_id) %>%
  mutate(prob = y/sum(y)) %>%
  filter(!((candidates == "Le Pen") & (prob > 0.28))) %>%
  filter(!is.na(candidates))
View(polls)
#
#
# prediction <- fit$summary("prediction_adjusted", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
#   mutate(
#     d = as.integer(str_match(variable, "(\\d)")[,2])
#   ) %>%
#   filter(d != 1) %>%
#   mutate(
#     candidates = candidate_vector[d - 1]
#   ) %>%
#   mutate(t1 = count_weeks, year = 2022)

cols <- c("Melenchon" = "brown4",
          "Hidalgo" ="brown2",
          "Jadot" = "limegreen",
          "Macron" ="gold",
          "Pecresse" ="blue",
          "Le Pen" ="navyblue",
          "Zemmour" = "orange")
plt <- ggplot() +
  geom_line(data = plt_df_rt_melt %>%
              filter(year == 2022) %>%
              filter(iter %in% sample(1:1800, 400)), aes(x = start_times_2022[t1], y = draws,
                                                         group = interaction(iter, candidates),
                                                         color = candidates),
            alpha = 0.06, size = 0.5) +
  geom_line(data = post_pred %>%
              filter(year == 2022),
            aes(x = start_times_2022[t1], y = pred_mu,
                color = candidates), size = 1, alpha = 1.2) +
  theme_bw() +
  geom_point(data = polls, aes(x = t1, y = prob, color = candidates)) +
  theme(legend.position="bottom") +
  geom_hline(data = election_results_all %>%
               filter(year == 2022), aes(yintercept = percentage)) +
  theme(axis.title.x = element_blank(), legend.title = element_blank()) +
  labs(y = 'Estimated vote intentions',
       caption = "Thick line: Mean
                  Thin lines: Sampled iterations") +
  geom_vline(data = data.frame(
    blocs = bloc_vector,
    intercept = start_times_2022[10] + 1),
    aes(xintercept = intercept), linetype = 2) +
  geom_vline(data = data.frame(
    blocs = bloc_vector,
    intercept = start_times_2022[18]),
    aes(xintercept = intercept)) +
  scale_color_manual(values = cols)
ggsave("plt/paper/2022_vote_intentions_polling_model_w_shares.png",
       plt, width = 10, height = 6)

################################################################################
## Figure 9:
fit <- read_rds("dta/fit/m2022_w_shares.Rds")
prediction_aggregate <- fit$draws("prediction") %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = "bloc",
               values_to = "draws",
               names_pattern = "([\\d]+)") %>%
  filter(!is.na(bloc)) %>%
  mutate(bloc = bloc_vector[1 + as.integer(bloc)]) %>%
  filter(bloc %in% c("Centre", "Droite", "Gauche radicale et extreme gauche")) %>%
  mutate(kind = "Aggregate")
#m1
prediction_m1 <- fit$draws("m1_prediction_new") %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = "bloc",
               values_to = "draws",
               names_pattern = "\\[([\\d]+)") %>%
  filter(!is.na(bloc)) %>%
  mutate(bloc = bloc_vector[1 + as.integer(bloc)]) %>%
  filter(bloc %in% c("Centre", "Droite", "Gauche radicale et extreme gauche")) %>%
  mutate(kind = "Polls")
#m2
prediction_m2 <- fit$draws("m2_prediction_new") %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = "bloc",
               values_to = "draws",
               names_pattern = "\\[([\\d]+)") %>%
  filter(!is.na(bloc)) %>%
  mutate(bloc = bloc_vector[1 + as.integer(bloc)]) %>%
  filter(bloc %in% c("Centre", "Droite", "Gauche radicale et extreme gauche")) %>%
  mutate(kind = "Historical")
#m3
prediction_m3 <- fit$draws("m3_prediction_new") %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = "bloc",
               values_to = "draws",
               names_pattern = "\\[([\\d]+)") %>%
  filter(!is.na(bloc)) %>%
  mutate(bloc = bloc_vector[1 + as.integer(bloc)]) %>%
  filter(bloc %in% c("Centre", "Droite", "Gauche radicale et extreme gauche")) %>%
  mutate(kind = "Fundamentals")

prediction <- bind_rows(
  prediction_aggregate,
  prediction_m1,
  prediction_m2,
  prediction_m3
) %>%
  filter(draws > 0) %>%
  mutate(candidates =
           case_when(
             bloc == "Centre" ~ "Macron",
             bloc == "Droite" ~ "Pecresse",
             TRUE ~ "Melenchon")) %>%
  mutate(
    kind = factor(kind,
                  levels = c(
                    "Polls",
                    "Historical",
                    "Fundamentals",
                    "Aggregate"
                  ))
  )

group.colors <- c(
  "Aggregate" = "Red",
  "Polls"     = "Green",
  "Historical" = "Blue",
  "Fundamentals" = "Yellow")

plt <- ggplot(prediction, aes(x = draws, fill = kind)) +
  geom_histogram(bins = 150, alpha = 0.3, position = "identity") +
  facet_grid(. ~ candidates) +
  theme_light() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") +
  labs(x = "Predicted vote share") +
  scale_fill_manual(values = group.colors)
plt
ggsave("plt/paper/2022_centre_droite_gauche_radicale.png",
       plt, width = 10, height = 6)





























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

Sys.setlocale("LC_TIME", "C")
plt <- ggplot(prediction %>%
                mutate(t = as.Date(t)), aes(x = t, y = `50%`)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = "red", alpha = 0.25) +
  geom_ribbon(aes(ymin = `10%`, ymax = `90%`), fill = "red", alpha = 0.125) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_hline(aes(yintercept = percentage)) +
  theme_light() +
  facet_wrap(bloc ~.) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Vote share",
       caption = "Median/50%/80%")
ggsave("plt/paper/2017_vote_share_prediction_joint.png", plt, width = 10, height = 6)

## 2) Change in prediction over time for 2017 model 1
# The models have to be rerun because the code didn't update the unemployment data.
prediction <- lapply(c(3,9,15), function(j){
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
  labs(y = "Vote share",
       caption = "Median/50%/80%")
ggsave("plt/paper/2017_vote_share_prediction_m1.png", plt, width = 10, height = 6)


## 3) Change in prediction over time for 2017 model 2
# The models have to be rerun because the code didn't update the unemployment data.
prediction <- lapply(c(3,9,15), function(j){
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
prediction <- lapply(c(3,9,15), function(j){
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





################################################################################
## 2017


################################################################################
## Current election

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
              filter(year == 2022) %>%
              filter(iter %in% sample(1:1800, 100)), aes(x = start_times_2012[t1], y = draws,
                                                         group = iter,
                                                         colour = 'Sampled model iterations'), alpha = 0.1) +
  geom_line(data = post_pred %>%
              filter(year == 2022),
            aes(x = start_times_2012[t1], y = pred_mu,
                colour = 'Average prediction')) +
  # geom_point(data = polls %>%
  #              filter(year == 2022) %>%
  #              group_by(poll_id) %>%
  #              mutate(prob = prob/sum(prob)), aes(x = start_times_2012[t1], y = prob)) +
  geom_point(data = prediction %>%
               filter(year == 2022), aes(x = start_times_2012[t1], y = `50%`), color = "black") +
  geom_errorbar(data = prediction %>%
                  filter(year == 2022), aes(x = start_times_2012[t1], ymin = `25%`, ymax = `75%`),
                color = "black", size = 0.5, width = 0) +
  geom_errorbar(data = prediction %>%
                  filter(year == 2022), aes(x = start_times_2012[t1], ymin = `10%`, ymax = `90%`),
                color = "black", size = 0.25, width = 0) +
  geom_point(data = prediction %>%
               filter(year == 2022), aes(x = start_times_2012[t1], y = `50%`), color = "black") +
  theme_bw() + theme(legend.position="bottom") +
  geom_hline(data = election_results_all %>%
               filter(year == 2022), aes(yintercept = percentage)) +
  theme(axis.title.x = element_blank(), legend.title = element_blank()) +
  labs(y = 'Estimated vote intentions',
       caption = "Black dot and whisks are the median aggregate prediction with 50/80% quantile") +
  geom_vline(data = data.frame(
    blocs = bloc_vector,
    intercept = start_times_2012[10]),
    aes(xintercept = intercept), linetype = 2) +
  facet_grid(bloc ~ ., scales = "free_y")
p


ggsave("plt/paper/2022_vote_intentions_polling_model.png",
       p, width = 10, height = 6)






















































































































































































































































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




