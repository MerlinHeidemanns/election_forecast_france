
fit <- read_rds(paste("dta/fit/m2012_", start_indicator,".Rds", sep = ""))

prediction <- lapply(c(4,5,6,7,8,9,10,11,12,13,14,15,16,17), function(j){
  fit <- read_rds(paste0("dta/fit/m2012_", j,".Rds", sep = ""))
  prediction_results <- fit$summary("prediction",
                                    ~ quantile(., c(0.1,0.25, 0.5, 0.75, 0.9))) %>%
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

ggplot(prediction_results, aes(x = percentage, y = `50%`)) +
  geom_point() +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`), width = 0, size = 0.75) +
  geom_errorbar(aes(ymin = `10%`, ymax = `90%`), width = 0, size = 0.5) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  theme_light()

ggplot(prediction, aes(x = t, y = `50%`)) +
  geom_point() +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`), width = 0, size = 0.75) +
  geom_errorbar(aes(ymin = `10%`, ymax = `90%`), width = 0, size = 0.5) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_hline(aes(yintercept = percentage)) +
  theme_light() +
  facet_wrap(bloc ~.)


prediction <- lapply(c(3,9,15), function(j){
  fit <- read_rds(paste0("dta/fit/m2017_", j,".Rds", sep = ""))
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


ggplot(prediction, aes(x = t, y = `50%`)) +
  geom_point() +
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`), width = 0, size = 0.75) +
  geom_errorbar(aes(ymin = `10%`, ymax = `90%`), width = 0, size = 0.5) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_hline(aes(yintercept = percentage)) +
  theme_light() +
  facet_wrap(bloc ~.)








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
         year = c(2002, 2007, 2012, 2017)[t2])  %>%
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
    year = c(2002, 2007, 2012, 2017)[t2]
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
         year = c(2002, 2007, 2012, 2017)[t2])

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
  mutate(t1 = count_weeks, year = 2017)

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





