#' ppc_plt_alpha_past
#' @param fit Cmdstan object
#' @param data_list If available, dataframe of p, r, alpha (alpha being the true
#' value)
#' @return plt
ppc_plt_alpha_past <- function(fit, data_list, data_polls){
  pollster_election_crosswalk <- data.frame(
    pollster_election_id = data_list$id_P_pollster_past,
    election_id = data_list$id_P_elections_past
  ) %>%
    distinct() %>%
    arrange(election_id, pollster_election_id) %>%
    group_by(election_id) %>%
    mutate(pollster_id = 1:n())

  alpha_hat_true <- fit$draws("alpha_past") %>%
    posterior::as_draws_df() %>%
    pivot_longer(
      everything(),
      names_to = "ber",
      values_to = "draws"
    ) %>%
    mutate(
      bloc_id = as.integer(str_match(ber, "(\\d+),")[, 2]),
      pollster_election_id = as.integer(str_match(ber, ",(\\d+)")[, 2]),
    ) %>%
    filter(!is.na(pollster_election_id)) %>%
    left_join(pollster_election_crosswalk) %>%
    dplyr::select(-ber) %>%
    left_join(data_polls$polls_past %>%
                group_by(election_id, pollster_id) %>%
                summarize(N = n()) %>%
                mutate(N = N/6)) %>%
    left_join(data_polls$polls_past %>%
                distinct(bloc_id, election_id, pollster_id, true_alpha))




  plt1_data <- alpha_hat_true %>%
    group_by(election_id, bloc_id, pollster_id) %>%
    summarize(
      q50 = quantile(true_alpha - draws, 0.5),
      q25 = quantile(true_alpha - draws, 0.25),
      q75 = quantile(true_alpha - draws, 0.75),
      q10 = quantile(true_alpha - draws, 0.10),
      q90 = quantile(true_alpha - draws, 0.90)
    ) %>%
    ungroup() %>%
    arrange(q50) %>%
    mutate(election_bloc_pollster = paste(election_id, bloc_id, pollster_id, sep = "."),
           election_bloc_pollster = factor(election_bloc_pollster, levels = election_bloc_pollster))

  #' Plot
  plt1 <- ggplot(plt1_data, aes(x = election_bloc_pollster,
                                color = as.factor(election_id),
                                shape = as.factor(pollster_id))) +
    geom_point(aes(y = q50)) +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75,
                  width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5,
                  width = 0) +
    geom_hline(aes(yintercept = 0),linetype = 2, color = "black") +
    theme_light() +
    theme(axis.title.x = element_blank()) +
    labs(
      y = "Polling house deviation",
      color = "Election",
      shape = "Bloc",
      caption = "Log-odds scale"
    ) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank())

  plt2_data <- alpha_hat_true %>%
    group_by(election_id, bloc_id, pollster_id) %>%
    summarize(
      q50 = quantile(abs(true_alpha - draws), 0.5),
      q25 = quantile(abs(true_alpha - draws), 0.25),
      q75 = quantile(abs(true_alpha - draws), 0.75),
      q10 = quantile(abs(true_alpha - draws), 0.10),
      q90 = quantile(abs(true_alpha - draws), 0.90)
    ) %>%
    ungroup() %>%
    arrange(q50) %>%
    mutate(election_bloc_pollster = paste(election_id, bloc_id, pollster_id, sep = "."),
           election_bloc_pollster = factor(election_bloc_pollster, levels = election_bloc_pollster))

  #' Plot
  plt2 <- ggplot(plt2_data, aes(x = election_bloc_pollster,
                                color = as.factor(election_id),
                                shape = as.factor(pollster_id))) +
    geom_point(aes(y = q50)) +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75,
                  width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5,
                  width = 0) +
    geom_hline(aes(yintercept = 0),linetype = 2, color = "black") +
    theme_light() +
    labs(
      y = "Polling house deviation",
      color = "Election",
      shape = "Bloc",
      caption = "Log-odds scale"
    ) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank())


  #' Ordered by number of associated polls
  plt3_data <- alpha_hat_true %>%
    group_by(election_id, pollster_id, N) %>%
    summarize(
      q50 = quantile(abs(true_alpha - draws), 0.5),
      q25 = quantile(abs(true_alpha - draws), 0.25),
      q75 = quantile(abs(true_alpha - draws), 0.75),
      q10 = quantile(abs(true_alpha - draws), 0.10),
      q90 = quantile(abs(true_alpha - draws), 0.90)
    ) %>%
    ungroup() %>%
    arrange(N, q50) %>%
    mutate(election_pollster = paste(election_id, pollster_id, sep = "."),
           election_pollster = factor(election_pollster, levels = election_pollster))

  #' Plot
  plt3 <- ggplot(plt3_data %>% mutate(N = N + rnorm(n(), 0, 0.1)), aes(x = N,
                                color = as.factor(election_id))) +
    geom_point(aes(y = q50)) +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75,
                  width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5,
                  width = 0) +
    geom_hline(aes(yintercept = 0),linetype = 2, color = "black") +
    theme_light() +
    labs(
      y = "Polling house deviation",
      x = "left to right more polls",
      color = "Election",
      caption = "Log-odds scale"
    ) +
    theme(legend.position = "none")



  plt <- gridExtra::grid.arrange(plt1, plt2, plt3, layout_matrix = rbind(c(1,1,3),
                                                                   c(2,2,3)))


  return(plt)
}