#' ppc_plt_alpha
#' @param fit Cmdstan object
#' @param true_alpha If available, dataframe of p, r, alpha (alpha being the true
#' value)
#' @return plt
ppc_plt_alpha <- function(fit, true_alpha = NULL){
  alpha_hat_true <- fit$draws("alpha") %>%
    posterior::as_draws_df() %>%
    pivot_longer(
      everything(),
      names_to = "cr",
      values_to = "draws"
    ) %>%
    mutate(
      candidate_id = str_match(cr, "(\\d+),")[, 2],
      pollster_id = as.integer(str_match(cr, ",(\\d+)")[, 2])
    ) %>%
    filter(!is.na(pollster_id)) %>%
    dplyr::select(-cr)
  plt1_data <- alpha_hat_true %>%
    group_by(candidate_id, pollster_id) %>%
    summarize(
      q50 = quantile(draws, 0.5),
      q25 = quantile(draws, 0.25),
      q75 = quantile(draws, 0.75),
      q10 = quantile(draws, 0.10),
      q90 = quantile(draws, 0.90)
    ) %>%
    left_join(true_alpha) %>%
    ungroup() %>%
    arrange(alpha) %>%
    mutate(party_pollster = paste(candidate_id, pollster_id, sep = "."),
           party_pollster = factor(party_pollster, levels = party_pollster))

  #' Plot
  plt1 <- ggplot(plt1_data, aes(x = party_pollster)) +
    geom_point(aes(y = q50)) +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75,
                  width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5,
                  width = 0) +
    theme_light() +
    labs(
      x = "Party - Pollster (sorted by true alpha)",
      y = "Polling house deviation",
      caption = "Log-odds scale"
    ) +
    geom_point(aes(y = alpha), color = "red", shape = 3) +
    coord_flip()

  #' Plot 2
  plt2_data <-  alpha_hat_true %>%
    left_join(true_alpha) %>%
    group_by(candidate_id, pollster_id, alpha) %>%
    summarize(
      q50 = quantile(alpha - draws, 0.5),
      q25 = quantile(alpha - draws, 0.25),
      q75 = quantile(alpha - draws, 0.75),
      q10 = quantile(alpha - draws, 0.10),
      q90 = quantile(alpha - draws, 0.90)
    ) %>%
    ungroup() %>%
    arrange(alpha) %>%
    mutate(party_pollster = paste(candidate_id, pollster_id, sep = "."),
           party_pollster = factor(party_pollster, levels = party_pollster))

  plt2 <- ggplot(plt2_data, aes(x = party_pollster, y = q50)) +
    geom_point() +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75,
                  width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5,
                  width = 0) +
    theme_light() +
    labs(
      x = "Party - Pollster (sorted by true alpha)",
      y = "true - estimate",
      caption = "Log-odds scale"
    ) +
    coord_flip() +
    geom_hline(aes(yintercept = 0), linetype = 2, color = "red")

  #' Plot 3
  plt3_data <-  alpha_hat_true %>%
    left_join(true_alpha) %>%
    group_by(candidate_id, pollster_id, alpha) %>%
    summarize(
      q50 = quantile(alpha - draws, 0.5),
      q25 = quantile(alpha - draws, 0.25),
      q75 = quantile(alpha - draws, 0.75),
      q10 = quantile(alpha - draws, 0.10),
      q90 = quantile(alpha - draws, 0.90)
    ) %>%
    ungroup() %>%
    arrange(q50) %>%
    mutate(party_pollster = paste(candidate_id, pollster_id, sep = "."),
           party_pollster = factor(party_pollster, levels = party_pollster))

  plt3 <- ggplot(plt3_data, aes(x = party_pollster, y = q50)) +
    geom_point() +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75,
                  width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5,
                  width = 0) +
    theme_light() +
    labs(
      x = "Party - Pollster (sorted by error)",
      y = "true - estimate",
      caption = "Log-odds scale"
    ) +
    coord_flip() +
    geom_hline(aes(yintercept = 0), linetype = 2, color = "red")

  ## Plot 4
  plt4_data <-  alpha_hat_true %>%
    left_join(true_alpha) %>%
    group_by(candidate_id, pollster_id, alpha) %>%
    summarize(
      q50 = quantile(alpha - draws, 0.5),
      q25 = quantile(alpha - draws, 0.25),
      q75 = quantile(alpha - draws, 0.75),
      q10 = quantile(alpha - draws, 0.10),
      q90 = quantile(alpha - draws, 0.90)
    ) %>%
    ungroup() %>%
    group_by(candidate_id) %>%
    arrange(q50)
  plt4 <- ggplot(plt4_data, aes(x = pollster_id, y = q50)) +
    geom_point() +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75,
                  width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5,
                  width = 0) +
    theme_light() +
    labs(
      x = "Party - Pollster (sorted by error)",
      y = "true - estimate",
      caption = "Log-odds scale"
    ) +
    coord_flip() +
    geom_hline(aes(yintercept = 0), linetype = 2, color = "red") +
    facet_wrap(candidate_id ~ ., nrow = 1)


  plt <- gridExtra::grid.arrange(plt1, plt2, plt3, plt4, layout_matrix = rbind(c(1,1,2,2,3,3),
                                                                  c(1,1,2,2,3,3),
                                                                  c(1,1,2,2,3,3),
                                                                  c(4,4,4,4,4,4),
                                                                  c(4,4,4,4,4,4)))


  return(plt)
}