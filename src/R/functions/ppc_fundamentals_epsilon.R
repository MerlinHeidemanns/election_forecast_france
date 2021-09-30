#' ppc_fundamentals_epsilon
#' @param fit cmdstar fit
ppc_fundamentals_epsilon <- function(fit){
  epsilon <- fit$draws("epsilon") %>%
    posterior::as_draws_df() %>%
    pivot_longer(everything(),
                 names_to = "variable",
                 values_to = "draws") %>%
    mutate(
      election_id = as.integer(str_match(variable, ",([\\d+])")[,2]) + 1,
      bloc_id = as.integer(str_match(variable, "([\\d+]),")[,2])
    ) %>%
    filter(!is.na(election_id)) %>%
    dplyr::select(-variable)

  ## Election ID
  epsilon_election_id <- epsilon %>%
    group_by(election_id) %>%
    summarize(
      q50 = quantile(draws, 0.5),
      q25 = quantile(draws, 0.25),
      q75 = quantile(draws, 0.75),
      q10 = quantile(draws, 0.1),
      q90 = quantile(draws, 0.9)
    )
  plt_election_id <- ggplot(epsilon_election_id, aes(x = as.factor(election_id), y = q50)) +
    geom_point() +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.5, width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
    theme_light() +
    labs(x = "Election id", y = "y - y_hat")

  ## Election ID x Bloc ID
  epsilon_election_id_bloc_id <- epsilon %>%
    group_by(election_id, bloc_id) %>%
    summarize(
      q50 = quantile(draws, 0.5),
      q25 = quantile(draws, 0.25),
      q75 = quantile(draws, 0.75),
      q10 = quantile(draws, 0.1),
      q90 = quantile(draws, 0.9)
    )
  plt_election_id_bloc_id <- ggplot(epsilon_election_id_bloc_id,
                                    aes(x = as.factor(election_id),
                                        y = q50, color = as.factor(bloc_id))) +
    geom_point(position = position_dodge(width = 0.4)) +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.5, width = 0,
                  position = position_dodge(width = 0.4)) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0,
                  position = position_dodge(width = 0.4)) +
    theme_light() +
    labs(x = "Election id", y = "y - y_hat", color = "Bloc id") +
    theme(legend.position = "bottom",
          legend.box="vertical",
          legend.margin=margin())+
    guides(color=guide_legend(nrow=1, byrow=TRUE))

  ## Bloc ID
  epsilon_bloc_id <- epsilon %>%
    group_by(bloc_id) %>%
    summarize(
      q50 = quantile(draws, 0.5),
      q25 = quantile(draws, 0.25),
      q75 = quantile(draws, 0.75),
      q10 = quantile(draws, 0.1),
      q90 = quantile(draws, 0.9)
    )
  plt_bloc_id <- ggplot(epsilon_bloc_id,
                        aes(x = as.factor(bloc_id),
                            y = q50)) +
    geom_point() +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.5, width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
    theme_light() +
    labs(x = "Bloc id", y = "y - y_hat")

  ## Joined plt
  plt <- gridExtra::grid.arrange(plt_bloc_id,
                                 plt_election_id,
                                 plt_election_id_bloc_id,
                                 layout_matrix = rbind(
                                   c(2,2,2,1,1),
                                   c(3,3,3,1,1)
                                 ))

  return(plt)
}