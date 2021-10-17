#' ppc_plt_epsilon
#' @returns Plot with the poll level error

ppc_plt_epsilon <- function(fit, data_list){
  epsilon_past <- fit$summary("epsilon_past",
                              ~ quantile(.,
                                         c(0.1, 0.25, 0.5, 0.75, 0.9)))
  colnames(epsilon_past) <- c("variable", "q10", "q25", "q50", "q75", "q90")
  epsilon_past <- epsilon_past %>%
    mutate(
      bloc_id = str_match(variable, "([\\d]+),")[,2],
      order_id = as.integer(str_match(variable, ",([\\d]+)")[,2])
    ) %>%
    left_join(
      data.frame(
        order_id = 1:sum(data_list$NPolls_Pollster_past),
        pollster_id = data_list$id_P_pollster_past,
        time_id = data_list$id_P_time_past
      )
    )

  ggplot(epsilon_past, aes(x = time_id, y = q50, color = as.factor(pollster_id))) +
    geom_point() +
    geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size = 0.5) +
    geom_errorbar(aes(ymin = q10, ymax = q90), width = 0, size = 0.25) +
    facet_wrap(bloc_id ~ .) +
    theme_light() +
    geom_vline(data = data.frame(
      election_time_id = data_list$id_E_time
    ),aes(xintercept = election_time_id)) +
    labs(y = "Error", x = "t") +
    theme(legend.position = "none")
}
