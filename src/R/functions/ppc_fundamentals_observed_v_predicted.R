ppc_fundamentals_observed_v_predicted <- function(fit, df, bloc_vector, election_years){
  hat_YVoteshare <- fit$summary("hat_YVoteshare", ~quantile(., c(0.10, 0.25, 0.5, 0.75, 0.9))) %>%
    mutate(
      obs_id = 1:n()
    ) %>%
    select(-variable) %>%
    left_join(
      df %>%
        arrange(bloc_id, election_id, department_id) %>%
        mutate(obs_id = 1:n()),
      by = c("obs_id" = "obs_id")
    ) %>%
    mutate(bloc = bloc_vector[bloc_id],
           year = election_years[election_id])
  ## Set colors
  cols <- c("Gauche radicale et extreme gauche" = "brown4",
            "Gauche" ="brown2",
            "Centre" ="gold",
            "Droite" ="blue",
            "Droite radicale et extreme droite" ="navyblue")

  plt <- ggplot(hat_YVoteshare, aes(x = percentage, y = `50%`, color = bloc)) +
    geom_point() +
    geom_errorbar(aes(ymin = `25%`, ymax = `75%`), size =0.5, width = 0) +
    geom_errorbar(aes(ymin = `10%`, ymax = `90%`), size =0.25, width = 0) +
    facet_grid(bloc ~ year) +
    theme_light() +
    scale_color_manual(values = cols) +
    labs(x = "Observed", y = "Predicted") +
    theme(legend.title = element_blank())
  return(plt)
}