#' ppc_obs_theta_plt_hist
#' @param df_out Dataframe containing posterior draws in theta_cond and
#' candidate identifiers in candidate_id

ppc_obs_theta_plt_hist <- function(df_out){
  plt <- ggplot(df_out, aes(x = theta_cond, fill = candidate_id)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
    theme_light() +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank()) +
    labs(x = "Vote share")
  return(plt)
}
