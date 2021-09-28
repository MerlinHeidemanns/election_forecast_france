#' ppc_obs_theta_bloc_politiques
#' @param fit Cmdstan object
#' Summarizes first choice support across all candidates

ppc_obs_theta_bloc_politiques <- function(fit){
  #' Import support vector
  #' Candidate names, time points, political bloc identifiers
  supvec_names <- read.csv("dta/polls_dta/candidate_identifiers.csv") %>%
    pull(candidate)
  supvec_time <- read_csv("dta/polls_dta/time_identifiers.csv") %>%
    pull(end_date)
  supvec_bloc <- read.csv("dta/polls_dta/candidate_party_identifiers.csv")
  #' Pull out theta, add names/times

  pi_theta <- fit$draws("prob_theta") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "var",
                 values_to = "val") %>%
    mutate(
      candidate = supvec_names[as.integer(str_match(var, "(\\d+),")[,2])],
      time = supvec_time[as.integer(str_match(var, ",(\\d+)")[,2])]
    ) %>%
    filter(!is.na(candidate))
  #' Check that all candidates on candidate list are represented
  NCandidate_fit <- pi_theta %>%
    distinct(candidate) %>%
    nrow()
  NCandidate_total <- length(supvec_names)
  if (NCandidate_fit < NCandidate_total){
    stop("Fewer candidates in fit than in name list.")
  }
  #' Check that all time points on list are represented
  NTime_fit <- pi_theta %>%
    distinct(time) %>%
    nrow()
  NTime_total <- length(supvec_time)
  if (NTime_fit < NTime_total){
    stop("Fewer time points in fit than in time point list.")
  }
  #' Summarize blocs
  #' Summarize
  pi_theta <- pi_theta  %>%
    left_join(supvec_bloc) %>%
    group_by(iter, bloc_politiques, time) %>%
    summarize(val = sum(val)) %>%
    group_by(bloc_politiques, time) %>%
    mutate(val = val * 100) %>%
    summarize(
      q50 = quantile(val, 0.5),
      q25 = quantile(val, 0.25),
      q75 = quantile(val, 0.75),
      q10 = quantile(val, 0.10),
      q90 = quantile(val, 0.90)
    ) %>%
    mutate(bloc_politiques = factor(bloc_politiques,
                                    levels = c("Abstention",
                                               "Extreme gauche a gauche",
                                               "Gauche a centre gauche",
                                               "Centre gauche a centre droit",
                                               "Centre droit a droite",
                                               "Droite a extreme droite")))
  plt <- ggplot(pi_theta,
                aes(x = time,
                    y = q50)) +
    geom_line(aes(color = bloc_politiques)) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = bloc_politiques),
                alpha = 0.5) +
    geom_ribbon(aes(ymin = q10, ymax = q90, fill = bloc_politiques),
                alpha = 0.25) +
    scale_fill_manual(values = c("red4", "red", "yellow", "blue", "blue4")) +
    scale_color_manual(values = c("red4", "red", "yellow", "blue", "blue4")) +
    theme_light() +
    lims(y = c(0, NA)) +
    labs(y = "Joint support over all candidates") +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank())
  return(plt)
}