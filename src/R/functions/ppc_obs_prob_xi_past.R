#' ppc_obs_prob_xi_past
#' @param fit cmdstan fit object

ppc_obs_prob_xi_past <- function(fit){
  ## Load candidate and election result vectors
  candidate_id_df <- read_csv("dta/polls_dta/candidate_identifiers_long.csv")
  election_results <- read_csv("dta/polls_dta/election_results_clean.csv") %>%
    mutate(candidate = str_replace_all(candidate, "\\-", " "))

  ## Get draws
  #' indicate iterations,
  #' get candidate_id, election_year from variable
  #' get candidate from the candidate_id link
  #' create candidate_year given that candidates take part more than once
  #' replace - to allow join w/o issues
  xi_past <- fit$draws("xi_past") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "variable",
                 values_to = "draws") %>%
    filter(draws > -300) %>%
    mutate(
      candidate_id = as.integer(str_match(variable, "([\\d]+),")[,2]),
      election_year = 2002 + 5 * (as.integer(str_match(variable, ",([\\d]+)")[,2]) - 1),
      candidate =
        ifelse(election_year == 2002,
               candidate_id_df$long_name[candidate_id_df$year == 2002][candidate_id],
               ifelse(election_year == 2007,
                      candidate_id_df$long_name[candidate_id_df$year == 2007][candidate_id],
                      ifelse(election_year == 2012,
                             candidate_id_df$long_name[candidate_id_df$year == 2012][candidate_id],
                             ifelse(election_year == 2017,
                                    candidate_id_df$long_name[candidate_id_df$year == 2017][candidate_id],
                                    NA)))),
      candidate_year = paste0(candidate, election_year),
      candidate = str_replace_all(candidate, "\\-", " ")
    ) %>%
    left_join(election_results,
              by = c("election_year" = "year",
                     "candidate" = "candidate"))

  ## combine election results and error and get error on
  ## percentage point scale
  ## summarize
  # create levels
  xi_past <- xi_past %>%
    filter(!is.na(candidate)) %>%
    group_by(iter, election_year) %>%
    mutate(divide_by = ifelse(candidate_id == max(candidate_id),
                              percentage, 0),
           divide_by = max(divide_by),
           logodds = log(percentage/divide_by),
           polling_error_percentage = 100 * (percentage -
                                               exp(logodds + draws)/sum(exp(logodds + draws)))) %>%
    group_by(candidate_year) %>%
    summarize(
      q10 = quantile(polling_error_percentage, 0.1),
      q25 = quantile(polling_error_percentage, 0.25),
      q50 = quantile(polling_error_percentage, 0.5),
      q75 = quantile(polling_error_percentage, 0.75),
      q90 = quantile(polling_error_percentage, 0.9)
    ) %>%
    arrange(q50) %>%
    mutate(candidate_year = factor(candidate_year, levels = candidate_year))

  ## Plot
  plt <- ggplot(xi_past, aes(x = candidate_year, y = q50)) +
    geom_point() +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.5, width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
    coord_flip() +
    theme_light() +
    theme(axis.title.y = element_blank()) +
    labs(y = "Polling error (percentage points)")

  ## return
  return(plt)
}
