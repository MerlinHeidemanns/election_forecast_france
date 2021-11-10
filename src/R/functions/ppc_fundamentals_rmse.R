output_error_department <- function(fit,
                                    bloc_vector,
                                    election_years,
                                    df,
                                    number,
                                    description){
  error <- fit$summary("error", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9)))
  colnames(error) <- c("variable", "q10", "q25", "q50", "q75", "q90")
  error <- error %>%
    mutate(
      bloc_id = as.integer(str_match(variable, "([\\d]+)\\]")[,2]),
      obs_id = as.integer(str_match(variable, "\\[([\\d]+)")[,2])
    ) %>%
    select(-variable) %>%
    left_join(
      df %>%
        arrange(bloc_id, election_id, department_id) %>%
        select(-bloc_id) %>%
        mutate(obs_id = 1:n())
    ) %>%
    mutate(bloc = bloc_vector[bloc_id],
           year = election_years[election_id]) %>%
    add_column(model_number = number,
               model_description = description)
  return(error)
}
output_rmse_bloc_election <- function(fit,
                                      bloc_vector,
                                      election_years,
                                      df,
                                      number,
                                      description){
  error <- fit$draws("error") %>%
    posterior::as_draws_df() %>%
    pivot_longer(everything(),
                 names_to = "variable",
                 values_to = "draws") %>%
    mutate(
      bloc_id = as.integer(str_match(variable, "([\\d]+)\\]")[,2]),
      obs_id = as.integer(str_match(variable, "\\[([\\d]+)")[,2])
    ) %>%
    filter(!is.na(bloc_id)) %>%
    select(-variable) %>%
    left_join(
      df %>%
        arrange(bloc_id, election_id, department_id) %>%
        select(-bloc_id) %>%
        mutate(obs_id = 1:n())
    ) %>%
    mutate(bloc = bloc_vector[bloc_id],
           year = election_years[election_id]) %>%
    group_by(bloc, year) %>%
    summarize(rmse = sqrt(mean(draws^2))) %>%
    add_column(model_number = number,
               model_description = description)
  return(error)
}

output_rmse_global <- function(fit,
                               number,
                               description){
  rmse <- fit$summary("rmse", ~quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9)))
  colnames(rmse) <- c("variable", "q10", "q25", "q50", "q75", "q90")
  rmse <- rmse %>%
    select(-variable) %>%
    add_column(model_number = number,
               model_description = description)
  return(rmse)
}


