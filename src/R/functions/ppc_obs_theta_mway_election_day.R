#' ppc_obs_mway_election_day
#' @param fit cmdstanr fit
#' @param candidate_subset The candidates to be compared
#' @param NIter Number of iterations


ppc_obs_theta_mway_election_day <- function(fit, candidate_subset, NIter = 500){
  candidate_list <- read.csv("dta/polls_dta/candidate_identifiers.csv") %>%
    pull(candidate)
  if (!all(candidate_subset %in% candidate_list)){
    stop("Please check the spelling of the candidates' names.")
  }
  candidate_subset_id <- match(candidate_subset, candidate_list)
  NCandidates <- length(candidate_list)

  #' Determine number of iterations
  iterations <- fit$metadata()$iter_sampling * length(fit$metadata()$id)

  #' Sample iterations
  iter_subset <- sort(sample(1:iterations, NIter))

  ## Transition matrix
  #' Subset transition matrix and turn into data frame
  trans_mat <- fit$draws("transition_matrix") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "cc",
                 values_to = "val") %>%
    filter(iter %in% iter_subset) %>%
    mutate(
      p1 = as.integer(str_match(cc, "(\\d+),")[,2]),
      p2 = as.integer(str_match(cc, ",(\\d+)")[,2])
    ) %>%
    dplyr::select(-cc) %>%
    filter(!is.na(p1))

  #' Shape into matrizes
  trans_mat_list <- lapply(unique(trans_mat$iter), function(ii){
    trans_mat %>%
      filter(iter == ii) %>%
      dplyr::select(-iter) %>%
      pivot_wider(id_cols = p1,
                  names_from = p2,
                  values_from = val) %>%
      dplyr::select(-p1) %>%
      as.matrix() %>%
      return()
  })

  #' Save into array
  trans_mat_array <- array(NA, dim = c(nrow(trans_mat_list[[1]]),
                                       ncol(trans_mat_list[[1]]),
                                       length(trans_mat_list)))
  for (jj in 1:length(trans_mat_list)){
    trans_mat_array[,,jj] <- trans_mat_list[[jj]]
  }

  ## Extract theta
  theta <- fit$draws("pi_theta_1r") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "ct",
                 values_to = "val") %>%
    filter(iter %in% iter_subset) %>%
    mutate(
      candidate_id = as.integer(str_match(ct, "(\\d+),")[,2]),
      time_id = as.integer(str_match(ct, ",(\\d+)")[,2])
    ) %>%
    filter(!is.na(time_id)) %>%
    filter(time_id == max(time_id)) %>%
    dplyr::select(-time_id, -ct) %>%
    pivot_wider(id_cols = candidate_id,
                names_from = iter,
                values_from = val) %>%
    dplyr::select(-candidate_id) %>%
    as.matrix()

  #' subset vector
  included <- candidate_subset_id
  excluded <- seq(1, NCandidates)[!seq(1, NCandidates) %in% included]

  #' Conditioning
  df_out <- lapply(1:NIter, function(j){
    theta_cond <- theta[included, j] + trans_mat_array[included, excluded, j] %*% solve(trans_mat_array[excluded,  excluded, j]) %*% (0 - theta[excluded,j])
    out <- data.frame(theta_cond = theta_cond,
                      candidate_id = candidate_list[included],
                      j = iter_subset[j]) %>%
      return(out)
  }) %>%
    do.call("bind_rows", .)
  return(df_out)
}


