## Functions
#' Simulate a multivariate random walk
#' @param P Number of parties
#' @param T Number of time points
#' @param rho correlation of transition matrix
#' @param sigma standard deviation of transition matrix
sim_random_walk_blocs <- function(NElections_past,
                            NCandidates,
                            NTime,
                            rho,
                            sigma){
  NBlocs <- 6
  NCandidates <- NCandidates + 1

  ## Assign blocs
  id_C_blocs <- c(1, sort(sample(2:6, NCandidates - 1, replace = TRUE)))
  while (length(unique(id_C_blocs)) != 6){
    id_C_blocs <- c(1, sort(sample(2:6, NCandidates - 1, replace = TRUE)))
  }

  ## Create random walk matrix
  # sigma_logodds <- abs(rnorm(NCandidates - 1, sigma, sigma))
  # sigma_odds <- exp(sigma_logodds) - 1
  # trans_matrix_rw <- matrix(rho,
  #                           nrow = NCandidates - 1,
  #                           ncol = NCandidates - 1)
  # diag(trans_matrix_rw) <- 1
  # identity_candidates <- matrix(0,
  #                               nrow = NCandidates - 1,
  #                               ncol = NCandidates - 1)
  # diag(identity_candidates) <- 1
  # trans_matrix_rw_odds <- (sigma_odds * identity_candidates) %*% trans_matrix_rw %*% (identity_candidates * sigma_odds)
  #
  # collapse_matrix <- matrix(0,
  #                           nrow = NBlocs - 1,
  #                           ncol = NCandidates - 1)
  # for (jj in 2:NCandidates){
  #   collapse_matrix[id_C_blocs[jj] - 1, jj - 1] <- 1
  # }
  # coll_trans_matrix_rw_odds <- collapse_matrix %*% trans_matrix_rw_odds %*% t(collapse_matrix)
  # coll_sigma <- sqrt(diag(coll_trans_matrix_rw_odds))
  # identity <- matrix(0, nrow = NBlocs - 1, ncol = NBlocs - 1)
  # diag(identity) <- 1
  # coll_cov_matrix_rw <- (log(coll_sigma + 1)/coll_sigma * identity) %*% coll_trans_matrix_rw_odds %*% t(log(coll_sigma + 1)/coll_sigma * identity)
  #
  # trans_matrix_rw_logodds <- (log(sigma_odds + 1) * identity_candidates) %*% trans_matrix_rw %*% (identity_candidates * log(sigma_odds + 1))

  ## only logodds
  sigma_logodds <- abs(rnorm(NCandidates - 1, 0, sigma))
  trans_matrix_rw <- matrix(rho,
                            nrow = NCandidates - 1,
                            ncol = NCandidates - 1)
  diag(trans_matrix_rw) <- 1
  identity_candidates <- matrix(0,
                                nrow = NCandidates - 1,
                                ncol = NCandidates - 1)
  diag(identity_candidates) <- 1
  trans_matrix_rw_logodds <- (sigma_logodds * identity_candidates) %*% trans_matrix_rw %*% (identity_candidates * sigma_logodds)

  collapse_matrix <- matrix(0,
                            nrow = NBlocs - 1,
                            ncol = NCandidates - 1)
  for (jj in 2:NCandidates){
    collapse_matrix[id_C_blocs[jj] - 1, jj - 1] <- 1
  }
  coll_trans_matrix_rw_logodds <- collapse_matrix %*% trans_matrix_rw_logodds %*% t(collapse_matrix)

  ## Simulate a multinomial random walk for the blocs
  ## Setup matrizes and start
  NTime_past <- NElections_past * 100
  theta_matrix_blocs <- matrix(NA, nrow = NTime_past, ncol = NBlocs)
  prob_theta_matrix_blocs <- matrix(NA, nrow = NTime_past, ncol = NBlocs)

  prob_theta_matrix_blocs[1, ] <- DirichletReg::rdirichlet(1, rep(10, NBlocs))
  theta_matrix_blocs[1, ] <- log(prob_theta_matrix_blocs[1, ]/prob_theta_matrix_blocs[1, 1])
  for (tt in 2:NTime_past){
    proposal_accepted <- FALSE
    while (proposal_accepted == FALSE){
      update <- MASS::mvrnorm(1, rep(0, NBlocs - 1), coll_trans_matrix_rw_logodds)
      previous <- theta_matrix_blocs[tt - 1, 2:NBlocs]
      if (all(exp_softmax(previous + update) > 0.005)){
        proposal_accepted <- TRUE
      }
    }
    theta_matrix_blocs[tt, 2:NBlocs] <- previous + update
  }
  theta_matrix_blocs[, 1] <- 0
  for (tt in 1:NTime_past){
    prob_theta_matrix_blocs[tt, ] <- exp_softmax(theta_matrix_blocs[tt, ])
    prob_theta_matrix_blocs[tt, ] <- (prob_theta_matrix_blocs[tt, ] + 0.1)/sum(prob_theta_matrix_blocs[tt, ] + 0.1)
    theta_matrix_blocs[tt, ] <- log(prob_theta_matrix_blocs[tt, ]/prob_theta_matrix_blocs[tt, 1])
  }


  #' Transform from blocs to candidates
  theta_matrix_candidates <- matrix(NA, nrow = NTime, ncol = NCandidates)
  prob_theta_matrix_candidates <- matrix(NA, nrow = NTime, ncol = NCandidates)

  for (jj in 1:NBlocs){
    prob_theta_matrix_candidates[1,id_C_blocs == jj] <- prob_theta_matrix_blocs[NTime_past, jj] * DirichletReg::rdirichlet(1, rep(4, sum(id_C_blocs == jj)))
  }


  theta_matrix_candidates[1,] <- log(prob_theta_matrix_candidates[1,]/prob_theta_matrix_candidates[1, 1])

  for (tt in 2:NTime){
    proposal_accepted <- FALSE
    while (proposal_accepted == FALSE){
      update <- MASS::mvrnorm(1, rep(0, NCandidates - 1), trans_matrix_rw_logodds)
      previous <- theta_matrix_candidates[tt - 1, 2:NCandidates]
      if (all(exp_softmax(previous + update) > 0.005)){
        proposal_accepted <- TRUE
      }
    }
    theta_matrix_candidates[tt, 2:NCandidates] <- previous + update
  }
  theta_matrix_candidates[, 1] <- 0
  for (tt in 1:NTime){
    prob_theta_matrix_candidates[tt, ] <- exp_softmax(theta_matrix_candidates[tt, ])
    prob_theta_matrix_candidates[tt, ] <- (prob_theta_matrix_candidates[tt, ])/sum(prob_theta_matrix_candidates[tt, ])
    theta_matrix_candidates[tt, ] <- log(prob_theta_matrix_candidates[tt, ]/prob_theta_matrix_candidates[tt, 1])
  }


  #' * Create transition matrix
  trans_matrix_pref <- matrix(0, nrow = NCandidates, ncol = NCandidates)
  for (jj in 1:NCandidates){
    included <- seq(1, NCandidates)
    included <- included[!included %in% jj]
    trans_matrix_pref[included, jj] <- - DirichletReg::rdirichlet(1, rep(5, NCandidates - 1))
  }
  diag(trans_matrix_pref) <- 1


  ## Create data frames
  #' true data current all parties
  data_candidates <- prob_theta_matrix_candidates %>%
    as.data.frame() %>%
    mutate(time_id = 1:n()) %>%
    pivot_longer(
      c(-time_id),
      names_to = "candidate_id",
      values_to = "share",
      names_prefix = "V"
    )
  #' true data blocs
  data_bloc <- prob_theta_matrix_blocs %>%
    as.data.frame() %>%
    mutate(time_id = 1:n()) %>%
    pivot_longer(
      c(-time_id),
      names_to = "bloc_id",
      values_to = "share",
      names_prefix = "V"
    ) %>%
    mutate(bloc_id = as.integer(bloc_id))

  return(list(
              NCandidates = NCandidates,
              NBlocs = NBlocs,
              df_candidates = data_candidates,
              df_blocs = data_bloc,
              prob_theta_matrix_candidates = prob_theta_matrix_candidates,
              theta_matrix_candidates = theta_matrix_candidates,
              prob_theta_matrix_blocs = prob_theta_matrix_blocs,
              theta_matrix_blocs = theta_matrix_blocs,
              transition_matrix_random_walk_candididates = trans_matrix_rw_logodds,
              transition_matrix_random_walk_blocs = coll_trans_matrix_rw_logodds,
              transition_matrix_preferences = trans_matrix_pref,
              id_C_blocs = id_C_blocs,
              sigma_logodds = sigma_logodds
              )
         )
}



