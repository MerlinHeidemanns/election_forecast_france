#' sim_polling_data
#' @param N Number of polls
#' @param N_R Number of polling houses
#' @param sigma_alpha Polling house deviations
#' @param sigma_tau Excess variance
#' @param sigma_xi Polling error
#' @param eta_matrix Matrix of log odds ratios
# N_first_round = 20
# N_second_round = 20
# N_first_round_past = 20
# N_R = 3
# sigma_alpha = 0.2
# sigma_tau = 0.2
# sigma_xi = 0.2
# eta_matrix = data$eta_matrix
# transition_matrix = data$transition_matrix
# pi_past = data$pi_past

source("src/R/functions/exp_softmax.R")
sim_polling_data_blocs <- function(NPolls,
                             NPolls_past,
                             NPollsters,
                             NCombinations,
                             NElections_past,
                             sigma_alpha,
                             sigma_tau,
                             sigma_xi,
                             theta_matrix_blocs,
                             prob_theta_matrix_blocs,
                             theta_matrix_candidates,
                             transition_matrix,
                             id_C_blocs,
                             combinations = TRUE){

  #' Determine number of parties and time points from input
  NCandidates <- dim(theta_matrix_candidates)[2]
  NTime <- dim(theta_matrix_candidates)[1]

  NBlocs <- dim(theta_matrix_blocs)[2]
  NTime_past <- dim(theta_matrix_blocs)[1]

  #' Draw polling house deviations, excess variance, and polling error
  alpha <- matrix(rnorm(NCandidates * NPollsters, 0, sigma_alpha), nrow = NPollsters, ncol = NCandidates)
  alpha <- demean_by_row(alpha)
  tau <- matrix(rnorm(NPolls * NCandidates, 0, sigma_tau),
                nrow = NPolls,
                ncol = NCandidates)
  tau <- demean_by_row(tau)
  xi <- matrix(rnorm(NCandidates, 0, sigma_xi), nrow = 1, ncol = NCandidates)
  xi <- demean_by_row(xi)

  candidate_combinations <- list()
  if (combinations){
    for (ii in 1:NCombinations){
      candidate_combinations[[ii]] <- c(1, sort(sample(2:NCandidates, sample((NCandidates - 4):(NCandidates - 2), 1))))
    }
    candidate_combinations[[NCombinations + 1]] <- c(1, 2, 3)
  } else {
    for (ii in 1:NCombinations){
      candidate_combinations[[ii]] <- seq(1, NCandidates)
    }
    candidate_combinations[[NCombinations + 1]] <- seq(1, NCandidates)
  }

  #' Determine which pollster omits abstention
  abstention_omitted <- c(rep(0, ceiling(0.5 * NPollsters)),
                            rep(1, floor(0.5 * NPollsters)))
  abstention_omitted <- rep(0, NPollsters)


  #' Simulate polls
  polls <- lapply(1:NPolls, function(x){
    #x = sample(1:N_first_round, 1)
    pollster_id <- sample(1:NPollsters, 1)
    time_id <- sample(1:NTime, 1)
    eta <- theta_matrix_candidates[time_id, ] +
      alpha[pollster_id, ] +
      tau[x, ] +
      xi
    NQuestions <- 1#sample(2:4, 1)
    out_survey <- lapply(1:NQuestions, function(jj){
      candidates_included <- candidate_combinations[[sample(1:NCombinations, 1)]]
      candidates_excluded <- (1:NCandidates)[!((1:NCandidates) %in% candidates_included)]
      NCandidates_included = length(candidates_included)
      NCandidates_excluded = NCandidates - NCandidates_included
      if (NCandidates_included < NCandidates){
        mat1 <- matrix(NA,
                       nrow = NCandidates_included,
                       ncol = NCandidates_excluded)
        mat2 <- matrix(NA, nrow = NCandidates_excluded,
                       ncol = NCandidates_excluded)
        for (j in 1:NCandidates_included){
          mat1[j,] = transition_matrix[candidates_included[j],][candidates_excluded]
        }
        for (j in 1:NCandidates_excluded){
          mat2[j,] = transition_matrix[candidates_excluded[j],][candidates_excluded]
        }
        theta_ss <- eta[candidates_included] +
          mat1 %*%
          solve(mat2) %*%
          (rep(0, NCandidates_excluded) - eta[NCandidates_excluded])
      } else if (NCandidates_included == NCandidates){
        theta_ss = eta
      }

      prob_theta <- exp_softmax(theta_ss)

      ## Create theta for those where abstention was omitted
      if (abstention_omitted[pollster_id]){
        prob_theta <- prob_theta[2:NCandidates_included]/sum(prob_theta[2:NCandidates_included])
      }

      ## Sample responses
      y <- rmultinom(1, 1000, prob_theta)

      ## How many candidates were excluded
      length_omitted <- NCandidates_included - (abstention_omitted[pollster_id])


      ## Create a dataframe
      out_question <- data.frame(
        question_id = rep(jj, length_omitted),
        y = y,
        candidate_id = candidates_included[(1 + abstention_omitted[pollster_id]):NCandidates_included],
        bloc_id = id_C_blocs[candidates_included[(1 + abstention_omitted[pollster_id]):NCandidates_included]],
        time_id = rep(time_id, length_omitted),
        pollster_id = rep(pollster_id, length_omitted),
        n = rep(1000, length_omitted),
        survey_id = rep(x, length_omitted),
        abstention_omitted = abstention_omitted[pollster_id],
        true_tau = tau[x, ]
      )
      return(out_question)
    })
    #out
    return(out_survey)
  }) %>%
    do.call("bind_rows", .)

  polls <- polls %>%
    group_by(survey_id, question_id) %>%
    mutate(question_id = cur_group_id()) %>%
    ungroup()


  #' -- Past polls --
  #' Draw election results for population
  populace <- as.integer(runif(NElections_past + 1, 5e+07, 6e+07))
  results_matrix <- matrix(NA, nrow = NElections_past + 1, ncol = NBlocs)
  election_time_points <- c(1, seq(NTime_past/NElections_past, NTime_past, length.out = NElections_past))
  for (jj in 1:(NElections_past + 1)){
    results_matrix[jj,] <- rmultinom(1, populace[jj], prob_theta_matrix_blocs[election_time_points[jj],])
  }


  #' Put elections into a nice dataframe
  results_df <- results_matrix %>%
    as.data.frame() %>%
    mutate(time_id = election_time_points) %>%
    pivot_longer(c(-time_id),
                 values_to = "y_result",
                 names_to = "bloc_id",
                 names_prefix = "V")

  #' Assign polls to time points
  #poll_time_points_potential <- seq(1,100 * NElections_past)[!seq(1, 100 * NElections_past) %in% election_time_points]
  poll_time_points_potential <- seq(1,100 * NElections_past)[(!seq(1, 100 * NElections_past) %in% election_time_points) &
                                                               (seq(1, 100 * NElections_past) %% 100 > 50)]
  poll_time_id <- sample(poll_time_points_potential, NPolls_past, replace = TRUE) %>%
                         sort()
  poll_election_id <- ceiling(poll_time_id/(NTime_past/NElections_past))
  poll_pollster_id <- sample(1:NPollsters, NPolls_past, replace = TRUE)
  NPolls_Elections <- table(poll_election_id)



  #' Draw parameters for the past
  xi_past <- matrix(rnorm(NElections_past * NBlocs, 0, sigma_xi),
                    nrow = NElections_past,
                    ncol = NBlocs)
  for (nn in 1:NElections_past){
    xi_past[nn, ] <- xi_past[nn, ] - mean(xi_past[nn, ])
  }
  alpha_past <- matrix(rnorm(NElections_past * NBlocs * NPollsters, 0, sigma_alpha),
                    nrow = NElections_past * NPollsters,
                    ncol = NBlocs)
  for (jj in 1:nrow(alpha_past)){
    alpha_past[jj, 1:NBlocs] <- alpha_past[jj, 1:NBlocs] - mean(alpha_past[jj, 1:NBlocs])
  }
  for (ii in 1:NElections_past){
    for (jj in 1:NPollsters){
      alpha_past[(1 + (ii - 1) * NPollsters):(ii * NPollsters), jj] <-
        alpha_past[(1 + (ii - 1) * NPollsters):(ii * NPollsters), jj] -
        mean(alpha_past[(1 + (ii - 1) * NPollsters):(ii * NPollsters), jj])
    }
  }

  tau_past <- matrix(rnorm(NBlocs * NPolls_past, 0, sigma_tau + 0.01),
                       nrow = NPolls_past,
                       ncol = NBlocs)
  sd(tau_past)
  for (jj in 1:nrow(tau_past)){
    tau_past[jj, ] <- tau_past[jj, ] - mean(tau_past[jj, ])
  }
  for (ii in 1:NElections_past){
    for (jj in 1:NBlocs){
      tau_past[sum(c(1, NPolls_Elections)[1:ii]):sum(NPolls_Elections[1:ii]), jj] <- tau_past[sum(c(1, NPolls_Elections)[1:ii]):sum(NPolls_Elections[1:ii]), jj] -
        mean(tau_past[sum(c(1, NPolls_Elections)[1:ii]):sum(NPolls_Elections[1:ii]), jj])
    }
  }
  sd(tau_past)


  abstention_omitter_pollster <- sample(c(rep(0,floor(NPollsters/2)), rep(1, ceiling(NPollsters/2))))
  abstention_omitter_pollster <- rep(0, NPollsters)
  #' Create polls
  polls_past <- lapply(1:NPolls_past, function(ii){
    pi <- theta_matrix_blocs[poll_time_id[ii],] +
      tau_past[ii, ] +
      alpha_past[(poll_election_id[ii] - 1) * (NPollsters) + poll_pollster_id[ii], ] +
      xi_past[poll_election_id[ii],]
    prob_pi <- exp_softmax(pi)
    if (abstention_omitter_pollster[poll_pollster_id[ii]] == 1){
      prob_pi <- prob_pi[2:6]/sum(prob_pi[2:6])
      y_past <- c(-99, rmultinom(1, 1000, prob_pi))
      prob_pi <- c(-99, prob_pi)
    } else {
      y_past <- rmultinom(1, 1000, prob_pi)
    }
    out <- data.frame(
      y = y_past,
      abstention_omitted = abstention_omitter_pollster[poll_pollster_id[ii]],
      poll_id = rep(ii, 6),
      bloc_id = 1:6,
      time_id = rep(poll_time_id[ii], 6),
      election_id = rep(poll_election_id[ii], 6),
      pollster_id = rep(poll_pollster_id[ii], 6),
      true_value = prob_pi,
      true_tau = tau_past[ii, ],
      true_xi = xi_past[poll_election_id[ii],],
      true_alpha = alpha_past[(poll_election_id[ii] - 1) * (NPollsters) + poll_pollster_id[ii], ]
    )
    return(out)
  }) %>%
    do.call("bind_rows", .)

  abstention_omitted_past <- abstention_omitter_pollster[poll_pollster_id]

  #' Output
  ## Sigma parameters
  sigma_parameters =
    data.frame(sigma_xi = sd(c(xi, xi_past)),
               sigma_tau = sd(c(tau, tau_past)),
               sigma_alpha = sd(c(alpha, alpha_past)))

  ## Polling house deviations true (current and past)
  true_alpha_current <- data.frame(alpha) %>%
    mutate(pollster_id = 1:NPollsters) %>%
    pivot_longer(
      c(-pollster_id),
      names_to = "candidate_id",
      values_to = "alpha",
      names_prefix = "X"
    )
  true_alpha_past <- data.frame(alpha_past) %>%
    mutate(pollster_id = rep(1:NPollsters, NElections_past),
           election_id = rep(1:NElections_past, NPollsters)) %>%
    pivot_longer(
      c(-pollster_id),
      names_to = "bloc_id",
      values_to = "alpha",
      names_prefix = "X"
    )

  ## Polling error true (current and past)
  true_xi_current <- data.frame(xi = t(xi)) %>%
    mutate(candidate_id = 1:NCandidates)

  true_xi_past <- xi_past %>%
    as.data.frame() %>%
    mutate(election_id = 1:n()) %>%
    pivot_longer(-c(election_id),
                 names_to = "bloc_id",
                 values_to = "xi_logodds",
                 names_prefix = "V") %>%
    mutate(bloc_id = as.integer(bloc_id))

  ## Make pollster indicator numeric
  polls <- polls %>%
    mutate(pollster_id = as.integer(pollster_id))

  ## Output
  return(list(NTime_past = NTime_past,
              NTime = NTime,
              polls = polls,
              polls_past = polls_past,
              NPollsters = NPollsters,
              NElections_past = NElections_past,
              sigma_parameters = sigma_parameters,
              true_alpha_current = true_alpha_current,
              true_alpha_past = true_alpha_past,
              true_xi_current = true_xi_current,
              true_xi_past = true_xi_past,
              abstention_omitted = abstention_omitted,
              abstention_omitted_past = abstention_omitted_past,
              results_matrix = results_matrix,
              election_time_points = election_time_points,
              election_df = results_df))
}
#' Example
# source("src/R/functions/sim_random_walk.R")
# data <- sim_random_walk(6, 4, 2, 2, 20, 20, 0, 0.1)
# df <- sim_polling_data(40, 40, 40, 3, 0, 0, 0, data$eta_matrix, data$transition_matrix, pi_past = data$pi_past)
# ggplot(df$polls_first_round, aes(x = t, y = y/n, color = as.factor(p))) +
#   geom_point() +
#   geom_smooth()
