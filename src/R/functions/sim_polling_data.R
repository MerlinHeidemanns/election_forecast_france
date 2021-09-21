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
sim_polling_data <- function(NPolls,
                             NPolls_past,
                             NPollsters,
                             NCombinations,
                             sigma_alpha,
                             sigma_tau,
                             sigma_xi,
                             theta_matrix,
                             transition_matrix,
                             prob_theta_past){

  #' Determine number of parties and time points from input
  NCandidates <- dim(theta_matrix)[2]
  NTime <- dim(theta_matrix)[1]

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

  for (ii in 1:NCombinations){
    candidate_combinations[[ii]] <- c(1, sort(sample(2:NCandidates, sample((NCandidates - 4):(NCandidates - 2), 1))))
  }
  candidate_combinations[[NCombinations + 1]] <- c(1, 2, 3)

  #' Determine which pollster omits abstention
  abstention_omitted <- c(rep(0, ceiling(0.5 * NPollsters)),
                            rep(1, floor(0.5 * NPollsters)))


  #' Simulate polls
  polls <- lapply(1:NPolls, function(x){
    #x = sample(1:N_first_round, 1)
    pollster_id <- sample(1:NPollsters, 1)
    time_id <- sample(1:NTime, 1)
    eta <- theta_matrix[time_id, ] +
      alpha[pollster_id, ] +
      tau[x, ] +
      xi
    NQuestions <- sample(2:4, 1)
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
        theta_ss = theta
      }

      prob_theta <- exp_softmax(theta_ss)
      if (abstention_omitted[pollster_id]){
        prob_theta <- prob_theta[2:NCandidates_included]/sum(prob_theta[2:NCandidates_included])
      }
      y <- rmultinom(1, 1000, prob_theta)
      length_omitted <- NCandidates_included - (abstention_omitted[pollster_id])

      out_question <- data.frame(
        question_id = rep(jj, length_omitted),
        y = y,
        candidate_id = candidates_included[(1 + abstention_omitted[pollster_id]):NCandidates_included],
        time_id = rep(time_id, length_omitted),
        pollster_id = rep(pollster_id, length_omitted),
        n = rep(1000, length_omitted),
        survey_id = rep(x, length_omitted),
        abstention_omitted = abstention_omitted[pollster_id]
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
  #' Draw an election
  #' Sample poll result
  #' Same format as first round polls
  NElections_past <- dim(prob_theta_past)[1]
  NCandidates_past <- rep(NA, NElections_past)
  NPollsters_past <- NElections_past + rpois(1, 5)
  id_rt_past <- c(sample(c(1:NElections_past), NElections_past),
               sample(1:NElections_past, NPollsters_past - NElections_past, replace = TRUE))
  for (ii in 1:NElections_past) NCandidates_past[ii] <- sum(prob_theta_past[ii,] > 0)
  #' parameters
  #' Create matrix for alpha_past
  alpha_past <- matrix(-10,
                       ncol = max(NCandidates_past),
                       nrow = NPollsters_past)
  #' Demean each row, rows = polling houses, columns = parties
  for (ii in 1:NPollsters_past){
    tmp <- rnorm(NCandidates_past[id_rt_past[ii]], 0, sigma_alpha)
    tmp <- tmp - mean(tmp)
    alpha_past[ii, 1:NCandidates_past[id_rt_past[ii]]] <- tmp
  }
  #' Create matrix for xi_past
  xi_past <- matrix(-10,
                    ncol = max(NCandidates_past),
                    nrow = NElections_past)
  print(dim(xi_past))
  #' Demean each row, rows = polling houses, columns = parties
  for (ii in 1:NElections_past){
    tmp <- rnorm(NCandidates_past[ii], 0, sigma_xi)
    tmp <- tmp - mean(tmp)
    xi_past[ii, 1:NCandidates_past[ii]] <- tmp
  }

  abstention_omitted_past <- c(rep(0, ceiling(0.5 * NPollsters_past)),
                               rep(1,   floor(0.5 * NPollsters_past)))

  #' Turn past results into log-odds ratios
  theta_past <- matrix(-10, nrow = dim(prob_theta_past)[1],
                     ncol = dim(prob_theta_past)[2])
  for (ii in 1:dim(prob_theta_past)[1]){
    tmp <- prob_theta_past[ii,1:NCandidates_past[ii]]
    tmp <- log(tmp/tmp[length(tmp)])
    theta_past[ii,1:NCandidates_past[ii]] <- tmp
  }
  #' Simulate polls
  polls_past <- lapply(1:NPolls_past, function(x){
    time_id <- sample(1:NElections_past, 1)
    pollster_id <- sample(seq(1:NPollsters_past)[id_rt_past == time_id], 1)
    NCandidates_past_t <- NCandidates_past[time_id]
    tau <- rnorm(NCandidates_past_t, 0, sigma_tau)
    tau <- tau - mean(tau)
    tmp <- exp_softmax(theta_past[time_id, 1:NCandidates_past_t] +
      alpha_past[pollster_id, 1:NCandidates_past_t] +
      tau +
      xi_past[time_id, 1:NCandidates_past_t])
    if (abstention_omitted_past[pollster_id]){
      tmp <- tmp[2:NCandidates_past_t]/sum(tmp[2:NCandidates_past_t])
    }
    y <- rmultinom(1, 1000, tmp)
    length_omitted <- NCandidates_past_t - abstention_omitted_past[pollster_id]
    out <- data.frame(
      y = y,
      candidate_id = seq(1 + abstention_omitted_past[pollster_id], NCandidates_past_t),
      t = rep(time_id, length_omitted),
      pollster_id = rep(pollster_id, length_omitted),
      n = rep(1000, length_omitted),
      survey_id = rep(x, length_omitted),
      tau = tau[(1 + abstention_omitted_past[pollster_id]):NCandidates_past_t],
      xi = xi_past[time_id, (1 + abstention_omitted_past[pollster_id]):NCandidates_past_t],
      abstention_omitted = rep(abstention_omitted_past[pollster_id], length_omitted)
    )
    return(out)
  }) %>%
    do.call("bind_rows", .)

  polls_past <- polls_past %>%
    group_by(pollster_id, t) %>%
    mutate(pollster_id = cur_group_id()) %>%
    ungroup()

  #' Output
  sigma_parameters =
    data.frame(sigma_xi = sd(xi),
               sigma_tau = sd(tau),
               sigma_alpha = sd(c(alpha, alpha_past[alpha_past != -10])))
  alpha <- data.frame(alpha) %>%
    mutate(pollster_id = 1:NPollsters) %>%
    pivot_longer(
      c(-pollster_id),
      names_to = "candidate_id",
      values_to = "alpha",
      names_prefix = "X"
    )
  xi <- data.frame(xi = t(xi)) %>%
    mutate(candidate_id = 1:NCandidates)

  print(xi_past)
  xi_past <- as.data.frame(xi_past) %>%
    mutate(election_id = 1:n()) %>%
    pivot_longer(-c(election_id),
                 names_to = "candidate_id",
                 values_to = "xi_logodds",
                 names_prefix = "V") %>%
    mutate(candidate_id = as.integer(candidate_id))
  return(list(polls = polls,
              polls_past = polls_past,
              NCandidates_past = NCandidates_past,
              NPollsters_past = NPollsters_past,
              NElections_past = NElections_past,
              sigma_parameters = sigma_parameters,
              alpha = alpha,
              tau = tau,
              xi = xi,
              xi_past = xi_past,
              abstention_omitted = abstention_omitted,
              abstention_omitted_past = abstention_omitted_past))
}
#' Example
# source("src/R/functions/sim_random_walk.R")
# data <- sim_random_walk(6, 4, 2, 2, 20, 20, 0, 0.1)
# df <- sim_polling_data(40, 40, 40, 3, 0, 0, 0, data$eta_matrix, data$transition_matrix, pi_past = data$pi_past)
# ggplot(df$polls_first_round, aes(x = t, y = y/n, color = as.factor(p))) +
#   geom_point() +
#   geom_smooth()
