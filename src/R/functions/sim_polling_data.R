#' sim_polling_data
#' @param N Number of polls
#' @param N_R Number of polling houses
#' @param sigma_alpha Polling house deviations
#' @param sigma_tau Excess variance
#' @param sigma_xi Polling error
#' @param eta_matrix Matrix of log odds ratios
sim_polling_data <- function(N_first_round,
                             N_second_round,
                             N_first_round_past,
                             N_R,
                             sigma_alpha,
                             sigma_tau,
                             sigma_xi,
                             eta_matrix,
                             transition_matrix,
                             pi_past){
  #' Determine number of parties and time points from input
  P <- dim(eta_matrix)[2]
  T <- dim(eta_matrix)[1]

  #' Draw polling house deviations, excess variance, and polling error
  alpha <- matrix(rnorm(P * N_R, 0, sigma_alpha), nrow = N_R, ncol = P)
  alpha <- demean_by_row(alpha)
  tau <- matrix(rnorm((N_first_round + N_second_round) * P, 0, sigma_tau),
                nrow = (N_first_round + N_second_round),
                ncol = P)
  tau <- demean_by_row(tau)
  xi <- matrix(rnorm(P, 0, sigma_xi), nrow = 1, ncol = P)
  xi <- demean_by_row(xi)

  #' Simulate polls
  polls_first_round <- lapply(1:N_first_round, function(x){
    r <- sample(1:N_R, 1)
    t <- sample(1:T, 1)
    eta <- eta_matrix[t, ] +
      alpha[r, ] +
      tau[x, ] +
      xi
    pi <- exp_softmax(eta)
    y <- rmultinom(1, 1000, pi)
    out <- data.frame(
      y = y,
      p = 1:P,
      t = t,
      r = rep(r, P),
      n = rep(1000, P),
      id = x
    )
    return(out)
  }) %>%
    do.call("bind_rows", .)

  polls_second_round <- lapply(1:N_second_round, function(x){
    r <- sample(1:N_R, 1)
    t = sample(1:T, 1)
    eta <- eta_matrix[t, ] +
      alpha[r, ] +
      tau[x + N_first_round, ] +
      xi
    eta_coll <- eta[1:2] +
      transition_matrix[1:2, 3:P] %*%
        solve(transition_matrix[3:P, 3:P]) %*%
        (rep(0, P - 2) - eta[3:P])
    pi <- exp_softmax(eta_coll)
    y <- rbinom(1, 1000, pi[1])
    out <- data.frame(
      y = y,
      t = t,
      r = r,
      n = 1000,
      id = x
    )
  }) %>%
    do.call("bind_rows", .)

  #' Past polls
  #' Draw an election
  #' Sample poll result
  #' Same format as first round polls
  N_past_elections <- dim(pi_past)[1]
  P_past_elections <- rep(NA, N_past_election)
  for (ii in 1:N_past_elections) P_past_elections[ii] <- sum(pi_past[ii,] > 0)
  polls_first_round_past <- lapply(1:N_first_round_past, function(x){
    t <- sample(1:N_past_elections, 1)
    N_p <- P_past_elections[t]
    y <- rmultinom(1, 1000, pi_past[t, 1:N_p])
    out <- data.frame(
      y = y,
      p = 1:N_p,
      t = t,
      n = rep(1000, N_p),
      id = x
    )
    return(out)
  }) %>%
  do.call("bind_rows", .)


  #' Output
  sigma_parameters =
    data.frame(sigma_xi = sd(xi),
               sigma_tau = sd(tau),
               sigma_alpha = sd(alpha))
  alpha <- data.frame(alpha) %>%
    mutate(r = 1:N_R) %>%
    pivot_longer(
      c(-r),
      names_to = "p",
      values_to = "alpha",
      names_prefix = "X"
    )
  xi <- data.frame(xi = t(xi)) %>%
    mutate(p = 1:P)

  return(list(polls_first_round = polls_first_round,
              polls_second_round = polls_second_round,
              polls_first_round_past = polls_first_round_past,
              P_past_elections = P_past_elections,
              N_elections_past = N_past_elections,
              sigma_parameters = sigma_parameters,
              alpha = alpha,
              tau = tau,
              xi = xi))
}
#' Example
# source("src/R/functions/sim_random_walk.R")
# data <- sim_random_walk(6, 4, 2, 2, 20, 20, 0, 0.1)
# df <- sim_polling_data(40, 40, 40, 3, 0, 0, 0, data$eta_matrix, data$transition_matrix, pi_past = data$pi_past)
# ggplot(df$polls_first_round, aes(x = t, y = y/n, color = as.factor(p))) +
#   geom_point() +
#   geom_smooth()
