#' sim_polling_data
#' @param N Number of polls
#' @param N_R Number of polling houses
#' @param sigma_alpha Polling house deviations
#' @param sigma_tau Excess variance
#' @param sigma_xi Polling error
#' @param eta_matrix Matrix of log odds ratios
sim_polling_data <- function(N,
                                  N_R,
                                  sigma_alpha,
                                  sigma_tau,
                                  sigma_xi,
                                  eta_matrix){
  #' Determine number of parties and time points from input
  P <- dim(eta_matrix)[2]
  T <- dim(eta_matrix)[1]

  #' Draw polling house deviations, excess variance, and polling error
  alpha <- matrix(rnorm(P * N_R, 0, sigma_alpha), nrow = N_R, ncol = P)
  alpha <- demean_by_row(alpha)
  tau <- matrix(rnorm(N * P, 0, sigma_tau), nrow = N, ncol = P)
  tau <- demean_by_row(tau)
  xi <- matrix(rnorm(P, 0, sigma_xi), nrow = 1, ncol = P)
  xi <- demean_by_row(xi)

  #' Simulate polls
  polls <- lapply(1:N, function(x){
    r <- sample(1:N_R, 1)
    t <- sample(1:T, 1)
    eta <- data$eta_matrix[t, ] +
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
  return(list(polls = polls,
              sigma_parameters = sigma_parameters,
              alpha = alpha,
              tau = tau,
              xi = xi))
}
#' Example
#' source("src/R/functions/sim_random_walk.R")
#' data <- sim_random_walk(4, 20, 1000, 0, 0.1)
#' df <- sim_polling_data(40, 3, 0.2, 0.2, 0.2, data$eta_matrix)
#' ggplot(df$polls, aes(x = t, y = y/n, color = as.factor(p))) +
#'   geom_point() +
#'   geom_smooth()
