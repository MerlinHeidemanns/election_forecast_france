## Functions
#' Simulate a multivariate random walk
#' @param P Number of parties
#' @param T Number of time points
#' @param rho correlation of transition matrix
#' @param sigma standard deviation of transition matrix
P_both = 4
P_past = 2
P_new = 3
T = 20
T_prior = 10
rho <- 0.5
sigma <- 0.1
sim_random_walk <- function(N_past_election,
                            P_both,
                            P_past,
                            P_new,
                            T,
                            T_prior = 0,
                            rho,
                            sigma){

  P_past_elections <- rpois(N_past_election, 4)
  while (min(P_past_elections) <= 2){
    P_past_elections[P_past_elections <= 2] <- rpois(sum(P_past_elections <= 2), 5)
  }
  max_P_past_elections <- max(P_past_elections)
  pi_past <- matrix(0, nrow = N_past_election, ncol = max_P_past_elections)
  for (ii in 1:N_past_election){
    tmp <- runif(P_past_elections[ii], 0.3, 1)
    tmp <- tmp/sum(tmp)
    pi_past[ii, 1:P_past_elections[ii]] <- tmp
  }


  P <- P_both + P_new + P_past
  ## Simulate a multinomial random walk
  #' Set N of parties and time points;
  #' Initial shares and log odds transformation
  #' Transition matrix for the process
  pi <- c(runif(P_both, 0.3, 1), rep(0, P_new), runif(P_past, 0.3, 1))
  pi <- pi/sum(pi)
  eta <- c(log(pi[1:length(pi) - 1]/pi[length(pi)]), 0)
  eta[eta == -Inf] = -2
  exp_softmax(eta)
  trans_matr_old <- matrix(sigma^2 * rho,
                                  nrow = P,
                                  ncol = P)
  diag(trans_matr_old) <- sigma^2

  ## Random walk
  #' Setup matrizes
  #' Transformation back into shares
  eta_matrix <- matrix(NA, nrow = T, ncol = P_both + P_new)
  pi_matrix <- matrix(NA, nrow = T, ncol = P_both + P_new)

  #' Determine start of election season
  #' Project forward
  #' Set disappearing parties to negative value
  eta_start <- eta + MASS::mvrnorm(1, rep(0, P), T_prior * trans_matr_old)
  eta_start_conditional <- eta_start[1:(P_both + P_new)] +
    trans_matr_old[1:(P_both + P_new), (P_both + P_new + 1):P] %*%
    solve(trans_matr_old[(P_both + P_new + 1):P, (P_both + P_new + 1):P]) %*%
    (rep(-10, P_past) - eta_start[(1 + P_both + P_new):P])

  trans_matr <- trans_matr_old[1:(P_both + P_new), 1:(P_both + P_new)] -
    trans_matr_old[1:(P_both + P_new), (1 + P_both + P_new):P] %*%
    solve(trans_matr_old[(1 + P_both + P_new):P, (1 + P_both + P_new):P]) %*%
    trans_matr_old[(1 + P_both + P_new):P, 1:(P_both + P_new)]

  eta_matrix[1, ] = eta_start_conditional
  #' Random walk
  for (t in 2:T){
    eta_matrix[t, ] <- eta_matrix[t - 1, ] + MASS::mvrnorm(1, rep(0, P_both + P_new), trans_matr)
  }
  for (t in 1:T){
    pi_matrix[t, ] <- exp(eta_matrix[t, ])/sum(exp(eta_matrix[t, ]))
  }

  #' Create collapsed data frame
  eta_matrix_coll <- matrix(NA, nrow = T, ncol = 2)
  pi_matrix_coll <- matrix(NA, nrow = T, ncol = 2)
  for (t in 1:T){
    eta_matrix_coll[t, ] <- eta_matrix[t, 1:2] +
      trans_matr[1:2, 3:(P_both + P_new)] %*%
      solve(trans_matr[3:(P_both + P_new), 3:(P_both + P_new)]) %*%
      (rep(-10, P_both + P_new - 2) - eta_matrix[t, 3:(P_both + P_new)])
  }
  for (t in 1:T){
    pi_matrix_coll[t, ] <- exp(eta_matrix_coll[t, ])/sum(exp(eta_matrix_coll[t, ]))
  }

  ## Create data frame
  data <- pi_matrix %>%
    as.data.frame() %>%
    mutate(t = 1:n()) %>%
    pivot_longer(
      c(-t),
      names_to = "p",
      values_to = "share",
      names_prefix = "V"
    )
  data_coll <- pi_matrix_coll %>%
    as.data.frame() %>%
    mutate(t = 1:n()) %>%
    pivot_longer(
      c(-t),
      names_to = "p",
      values_to = "share",
      names_prefix = "V"
    )
  return(list(
              P_both = P_both,
              P_new = P_new,
              P_past = P_past,
              df = data,
              pi_matrix = pi_matrix,
              eta_matrix = eta_matrix,
              transition_matrix_old = trans_matr_old,
              transition_matrix = trans_matr,
              eta_start = eta,
              pi_start = pi,
              df_coll = data_coll,
              pi_past = pi_past))
}



