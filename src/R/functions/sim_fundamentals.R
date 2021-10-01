#' sim_fundamentals
#' @param NElections How many past elections?
#' @param NBlocs How many political blocs?
#' @return list object


sim_fundamentals <- function(NElections, NBlocs){

  ## Load functions
  source("src/R/functions/exp_softmax.R")

  ## CovMat constructions
  corr_mat_alpha <- matrix(0.5, ncol = NBlocs - 1, nrow = NBlocs - 1)
  diag(corr_mat_alpha) <- 1
  sigma_alpha <- abs(rnorm(NBlocs - 1, 0, 0.1))
  diag_mat <- matrix(0, nrow = NBlocs - 1, ncol = NBlocs - 1)
  diag(diag_mat) <- 1
  cov_mat_alpha <- (diag_mat * sigma_alpha) %*% corr_mat_alpha %*% (diag_mat * sigma_alpha)

  ## Latent support
  prob_alpha <- matrix(NA, nrow = NElections, ncol = NBlocs)
  logodds_alpha <- matrix(NA, nrow = NElections, ncol = NBlocs)
  alpha_prior <- DirichletReg::rdirichlet(1, rep(5, NBlocs))

  logodds_alpha[1, ] <- log(alpha_prior/alpha_prior[1])
  logodds_alpha[, 1] <- 0

  for (tt in 2:NElections)
    logodds_alpha[tt, 2:NBlocs] = logodds_alpha[tt - 1, 2:NBlocs] + MASS::mvrnorm(1, rep(0, NBlocs - 1), cov_mat_alpha)

  for (tt in 1:NElections)
    prob_alpha[tt,] = exp_softmax(logodds_alpha[tt, ])

  ## Support in election
  logodds_mu <- matrix(NA, nrow = NElections, ncol = NBlocs)
  prob_mu <- matrix(NA, nrow = NElections, ncol = NBlocs)
  incumbency <- matrix(0, nrow = NElections, ncol = NBlocs)
  presidential_popularity <- rep(NA, NElections)


  ## Coefficients
  beta_incumbency <- -abs(rnorm(1, 0, 0.3))
  beta_popularity <- abs(rnorm(1, 0.1, 0.01))
  beta_unemployment <- -abs(rnorm(1, 0.1, 0.01))

  beta_fundamentals <- matrix(c(beta_popularity,
                                beta_unemployment), ncol = 1)

  ## Generate election outcomes
  #' population size
  populace <- runif(1, 1000000, 2000000)
  popularity <- runif(NElections, 0.3, 0.7)
  unemployment <- runif(NElections, 0.01, 0.1)
  x_fundamentals <- matrix(cbind(popularity, unemployment), nrow = NElections)


  y <- matrix(NA, nrow = NElections, ncol = NBlocs)

  incumbency_distance <- matrix(rep(seq(1, NBlocs), NBlocs - 1), ncol = NBlocs, byrow = TRUE)
  incumbency_distance[,2:NBlocs] <- abs(incumbency_distance[,2:NBlocs] - seq(2, 6))
  incumbency_distance[,1] <- 5
  incumbency_distance = incumbency_distance + 1

  beta_popularity <- c(abs(rnorm(1, 0.1, 0.01)),
                       sort(-abs(rnorm(4, 0.05, 0.02))),
                       -abs(rnorm(1, 0.05, 0.1)))
  beta_unemployment <- c(-abs(rnorm(1, 0.1, 0.01)),
                         sort(abs(rnorm(4, 0.05, 0.02)), decreasing = TRUE),
                         abs(rnorm(1, 0.05, 0.1)))

  beta_fundamentals <- rbind(beta_popularity,
                             beta_unemployment)

  incumbency[1, ] <- c(0, rmultinom(1, 1, rep(1, NBlocs - 1)/(NBlocs - 1)))
  logodds_mu[1, ] <- logodds_alpha[1, ] + beta_incumbency * incumbency[1, ]
  prob_mu[1, ] <- exp_softmax(
    logodds_mu[1, ] +
      x_fundamentals[1, ] %*% beta_fundamentals * incumbency[1, ])
  y[1, ] <- rmultinom(1, populace, prob_mu[1, ])
  for (tt in 2:NElections){
    incumbency[tt, ] <- c(0, as.integer(y[tt - 1, 2:NBlocs] == max(y[tt - 1, 2:NBlocs])))
    logodds_mu[tt, ] <- logodds_alpha[tt, ] +
      beta_incumbency * incumbency[tt, ] +
      x_fundamentals[tt, ] %*% beta_fundamentals * incumbency[tt, ]
    prob_mu[tt, ] <- exp_softmax(logodds_mu[tt, ])
    y[tt, ] <- rmultinom(1, populace, prob_mu[tt, ])
  }

  y_df <- y %>%
    as.data.frame() %>%
    mutate(election_id = 1:n()) %>%
    pivot_longer(c(-election_id),
                 names_to = "bloc_id",
                 values_to = "y",
                 names_prefix = "V") %>%
    group_by(election_id) %>%
    mutate(percentage = y/sum(y))


  ## Plot y_df
  y_plt <- ggplot(y_df, aes(x = election_id, y = percentage, color = bloc_id)) +
    geom_line() +
    theme_light() +
    labs(x = "Election", y = "Share", color = "Bloc")

  ## Return
  out_list <- list(
    NElections = NElections,
    NBlocs = NBlocs,
    y = y,
    y_df = y_df,
    y_plt = y_plt,
    incumbency = incumbency,
    x_fundamentals = x_fundamentals,
    sigma_alpha = data.frame(sigma_alpha = sigma_alpha,
                             bloc_id = 2:NBlocs),
    beta_incumbency = beta_incumbency,
    beta_fundamentals = data.frame(name = c("popularity", "unemployment"),
                                  value = c(beta_fundamentals))
  )
}
