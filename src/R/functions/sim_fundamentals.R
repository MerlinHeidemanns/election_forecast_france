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

  ## Generate election outcomes
  #' population size
  populace <- runif(1, 1000000, 2000000)

  y <- matrix(NA, nrow = NElections, ncol = NBlocs)
  for (tt in 1:NElections)
    y[tt, ] <- rmultinom(1, populace, prob_alpha[tt, ])

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
    y_plt = y_plt
  )
}
