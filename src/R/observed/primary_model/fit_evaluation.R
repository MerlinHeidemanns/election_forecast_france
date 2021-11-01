################################################################################
## Title: Fit evaluation
################################################################################
## Libraries
library(tidyverse)
################################################################################
## Load data
data_file <- read_rds(file = "dta/fits/2021_10_29_primary.Rds")
candidate_vector <- data_file$df_cleaned$candidate_vector
################################################################################
## Assess y_matrix
y <- data_file$data_list$y
combination <- data_file$data_list$id_P_combinations
included <- data_file$data_list$candidates_included[,1,]
candidates <- data_file$data_list$NCandidates_Poll
df <- lapply(1:ncol(y), function(ii){
  data.frame(
    y = y[1:candidates[ii], ii],
    candidate = candidate_vector[included[combination[ii],1:candidates[ii]]],
    question_id = ii
  ) %>%
    return()
}) %>%
  do.call("bind_rows", .)
df <- df %>%
  group_by(question_id) %>%
  mutate(share = y/sum(y)) %>%
  filter(candidate == "Francois Asselineau")
################################################################################
## theta estimate
ppc_obs_theta_latent <- function(fit, candidate_vector, df_t_unit){
  theta <- fit$draws("theta_candidates") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "variable",
                 values_to = "draws") %>%
    mutate(
      candidate = candidate_vector[
        as.integer(
          str_match(variable, ",([\\d]+),")[,2]
        )
      ],
      time_unit_id = as.integer(
        str_match(variable, ",([\\d]+)\\]")[,2]
      )
    ) %>%
    filter(!is.na(candidate)) %>%
    group_by(time_unit_id, iter) %>%
    mutate(draws = exp(draws)/sum(exp(draws))) %>%
    group_by(time_unit_id, candidate) %>%
    summarize(
      q50 = quantile(draws, 0.5),
      q25 = quantile(draws, 0.25),
      q75 = quantile(draws, 0.75),
      q10 = quantile(draws, 0.10),
      q90 = quantile(draws, 0.90)
    ) %>%
    left_join(df_t_unit)
  ggplot(theta, aes(x = time_id, y = q50)) +
    geom_line(color = "blue") +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.3, fill = "blue") +
    geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.1, fill = "blue") +
    theme_light() +
    facet_wrap(candidate ~ .) +
    labs(y = "Latent support",
         x = "Time",
         caption = "Median, 50, 80")
}
ppc_obs_theta_latent(data_file$fit, candidate_vector, data_file$df_cleaned$df_t_unit)
################################################################################
## Transition matrix
ppc_obs_transition_matrix <- function(fit, candidate_vector = candidate_vector){
  ## Take median
  transition_matrix_hat <- fit$summary("transition_matrix", ~ quantile(., 0.5))
  colnames(transition_matrix_hat) <- c("variable", "q50")
  transition_matrix_hat <- transition_matrix_hat %>%
    mutate(
      from_candidate = supvec_names[as.integer(str_match(variable, ",([\\d+]+),")[,2])],
      to_candidate = supvec_names[as.integer(str_match(variable, ",([\\d+]+)\\]")[,2])],
      from_candidate = factor(from_candidate, levels = candidate_vector),
      to_candidate = factor(to_candidate, levels = candidate_vector)
    ) %>%
    dplyr::select(-variable) %>%
    mutate(q50 = abs(q50)) %>%
    arrange(from_candidate, to_candidate)

  ## Categorize
  transition_matrix_hat <- transition_matrix_hat %>%
    mutate(q50_cat = round(q50, 1))

  ## Plot
  plt <- ggplot(data = transition_matrix_hat,
                aes(x = to_candidate,
                    y = from_candidate,
                    fill = as.factor(q50_cat))) +
    geom_tile(alpha = 0.5) +
    labs(fill = "Transition probability")+
    theme_light() +
    ggplot2::theme(axis.text.x =
                     ggplot2::element_text(angle = 45,
                                           vjust = 1,
                                           size = 12,
                                           hjust = 1),
                   axis.text.y = ggplot2::element_text(size = 12)) +
    scale_fill_brewer(palette = "Greens") +
    ggplot2::coord_fixed() +
    labs(y = "From", x = "To")
  return(plt)
}
ppc_obs_transition_matrix(data_file$fit, candidate_vector = candidate_vector)
################################################################################
source("src/R/functions/ppc_obs_theta_twoway_plt.R")
ppc_obs_theta_twoway_plt <- function(fit, NIter = 500, candidate_vector){

  NCandidates <- length(candidate_vector)

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
      p1 = as.integer(str_match(cc, ",(\\d+),")[,2]),
      p2 = as.integer(str_match(cc, ",(\\d+)\\]")[,2])
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
  theta <- fit$draws("theta_candidates") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "ct",
                 values_to = "val") %>%
    filter(iter %in% iter_subset) %>%
    mutate(
      candidate_id = as.integer(str_match(ct, ",(\\d+),")[,2]),
      time_id = as.integer(str_match(ct, ",(\\d+)\\]")[,2])
    ) %>%
    filter(!is.na(time_id)) %>%
    filter(time_id == max(time_id)) %>%
    dplyr::select(-time_id, -ct) %>%
    group_by(iter) %>%
    mutate(val = exp(val)/sum(exp(val))) %>%
    ungroup() %>%
    pivot_wider(id_cols = candidate_id,
                names_from = iter,
                values_from = val) %>%
    dplyr::select(-candidate_id) %>%
    as.matrix()


  count <- 0
  matchup_probabilities <- data.frame()
  for (aa in 2:(length(candidate_vector) - 1)){
    for (bb in (aa + 1):length(candidate_vector)){
      print(count)
      count = count + 1
      included <- c(1, aa, bb)
      excluded <- seq(2, NCandidates)[!seq(2, NCandidates) %in% included]
      #' Conditioning
      df_out <- lapply(1:NIter, function(j){
        theta_cond <- theta[included, j] + trans_mat_array[included, excluded, j] %*% solve(trans_mat_array[excluded,  excluded, j]) %*% (0 - theta[excluded,j])
        out <- data.frame(theta_cond = theta_cond,
                          candidate = candidate_vector[included],
                          j = iter_subset[j])
      }) %>%
        do.call("bind_rows", .)
      df_out <- df_out %>%
        filter(candidate != "_Abstention") %>%
        group_by(j) %>%
        mutate(theta_cond = theta_cond / sum(theta_cond))
      out <- df_out %>%
        group_by(candidate) %>%
        summarize(win_prob = mean(theta_cond > 0.5)) %>%
        mutate(n = count)
      matchup_probabilities <- bind_rows(matchup_probabilities,
                                         out)
    }
  }

  m1 <- matchup_probabilities %>%
    group_by(n) %>%
    mutate(id = 1:n()) %>%
    ungroup() %>%
    pivot_wider(id_cols = c(n),
                names_from = id,
                values_from = c(candidate, win_prob)) %>%
    dplyr::select(-win_prob_1, -n)
  m1 <- m1 %>%
    bind_rows(m1 %>%
                rename(candidate_1 = candidate_2,
                       candidate_2 = candidate_1) %>%
                mutate(win_prob_2 = 1 - win_prob_2)) %>%
    mutate(win_prob_2 = as.numeric(win_prob_2)) %>%
    pivot_wider(id_cols = candidate_1,
                names_from = candidate_2,
                values_from = win_prob_2) %>%
    as.data.frame()

  rownames(m1) <- m1[,1]
  m1 <- m1[,2:ncol(m1)]
  m1 <- m1[candidate_vector[2:NCandidates],candidate_vector[2:NCandidates]]
  #m1 <- as.numeric(m1)


  m2 <- m1 %>%
    rownames_to_column(var = "candidate_1") %>%
    pivot_longer(c(-candidate_1),
                 names_to = "candidate_2",
                 values_to = "prob")

  m2 <- m2 %>%
    mutate(prob_cat = round(prob, 1)) %>%
    mutate(candidate_1 = factor(candidate_1, levels = candidate_vector),
           candidate_2 = factor(candidate_2, levels = candidate_vector)) %>%
    arrange(candidate_1, candidate_2)
  plt <- ggplot(data = m2,
                aes(x = candidate_1, y = candidate_2, fill = prob_cat)) +
    geom_tile(alpha = 0.5) +
    labs(fill = "Win probability (row v col)")+
    theme_light() +
    ggplot2::theme(axis.text.x =
                     ggplot2::element_text(angle = 45,
                                           vjust = 1,
                                           size = 12,
                                           hjust = 1),
                   axis.text.y = ggplot2::element_text(size = 12)) +
    #scale_fill_brewer(palette = "Blues") +
    ggplot2::scale_fill_gradient2(low = "blue", high = "red", midpoint = 0.5, limit = c(0, 1), space = "Lab")+
    ggplot2::coord_fixed() +
    theme(axis.title = element_blank())
  return(plt)
}
ppc_obs_theta_twoway_plt(fit, 500, candidate_vector)
################################################################################
candidate_subset <- c("_Abstention",
                      "Marine Le Pen",
                      "Emmanuel Macron")
ppc_obs_theta_mway_time_plt <- function(fit, candidate_vector, candidate_subset,
                                        unit_df,
                                        NIter = 500){

  if (!all(candidate_subset %in% candidate_vector)){
    stop("Please check the spelling of the candidates' names.")
  }
  candidate_subset_id <- match(candidate_subset, candidate_vector)
  NCandidates <- length(candidate_vector)

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
      p1 = as.integer(str_match(cc, ",(\\d+),")[,2]),
      p2 = as.integer(str_match(cc, ",(\\d+)\\]")[,2])
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
  theta <- fit$draws("theta_candidates") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "ct",
                 values_to = "val") %>%
    filter(iter %in% iter_subset) %>%
    mutate(
      candidate_id = as.integer(str_match(ct, ",(\\d+),")[,2]),
      time_id = as.integer(str_match(ct, ",(\\d+)\\]")[,2])
    ) %>%
    filter(!is.na(time_id)) %>%
    dplyr::select(-ct) %>%
    group_by(time_id, iter) %>%
    mutate(val = exp(val)/sum(exp(val))) %>%
    ungroup() %>%
    pivot_wider(id_cols = c(time_id, candidate_id),
                names_from = iter,
                values_from = val)

  #' subset vector
  included <- candidate_subset_id
  excluded <- seq(1, NCandidates)[!seq(1, NCandidates) %in% included]

  theta_time <- lapply(1:max(theta$time_id), function(ii){
    theta_subset <- theta %>%
      filter(time_id == ii) %>%
      dplyr::select(-time_id, -candidate_id) %>%
      as.matrix()
    #' Conditioning
    df_out <- lapply(1:NIter, function(j){
      theta_cond <- theta_subset[included, j] + trans_mat_array[included, excluded, j] %*% solve(trans_mat_array[excluded,  excluded, j]) %*% (0 - theta_subset[excluded,j])
      out <- data.frame(theta_cond = theta_cond,
                        candidate_id = candidate_vector[included],
                        j = iter_subset[j],
                        time_id = ii) %>%
        return(out)
    }) %>%
      do.call("bind_rows", .) %>%
      return(df_out)
  }) %>%
    do.call("bind_rows", .)

  ## Load time ids
  time <- unit_df %>%
    rename(time_id_integers = time_id)

  ## Summarize and merge time ids in
  theta_time <- theta_time %>%
    group_by(candidate_id, time_id) %>%
    summarize(
      q50 = quantile(theta_cond, 0.5),
      q25 = quantile(theta_cond, 0.25),
      q75 = quantile(theta_cond, 0.75),
      q10 = quantile(theta_cond, 0.1),
      q90 = quantile(theta_cond, 0.9),
    ) %>%
    left_join(time, by = c("time_id" = "time_unit_id"))

  ## Return plt
  plt <- ggplot(theta_time, aes(x = time_id_integers, y = q50)) +
    geom_line(aes(color = candidate_id)) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = candidate_id), alpha = 0.3) +
    geom_ribbon(aes(ymin = q10, ymax = q90, fill = candidate_id), alpha = 0.15) +
    theme_light() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.x = element_blank()) +
    labs(y = "Support",
         caption = "Median, 50%")

  return(plt)
}
plt <- ppc_obs_theta_mway_time_plt(data_file$fit, candidate_vector, candidate_subset,
                            data_file$df_cleaned$df_t_unit)
################################################################################
## Covariance terms
ppc_obs_sigma_cov <- function(fit, candidate_vector){
  sigma_cov <- fit$summary("sigma_cov",
                                          ~ quantile(., c(0.10, 0.25, 0.5, 0.75, 0.9)))
  colnames(sigma_cov) <- c("variable", "q10", "q25", "q50", "q75", "q90")
  sigma_cov <- sigma_cov %>%
    mutate(
      candidate = candidate_vector[
        as.integer(
          str_match(variable, "([\\d]+),")[,2]
        ) + 1
      ]
    ) %>%
    arrange(q50) %>%
    mutate(
      candidate = factor(candidate, levels = candidate)
    )
  ggplot(sigma_cov, aes(x = candidate, y = q50)) +
    geom_point() +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.5, width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
    theme_light() +
    coord_flip() +
    theme(axis.title.y = element_blank()) +
    labs(y = "Sigma random walk")
}
#' Candidate with smaller polling numbers have higher variance?
ppc_obs_sigma_cov(data_file$fit, candidate_vector)
################################################################################
##
tau <- data_file[["fit"]]$draws("tau") %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  filter(iter < 300) %>%
  select(-iter) %>%
  pivot_longer(everything(),
               names_to = "var",
               values_to = "draws") %>%
  mutate(
    candidate = candidate_vector[
      as.integer(
        str_match(var, "([\\d]+),")[,2]
      )
    ],
    survey_id = as.integer(
      str_match(var, ",([\\d]+)")[,2]
    )
  ) %>%
  filter(!is.na(candidate))
ggplot(tau, aes(x = draws)) +
  geom_histogram(bins = 200) +
  facet_wrap(candidate ~ ., scales = "free_x")


