
ppc_obs_theta_twoway_plt <- function(fit, NIter = 500){
  candidate_list <- read.csv("dta/polls_dta/candidate_identifiers.csv") %>%
    pull(candidate)
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
  theta <- fit$draws("prob_theta") %>%
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


  count <- 0
  matchup_probabilities <- data.frame()
  for (aa in 2:(length(candidate_list) - 1)){
    for (bb in (aa + 1):length(candidate_list)){
      print(count)
      count = count + 1
      included <- c(1, aa, bb)
      excluded <- seq(2, NCandidates)[!seq(2, NCandidates) %in% included]
      #' Conditioning
      df_out <- lapply(1:NIter, function(j){
        theta_cond <- theta[included, j] + trans_mat_array[included, excluded, j] %*% solve(trans_mat_array[excluded,  excluded, j]) %*% (0 - theta[excluded,j])
        out <- data.frame(theta_cond = theta_cond,
                          candidate = candidate_list[included],
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
  m1 <- m1[candidate_list,candidate_list]
  m1 <- as.numeric(m1)


  m2 <- m1 %>%
    rownames_to_column(var = "candidate_1") %>%
    pivot_longer(c(-candidate_1),
                 names_to = "candidate_2",
                 values_to = "prob")

  m2 <- m2 %>%
    mutate(prob_cat = round(prob, 1))
  plt <- ggplot(data = m2,
                aes(x = candidate_1, y = candidate_2, fill = prob_cat)) +
    geom_tile(alpha = 0.5) +
    labs(fill = "Win probability (row v col")+
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