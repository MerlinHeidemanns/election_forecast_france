ppc_party_positions_density <- function(fit, bloc_vector, data_list){
  ## Parameters
  mu <- fit$draws("mu_parties") %>%
    posterior::as_draws_df() %>%
    select(contains("mu")) %>%
    as.matrix()

  sigma <- fit$draws("sigma_parties") %>%
    posterior::as_draws_df() %>%
    select(contains("sigma")) %>%
    as.matrix()

  ## Densities and vote share
  out <- lapply(sample(1:1800, 250), function(jj){
    out0 <- lapply(seq(-3, 3, 0.2), function(qq){
      out1 <- lapply(1:5, function(ii){
        out2 <- data.frame(p = bloc_vector[ii],
                           prob = dnorm(qq, mu[jj,ii], sigma[jj, ii]),
                           mu = qq)
        return(out2)
      }) %>%
        do.call("bind_rows", .) %>%
        mutate(prob = prob/sum(prob))
      return(out1)
    }) %>%
      do.call("bind_rows", .)
    return(out0)
  }) %>%
    do.call("bind_rows", .)
  out <- out %>%
    group_by(mu, p) %>%
    summarize(
      q50 = quantile(prob, 0.5),
      q25 = quantile(prob, 0.25),
      q75 = quantile(prob, 0.75),
      q10 = quantile(prob, 0.1),
      q90 = quantile(prob, 0.9)
    )

  ## Density for latent outcome
  mu_pos <- fit$summary("mu", ~quantile(., 0.5))
  data_frame_outcome <- as.data.frame(data_list$YVoteshare)
  colnames(data_frame_outcome) <- bloc_vector
  mu_pos <- cbind(mu_pos, data_frame_outcome)
  mu_pos <- mu_pos %>%
    select(-variable) %>%
    pivot_longer(c(-`50%`),
                 names_to = "p",
                 values_to = "percentage") %>%
    rename(q50 = `50%`)

  ## Set colors
  cols <- c("Gauche radicale et extreme gauche" = "brown4",
            "Gauche" ="brown2",
            "Ecologisme" = "limegreen",
            "Centre" ="gold",
            "Droite" ="blue",
            "Droite radicale et extreme droite" ="navyblue")
  ## Plot
  plt <- ggplot(out, aes(x = mu, y = q50)) +
    geom_line(aes(color = p)) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = p), alpha = 0.3) +
    geom_ribbon(aes(ymin = q10, ymax = q90, fill = p), alpha = 0.15) +
    geom_histogram(data = mu_pos, aes(x = q50, after_stat(ndensity)/2), alpha = 0.3) +
    theme_light() +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    theme(legend.title = element_blank()) +
    labs(x = "Left-Right position of departement",
         y = "Expected voteshare")

  return(plt)
}
ppc_party_positions_density(fit, bloc_vector, data_list)
ppc_party_positions_density_multi <- function(fit, bloc_vector, data_list){
  ## Parameters
  mu <- fit$draws("mu_parties") %>%
    posterior::as_draws_df() %>%
    select(contains("mu")) %>%
    as.matrix()

  sigma <- fit$draws("sigma_parties") %>%
    posterior::as_draws_df() %>%
    select(contains("sigma")) %>%
    as.matrix()

  ## Densities and vote share
  out <- lapply(sample(1:1800, 250), function(jj){
    out0 <- lapply(seq(-3, 3, 0.2), function(qq){
      out1 <- lapply(1:6, function(kk){
        out2 <- lapply(1:5, function(ii){
          index_mu <- (ii - 1) * 6 + kk
          out3 <- data.frame(p = bloc_vector[ii],
                             prob = dnorm(qq, mu[jj,ii], sigma[jj, index_mu]),
                             mu = qq,
                             election = election_years[kk])
        }) %>%
          do.call("bind_rows", .) %>%
          mutate(prob = prob/sum(prob))
        return(out2)
      }) %>%
        do.call("bind_rows", .)
      return(out1)
    }) %>%
      do.call("bind_rows", .)
    return(out0)
  }) %>%
    do.call("bind_rows", .)
  out <- out %>%
    group_by(mu, p, election) %>%
    summarize(
      q50 = quantile(prob, 0.5),
      q25 = quantile(prob, 0.25),
      q75 = quantile(prob, 0.75),
      q10 = quantile(prob, 0.1),
      q90 = quantile(prob, 0.9)
    )

  ## Density for latent outcome
  mu_pos <- fit$summary("mu", ~quantile(., 0.5))
  data_frame_outcome <- as.data.frame(data_list$YVoteshare)
  colnames(data_frame_outcome) <- bloc_vector
  mu_pos <- cbind(mu_pos, data_frame_outcome) %>%
    add_column(election = election_years[data_list$id_Obs_elections])
  mu_pos <- mu_pos %>%
    select(-variable) %>%
    pivot_longer(c(-`50%`, -election),
                 names_to = "p",
                 values_to = "percentage") %>%
    rename(q50 = `50%`)

  ## Set colors
  cols <- c("Gauche radicale et extreme gauche" = "brown4",
            "Gauche" ="brown2",
            "Centre" ="gold",
            "Droite" ="blue",
            "Droite radicale et extreme droite" ="navyblue")
  ## Plot
  plt <- ggplot(out, aes(x = mu, y = q50)) +
    geom_line(aes(color = p)) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = p), alpha = 0.3) +
    geom_ribbon(aes(ymin = q10, ymax = q90, fill = p), alpha = 0.15) +
    geom_histogram(data = mu_pos, aes(x = q50, after_stat(ndensity)/2), alpha = 0.3) +
    theme_light() +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    theme(legend.title = element_blank()) +
    facet_wrap(election ~ .) +
    labs(x = "Left-Right position of departement",
         y = "Expected voteshare")

  return(plt)
}
ppc_party_positions_density_multi(fit, bloc_vector, data_list)
source("src/R/functions/ppc_fundamentals_observed_v_predicted.R")
ppc_fundamentals_observed_v_predicted(fit, df, bloc_vector, election_years)
