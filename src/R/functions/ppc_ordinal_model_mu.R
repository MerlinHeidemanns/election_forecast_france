


compute_theta <- function(mu, thres){
  len_theta <- length(thres)
  out_theta <- rep(NA, len_theta)
  out_theta[1] = (2 * atan(mu - thres[len_theta]) + pi)/(2 * pi) +
    (1 - (2 * atan(mu - thres[1]) + pi)/(2 * pi))
  for (k in 2:len_theta){
    out_theta[k] = ((2.0 * atan((mu - thres[k - 1])) -
                       2.0 * atan(mu - thres[k])))/(2.0 * pi);
  }
  return(out_theta)
}

thres <- fit$draws("thresholds") %>%
  posterior::as_draws_matrix()


mu <- fit$summary("mu", ~quantile(., 0.5)) %>%
  add_column(election = c(1988, 1995, 2002, 2007, 2012, 2017)[data_list$id_Obs_elections])

data_frame_outcome <- as.data.frame(data_list$YVoteshare)
colnames(data_frame_outcome) <- bloc_vector
mu <- cbind(mu, data_frame_outcome)
mu <- mu %>%
  select(-variable) %>%
  pivot_longer(c(-election, -`50%`),
               names_to = "theta",
               values_to = "percentage")

out <- lapply(sample(1:1800, 100, replace = FALSE), function(jj){
  out1 <- lapply(1:6, function(rr){
    upper <- mu %>%
      filter(election == c(1988, 1995, 2002, 2007, 2012, 2017)[rr]) %>%
      pull(`50%`) %>%
      max()
    lower <- mu %>%
      filter(election == c(1988, 1995, 2002, 2007, 2012, 2017)[rr]) %>%
      pull(`50%`) %>%
      min()
    out2 <- lapply(seq(-3, 3, length.out = 20), function(ii){
      thresholds <- thres[jj, (seq(rr, 30, 6))[1:data_list$NBlocs_Elections[rr]]]
      out3 <- data.frame(t(c(mu = 2 * atan(ii), compute_theta(ii, thresholds)))) %>%
        mutate(election = rr)
      colnames(out3) <- c("mu", bloc_vector[data_list$included_blocs[rr,]], "election")
      return(out3)
    }) %>%
      do.call("bind_rows", .)
    return(out2)
  }) %>%
    do.call("bind_rows", .)
  return(out1)
}) %>%
  do.call("bind_rows", .) %>%
  pivot_longer(c(-mu, -election),
               names_to = "theta",
               values_to = "prob") %>%
  filter(!is.na(prob)) %>%
  group_by(mu, theta, election) %>%
  summarize(
    q50 = quantile(prob, 0.5),
    q25 = quantile(prob, 0.25),
    q75 = quantile(prob, 0.75),
    q10 = quantile(prob, 0.1),
    q90 = quantile(prob, 0.9)
  )
out <- out %>%
  mutate(
    election = c(1988, 1995, 2002, 2007, 2012, 2017)[election]
  )



cols <- c("Gauche radicale et extreme gauche" = "brown4",
          "Gauche" ="brown2",
          #"Ecologisme" ="limegreen",
          "Centre" ="gold",
          "Droite" ="blue",
          "Droite radicale et extreme droite" ="navyblue")
ggplot() +
  geom_line(data = out, aes(x = mu, y = q50, color = theta)) +
  geom_ribbon(data = out, aes(x = mu, y = q50, ymin = q25, ymax = q75, fill = theta), alpha = 0.25) +
  geom_ribbon(data = out, aes(x = mu, y = q50, ymin = q10, ymax = q90, fill = theta), alpha = 0.10) +
  geom_point(data = mu %>%
               filter(percentage != 0), aes(x = 2 * atan(`50%`), y = percentage, color = theta)) +
  facet_wrap(election ~ .) +
  labs(x = "Latent outcome", y = "Vote share") +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme(legend.title = element_blank())


ggplot() +
  geom_line(data = out, aes(x = mu, y = q50)) +
  geom_ribbon(data = out, aes(x = mu, y = q50, ymin = q25, ymax = q75), alpha = 0.25) +
  geom_ribbon(data = out, aes(x = mu, y = q50, ymin = q10, ymax = q90), alpha = 0.10) +
  geom_point(data = mu, aes(x = `50%`, y = percentage), size = 0.6) +
  facet_wrap(theta ~ election, scales = "free_x", nrow = 6) +
  labs(x = "Latent outcome", y = "Vote share") +
  theme(legend.title = element_blank()) +
  theme_light()





### Circular plot

compute_theta_circle <- function(mu, thres){
  adjustment = pnorm(pi) - pnorm(-pi)
  len_theta = length(thres) + 1
  out_theta = rep(NA, len_theta)
  out_theta[1] = pnorm(thres[1] - mu) - pnorm(-pi - mu)
  for (k in 2:(len_theta - 1)){
    out_theta[k] = pnorm(thres[k] - mu) - pnorm(thres[k - 1] - mu)
  }
  out_theta[len_theta] = pnorm(pi - mu) - pnorm(thres[len_theta - 1] - mu)

  return(out_theta/adjustment)
}

thres <- fit$draws("thresholds") %>%
  posterior::as_draws_matrix()


mu <- fit$summary("mu", ~quantile(., 0.5)) %>%
  add_column(election = c(1988, 1995, 2002, 2007, 2012, 2017)[data_list$id_Obs_elections])

data_frame_outcome <- as.data.frame(data_list$YVoteshare)
colnames(data_frame_outcome) <- bloc_vector
mu <- cbind(mu, data_frame_outcome)
mu <- mu %>%
  select(-variable) %>%
  pivot_longer(c(-election, -`50%`),
               names_to = "theta",
               values_to = "percentage")

out <- lapply(sample(1:1800, 100, replace = FALSE), function(jj){
  out1 <- lapply(1:6, function(rr){
    upper <- mu %>%
      filter(election == c(1988, 1995, 2002, 2007, 2012, 2017)[rr]) %>%
      pull(`50%`) %>%
      max()
    lower <- mu %>%
      filter(election == c(1988, 1995, 2002, 2007, 2012, 2017)[rr]) %>%
      pull(`50%`) %>%
      min()
    out2 <- lapply(seq(-pi, pi, length.out = 20), function(ii){
      thresholds <- thres[jj, (seq(rr, 30, 6))[1:(data_list$NBlocs_Elections[rr] - 1)]]
      out3 <- data.frame(t(c(mu = ii, compute_theta_circle(ii, thresholds)))) %>%
        mutate(election = rr)
      colnames(out3) <- c("mu", bloc_vector[data_list$included_blocs[rr,]], "election")
      return(out3)
    }) %>%
      do.call("bind_rows", .)
    return(out2)
  }) %>%
    do.call("bind_rows", .)
  return(out1)
}) %>%
  do.call("bind_rows", .) %>%
  pivot_longer(c(-mu, -election),
               names_to = "theta",
               values_to = "prob") %>%
  filter(!is.na(prob)) %>%
  group_by(mu, theta, election) %>%
  summarize(
    q50 = quantile(prob, 0.5),
    q25 = quantile(prob, 0.25),
    q75 = quantile(prob, 0.75),
    q10 = quantile(prob, 0.1),
    q90 = quantile(prob, 0.9)
  )
out <- out %>%
  mutate(
    election = c(1988, 1995, 2002, 2007, 2012, 2017)[election]
  )



cols <- c("Gauche radicale et extreme gauche" = "brown4",
          "Gauche" ="brown2",
          "Ecologisme" ="limegreen",
          "Centre" ="gold",
          "Droite" ="blue",
          "Droite radicale et extreme droite" ="navyblue")
ggplot() +
  geom_line(data = out, aes(x = mu, y = q50, color = theta)) +
  geom_ribbon(data = out, aes(x = mu, y = q50, ymin = q25, ymax = q75, fill = theta), alpha = 0.25) +
  geom_ribbon(data = out, aes(x = mu, y = q50, ymin = q10, ymax = q90, fill = theta), alpha = 0.10) +
  geom_point(data = mu %>%
               filter(percentage != 0), aes(x = `50%`, y = percentage, color = theta)) +
  facet_wrap(election ~ .) +
  labs(x = "Latent outcome", y = "Vote share") +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme(legend.title = element_blank())

