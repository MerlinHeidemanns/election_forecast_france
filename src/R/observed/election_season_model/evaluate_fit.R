###############################################################################
## Title: Evaluate output of election season model
###############################################################################
## Clean environment
rm(list = ls())
###############################################################################
## Load libraries
library(tidyverse)
library(cmdstanr)
###############################################################################
## Vectors
bloc_vector <- c("Abstention",
                 "Gauche radicale et extreme gauche",
                 "Gauche",
                 "Centre",
                 "Droite",
                 "Droite radicale et extreme droite")
###############################################################################
## Load data
data_file <- read_rds(file = "dta/fits/2021_10_30_election_season_list.Rds")
data_file <- read_rds(file = "dta/fits/2021_10_30_election_season_list_ar.Rds")
###############################################################################
## Theta
ppc_obs_theta_candidates <- function(data_file){
  theta_candidates <- data_file$fit$draws("theta_candidates") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    filter(iter <= 1000) %>%
    pivot_longer(c(-iter),
                 names_to = "variable",
                 values_to = "draws")
  theta_candidates <- theta_candidates %>%
    mutate(
      election_year = as.integer(
        str_match(variable, "\\[([\\d]+),")[, 2]) * 5 + 1997,
      time_id = as.integer(
        str_match(variable, ",([\\d]+)\\]")[, 2]),
      candidate_id = as.integer(
        str_match(variable, ",([\\d]+),")[, 2])
    ) %>%
    left_join(
      data.frame(
        election_year = seq(2002, 2017, 5),
        candidates = data_file$data_list$NCandidates
      )
    ) %>%
    filter(
      candidate_id <= candidates
    ) %>%
    group_by(iter, election_year, time_id) %>%
    mutate(
      draws = exp(draws)/sum(exp(draws))
    )
  theta_candidates <- theta_candidates %>%
    filter(!is.nan(election_year), !is.nan(draws), !is.na(time_id), !is.nan(candidate_id)) %>%
    group_by(election_year, time_id, candidate_id) %>%
    summarize(
      q50 = quantile(draws, 0.5),
      q25 = quantile(draws, 0.25),
      q75 = quantile(draws, 0.75),
      q10 = quantile(draws, 0.10),
      q90 = quantile(draws, 0.90)
    )
  theta_candidates <- theta_candidates %>%
    mutate(bloc_id =
             ifelse(election_year == 2002,
                    data_file$data_list$id_C_blocs[1,][candidate_id],
                    ifelse(election_year == 2007,
                           data_file$data_list$id_C_blocs[2,][candidate_id],
                           ifelse(election_year == 2012,
                                  data_file$data_list$id_C_blocs[3,][candidate_id],
                                  data_file$data_list$id_C_blocs[4,][candidate_id]
                           )))) %>%
    left_join(
      data_file$data_polls %>%
        distinct(election_id,
                 candidate_id,
                 candidate) %>%
        mutate(election_year = election_id * 5 + 1997)
    )%>%
    mutate(bloc_id = ifelse(candidate_id == 1, "Abstention", bloc_id),
           candidate = ifelse(candidate_id == 1, "Abstention", candidate))

  plts <- lapply(seq(2002, 2017, 5), function(ii){
    plt <- ggplot(theta_candidates %>%
                    filter(election_year == ii),
                  aes(x = time_id, y = q50)) +
      geom_line(aes(color = candidate)) +
      geom_ribbon(aes(ymin =q25, ymax = q75, fill = candidate), alpha = 0.25) +
      geom_ribbon(aes(ymin =q10, ymax = q90, fill = candidate), alpha = 0.25) +
      facet_wrap(bloc_id ~.) +
      theme_light()
  })
  return(plts)
}
plts_theta <- ppc_obs_theta_candidates(data_file)
###############################################################################
## Cov theta
sigma_cov <- data_file$fit$summary("sigma_cov",
                                ~quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9)))
colnames(sigma_cov) <- c("variable", "q10", "q25", "q50", "q75", "q90")
sigma_cov <- sigma_cov %>%
  mutate(
    candidate_id = 1 + as.integer(
      str_match(variable, "\\[([\\d]+),")[, 2]),
    election_id = as.integer(
      str_match(variable, "([\\d]+)\\]")[, 2]),
    election_year = election_id * 5 + 1997,
    bloc_id =
      ifelse(election_year == 2002,
             data_file$data_list$id_C_blocs[1,][candidate_id],
       ifelse(election_year == 2007,
            data_file$data_list$id_C_blocs[2,][candidate_id],
      ifelse(election_year == 2012,
            data_file$data_list$id_C_blocs[3,][candidate_id],
            data_file$data_list$id_C_blocs[4,][candidate_id]
    )))) %>%
  left_join(
    data_file$data_polls %>%
      distinct(election_id,
               candidate_id,
               candidate) %>%
      mutate(election_year = election_id * 5 + 1997)
  )%>%
  left_join(
    data.frame(
      election_year = seq(2002, 2017, 5),
      candidates = data_file$data_list$NCandidates
    )
  ) %>%
  filter(
    candidate_id <= candidates
  )
ggplot(sigma_cov, aes(x = bloc, y = q50, color = as.factor(election_year))) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size = 0.5,
                position = position_dodge(width = 0.5))
###############################################################################
alpha <- data_file$fit$draws("alpha") %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  filter(iter <= 1000) %>%
  pivot_longer(c(-iter),
               names_to = "variable",
               values_to = "draws")
alpha <- alpha %>%
  mutate(
    candidate_id = as.integer(
      str_match(variable, "\\[([\\d]+),")[, 2]),
    pollster_id = as.integer(
      str_match(variable, "([\\d]+)\\]")[, 2]),
  ) %>%
  left_join(
    data.frame(
      pollster_id = data_file$data_list$id_S_pollster ,
      election_id = data_file$data_list$id_S_season,
      candidates = data_file$data_list$NCandidates[data_file$data_list$id_S_season]
    ) %>%
      distinct()
  ) %>%
  filter(
    candidate_id <= candidates
  ) %>%
  mutate(
    election_year = election_id * 5 + 1997,
    bloc_id =
           ifelse(election_year == 2002,
                  data_file$data_list$id_C_blocs[1,][candidate_id],
                  ifelse(election_year == 2007,
                         data_file$data_list$id_C_blocs[2,][candidate_id],
                         ifelse(election_year == 2012,
                                data_file$data_list$id_C_blocs[3,][candidate_id],
                                data_file$data_list$id_C_blocs[4,][candidate_id]
                         )))) %>%
  left_join(
    data_file$data_polls %>%
      distinct(election_id,
               candidate_id,
               candidate) %>%
      mutate(election_year = election_id * 5 + 1997)
  )%>%
  mutate(bloc_id = ifelse(candidate_id == 1, "Abstention", bloc_id),
         candidate = ifelse(candidate_id == 1, "Abstention", candidate))

alpha_candidate <- alpha %>%
  group_by(bloc_id, candidate, pollster_id, election_year) %>%
  summarize(
    q50 = quantile(draws, 0.5),
    q25 = quantile(draws, 0.25),
    q75 = quantile(draws, 0.75),
    q10 = quantile(draws, 0.10),
    q90 = quantile(draws, 0.90)
  )

ggplot(alpha %>% filter(election_year == 2017),
       aes(x = interaction(candidate, bloc_id),
           y = q50,
           color = as.factor(pollster_id))) +
  geom_point(position = position_dodge(width = 0.2)) +
  coord_flip()


alpha_bloc <- alpha %>%
  group_by(bloc_id, pollster_id, election_year) %>%
  summarize(
    q50 = quantile(draws, 0.5),
    q25 = quantile(draws, 0.25),
    q75 = quantile(draws, 0.75),
    q10 = quantile(draws, 0.10),
    q90 = quantile(draws, 0.90)
  )
ggplot(alpha_bloc %>% filter(election_year == 2012),
       aes(x = bloc_id,
           y = q50,
           color = as.factor(pollster_id))) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = q25, ymax = q75),
                size = 0.5, width = 0,
                position = position_dodge(width = 0.2)) +
  coord_flip()


