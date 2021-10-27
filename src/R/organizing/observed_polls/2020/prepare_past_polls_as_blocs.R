################################################################################
## Title: Prepare past polls as blocs
################################################################################
## Empty environment
rm(list = ls())
################################################################################
## Libraries
library(tidyverse)
################################################################################
## Vectors
bloc_vector <- c("Abstention",
                 "Gauche radicale et extreme gauche",
                 "Gauche",
                 "Centre",
                 "Droite",
                 "Droite radicale et extreme droite")
cols <- c("Abstention" = "black",
          "Gauche radicale et extreme gauche" = "red",
          "Gauche" = "pink",
          "Centre" = "orange",
          "Droite" = "blue",
          "Droite radicale et extreme droite" = "dark blue"
          )
################################################################################
## Load data
df_2002 <- read_csv(file = "dta/polls_dta/2002_polls_clean.csv")
df_2007 <- read_csv(file = "dta/polls_dta/2007_polls_clean.csv")
df_2012 <- read_csv(file = "dta/polls_dta/2012_polls_clean.csv")
df_2017 <- read_csv(file = "dta/polls_dta/2017_polls_clean.csv")
df_bloc_id <- read_csv("dta/polls_dta/candidate_bloc_association.csv")
################################################################################
## Mangle
#' rowbind past polls
df <- bind_rows(
  df_2002,
  df_2007,
  df_2012,
  df_2017
)
#' Merge
df_w_bloc <- df %>%
  left_join(df_bloc_id %>%
              distinct(bloc, party,
                     candidate, election_year)) %>%
  distinct(poll_id, candidate, .keep_all = TRUE) %>%
  #' filter
  #' The filter below drops the two other candidates Dieudonne and Cheminade
  #'filter(is.na(bloc)) %>% distinct(candidate)
  filter(!is.na(bloc)) %>%
  mutate(bloc = factor(bloc, levels = bloc_vector))
df_summarized <- df_w_bloc %>%
  group_by(poll_id, start_day, end_day,
           pollName,
           pollster_id, abstention, election_year,
           bloc) %>%
  summarize(y = sum(y)) %>%
  mutate(jj = as.integer(bloc)/2 >= 1) %>%
  filter(sum(jj) >= 5) %>%
  select(-jj) %>%
  group_by(poll_id) %>%
  mutate(n = sum(y))
#' Save
write.csv(df_summarized, "dta/polls_dta/polls_02_17/polls_02_17_summarized_blocs.csv")
################################################################################
#
# ggplot(df_summarized, aes(x = start_day, y = y/n, color = bloc)) +
#   geom_point(alpha = 0.5, size = 0.8) +
#   geom_smooth(method = "loess", span = 1, se = 0) +
#   theme_light() +
#   scale_color_manual(values = cols) +
#   theme(legend.position = "bottom",
#         axis.title.x = element_blank()) +
#   labs(y = "Support")


