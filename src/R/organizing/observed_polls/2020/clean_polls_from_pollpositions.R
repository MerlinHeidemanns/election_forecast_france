rm(list = ls())
###############################################################################
## Title: Load current polling data from PollPositions
###############################################################################
## Libraries
library(tidyverse)
###############################################################################
## Load
df <- read_csv("dta/polls_dta/polls_2020/polls_position_polls_2022_raw.csv")
###############################################################################
## Cleaning
#' Remove extreme gauche poll
df <- df %>%
  group_by(survey_id, question_id) %>%
  mutate(remove = max(candidates == "Extrme Gauche")) %>%
  filter(remove == 0)
###############################################################################
## Adjusting
#' Unique question id



lapply(1:)
# df_plt <- df %>%
#   mutate(abstention_total = 1 - intentions_exprimees/interroges) %>%
#   distinct(abstention_total, nspp, base, survey_id)
# ggplot(df_plt, aes(x = nspp/100, y = abstention_total, color = base)) +
#   geom_point() +
#   geom_abline(aes(intercept = 0, slope = 1)) +
#   labs(
#     x = "No intention to vote having indicated a high likelihood to vote",
#     y = "Unlikely to vote + no intention to vote"
#   ) +
#   coord_equal()