################################################################################
## Mangle current polls
################################################################################
## Libraries
library(tidyverse)
################################################################################
## Data
df <- read_csv("dta/polls_dta/polls_2020/polls_position_polls_2022_raw.csv")
################################################################################
## Mangle
main_candidates <- c("Emmanuel Macron", "Marine Le Pen", "Valerie Pecresse",
                     "Anne Hidalgo", "Yannick Jadot", "Eric Zemmour", "Jean Luc Melenchon")
df <- df %>%
  mutate(z = ifelse(candidates %in% main_candidates, 1, 0)) %>%
  group_by(question_id, survey_id) %>%
  mutate(sum_z = sum(z)) %>%
  filter(sum(z) == length(main_candidates)) %>%
  ungroup() %>%
  mutate(
    candidates = case_when(
      candidates == "Nathalie Arthaud" ~ "Arthaud",
      candidates == "Philippe Poutou" ~ "Poutou",
      candidates == "Jean Luc Melenchon" ~ "Melenchon",
      candidates == "Arnaud Montebourg" ~ "Montebourg",
      candidates == "Anne Hidalgo" ~ "Hidalgo",
      candidates == "Yannick Jadot" ~ "Jadot",
      candidates == "Valerie Pecresse" ~ "Pecresse",
      candidates == "Nicolas Dupont Aignan" ~ "Dupont Aignan",
      candidates == "Florian Philippot" ~ "Philippot",
      candidates == "Marine Le Pen" ~ "Le Pen",
      candidates == "Eric Zemmour" ~ "Zemmour",
      candidates == "Jean Lassalle" ~ "Lassalle",
      candidates == "Fabien Roussel" ~ "Roussel",
      candidates == "Francois Asselineau" ~ "Asselineau",
      candidates == "Hel`ene Thouy" ~ "Thouy",
      candidates == "Emmanuel Macron" ~ "Macron",
      candidates == "Jean Frederic Poisson" ~ "Poisson",
      candidates == "Jean Christophe Lagarde" ~ "Lagarde"
    )
  )
################################################################################
## Save
write_csv(df, file = "dta/polls_dta/election_season_model/poll_input_current.csv")
################################################################################