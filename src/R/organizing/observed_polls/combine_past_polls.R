rm(list = ls())
## Libraries
library(tidyverse)
## Load data
df_2017 <- read_csv("dta/polls_dta/2017_polls_clean.csv")
df_2012 <- read_csv("dta/polls_dta/2012_polls_clean.csv")
df_2007 <- read_csv("dta/polls_dta/2007_polls_clean.csv")
df_2002 <- read_csv("dta/polls_dta/2002_polls_clean.csv")

## Bind
df_joint <- bind_rows(df_2017,
                      df_2012,
                      df_2007,
                      df_2002)

## Save
write_csv(df_joint, "dta/polls_dta/past_polls_clean.csv")


## Pollster identifiers
pollster_id_df_2017 <- read_csv("dta/polls_dta/pollster_identifiers_2017.csv") %>%
  mutate(year = 2017)
pollster_id_df_2012 <- read_csv("dta/polls_dta/pollster_identifiers_2012.csv") %>%
  mutate(year = 2012)
pollster_id_df_2007 <- read_csv("dta/polls_dta/pollster_identifiers_2007.csv") %>%
  mutate(year = 2007)
pollster_id_df_2002 <- read_csv("dta/polls_dta/pollster_identifiers_2002.csv") %>%
  mutate(year = 2002)

## Bind
pollster_id_df <- bind_rows(
  pollster_id_df_2017,
  pollster_id_df_2012,
  pollster_id_df_2007,
  pollster_id_df_2002) %>%
  dplyr::select(-X1)

write.csv(pollster_id_df, "dta/polls_dta/pollster_identifiers_past.csv")


## Identifiers
candidate_long_id_2017 <- read_csv("dta/polls_dta/candidate_identifiers_2017_long.csv") %>%
  mutate(year = 2017)
candidate_long_id_2012 <- read_csv("dta/polls_dta/candidate_identifiers_2012_long.csv") %>%
  mutate(year = 2012)
candidate_long_id_2007 <- read_csv("dta/polls_dta/candidate_identifiers_2007_long.csv") %>%
  mutate(year = 2007)
candidate_long_id_2002 <- read_csv("dta/polls_dta/candidate_identifiers_2002_long.csv") %>%
  mutate(year = 2002)

## Bind
candidate_id_df <- bind_rows(
  candidate_long_id_2017,
  candidate_long_id_2012,
  candidate_long_id_2007,
  candidate_long_id_2002) %>%
  dplyr::select(-X1) %>%
  mutate(long_name = ifelse(grepl("Hollande", long_name),
                            "Francois Hollande",
         long_name),
         long_name = str_replace_all(long_name, "\\-", " "))

write.csv(candidate_id_df, "dta/polls_dta/candidate_identifiers_long.csv")

