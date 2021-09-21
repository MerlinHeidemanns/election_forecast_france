rm(list = ls())
## Poll checks 2017

## Libraries
library(tidyverse)
## Load data
df <- read_csv("dta/polls_dta/2017_polls_compiled.csv")
## End day before start day
field_time <- df %>%
  mutate(field_time = 1 + as.integer(difftime(end_day, start_day, unit = "days"))) %>%
  pull(field_time)
table(field_time)

## Percentage
df %>%
  filter(Candidate != "ZZZ_Abstention") %>%
  group_by(poll_id) %>%
  summarize(total_percentage = sum(Percentage)) %>%
  filter(total_percentage != 100)

## Candidates
# * Are they also distinct
candidates <- df %>%
  pull(Candidate) %>%
  unique() %>%
  sort()
candidates_crosswalk <- data.frame(short_name = candidates,
           long_name = NA)
election_results_2017 <- read_csv("dta/polls_dta/election_results_2017.csv")
for (j in 1:nrow(candidates_crosswalk)){
  if (any(grepl(candidates_crosswalk[j, 1],
                election_results_2017$candidate))){
    candidates_crosswalk[j, 2] = election_results_2017$candidate[grepl(candidates_crosswalk[j, 1],
                                                                       election_results_2017$candidate)]
  }
}
df <- df %>%
  left_join(candidates_crosswalk, by = c("Candidate" = "short_name"))


candidate_short_id_df <- df %>%
  distinct(Candidate) %>%
  arrange(Candidate) %>%
  mutate(candidate_short_id = 1:n())
write.csv(candidate_short_id_df, "dta/polls_dta/candidate_identifiers_2017_short.csv")
candidate_long_id_df <-df %>%
  distinct(long_name) %>%
  arrange(long_name) %>%
  mutate(candidate_long_id = 1:n())
write.csv(candidate_long_id_df, "dta/polls_dta/candidate_identifiers_2017_long.csv")
df <- df %>%
  left_join(candidate_short_id_df) %>%
  left_join(candidate_long_id_df)


## Pollsters
# * Are they also distinct
df <- df %>%
  mutate(
    PollingFirm = ifelse(grepl("Ifop", PollingFirm), "Ifop", PollingFirm),
    PollingFirm = str_remove_all(PollingFirm, "\\*")
  )
df %>%
  pull(PollingFirm) %>%
  unique() %>%
  sort()
pollster_id_df <- df %>%
  distinct(PollingFirm) %>%
  arrange(PollingFirm) %>%
  mutate(pollster_id = 1:n())
write.csv(pollster_id_df, "dta/polls_dta/pollster_identifiers_2017.csv")
df <- df %>%
  left_join(pollster_id_df)

## Plot
ggplot(df, aes(x = start_day, y = Percentage)) +
  geom_point() +
  facet_wrap(Candidate ~ .)

## Turn percentages into integers
df <- df %>%
  group_by(poll_id) %>%
  mutate(abstention = ifelse(Candidate == "ZZZ_Abstention", Percentage, 0),
         abstention = max(abstention),
         Percentage = ifelse(Candidate == "ZZZ_Abstention",
                             Percentage/100,
                             Percentage/100 * (100 - abstention)/100
                             ),
         y = round(SampleSize * Percentage))
write.csv(df, file = "dta/polls_dta/2017_polls_clean.csv")








