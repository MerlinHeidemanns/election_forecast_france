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

## percentage
df %>%
  filter(candidate != "_Abstention") %>%
  group_by(poll_id) %>%
  summarize(total_percentage = sum(percentage)) %>%
  filter(total_percentage != 100)

## candidates
# * Are they also distinct
candidates <- df %>%
  pull(candidate) %>%
  unique() %>%
  sort()
candidates_crosswalk <- data.frame(short_name = candidates,
           long_name = NA)
election_results_2017 <- read_csv("dta/polls_dta/election_results_clean.csv") %>%
  filter(year == 2017)
for (j in 1:nrow(candidates_crosswalk)){
  if (any(grepl(candidates_crosswalk[j, 1],
                election_results_2017$candidate))){
    candidates_crosswalk[j, 2] = election_results_2017$candidate[grepl(candidates_crosswalk[j, 1],
                                                                       election_results_2017$candidate)]
  }
}
df <- df %>%
  left_join(candidates_crosswalk, by = c("candidate" = "short_name"))


candidate_short_id_df <- df %>%
  distinct(candidate) %>%
  arrange(candidate) %>%
  mutate(candidate_short_id = 1:n())
write.csv(candidate_short_id_df, "dta/polls_dta/candidate_identifiers_2017_short.csv")
candidate_long_id_df <-df %>%
  distinct(long_name) %>%
  arrange(long_name) %>%
  filter(!is.na(long_name)) %>%
  mutate(candidate_long_id = 1:n())
write.csv(candidate_long_id_df, "dta/polls_dta/candidate_identifiers_2017_long.csv")
df <- df %>%
  left_join(candidate_short_id_df) %>%
  left_join(candidate_long_id_df)


## Pollsters
# * Are they also distinct
df <- df %>%
  mutate(
    pollName = ifelse(grepl("Ifop", pollName), "Ifop", pollName),
    pollName = str_remove_all(pollName, "\\*")
  )
df %>%
  pull(pollName) %>%
  unique() %>%
  sort()
pollster_id_df <- df %>%
  distinct(pollName) %>%
  arrange(pollName) %>%
  mutate(pollster_id = 1:n())
write.csv(pollster_id_df, "dta/polls_dta/pollster_identifiers_2017.csv")
df <- df %>%
  left_join(pollster_id_df)

## Plot
ggplot(df, aes(x = start_day, y = percentage)) +
  geom_point() +
  facet_wrap(candidate ~ .)

## Turn percentages into integers
df <- df %>%
  group_by(poll_id) %>%
  mutate(abstention = ifelse(candidate == "_Abstention", percentage, 0),
         abstention = max(abstention),
         percentage = ifelse(candidate == "_Abstention",
                             percentage/100,
                             percentage/100 * (100 - abstention)/100
                             ),
         y = round(sampleSize * percentage))

## Add election year
df <- df %>%
  mutate(election_year = 2017)

write.csv(df, file = "dta/polls_dta/2017_polls_clean.csv")








