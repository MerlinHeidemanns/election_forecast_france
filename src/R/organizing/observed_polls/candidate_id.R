rm(list = ls())
## Libraries
library(tidyverse)
## Load data
candidate_id <- read_csv("dta/polls_dta/party_association.csv")
## Remove -
candidate_id <- candidate_id %>%
  mutate(candidate = str_replace_all(candidate, "\\-", " "),
         long_name = NA,
         candidate_long_id = NA) %>%
  distinct(candidate, party, bloc, year, long_name, candidate_long_id)

candidate_id_df_past <- read_csv("dta/polls_dta/candidate_identifiers_long.csv")

for (j in 1:nrow(candidate_id)){
  year_select <- candidate_id$year[j]
  candidate <- candidate_id$candidate[j]
  possible_candidates <- candidate_id_df_past %>%
    filter(year == year_select) %>%
    pull(long_name)
  candidate_long_ids <- candidate_id_df_past %>%
    filter(year == year_select) %>%
    pull(candidate_long_id)
  long_name <- possible_candidates[grepl(candidate, possible_candidates)]
  candidate_long_id <- candidate_long_ids[grepl(candidate, possible_candidates)]
  if (length(long_name) > 0){
    candidate_id$long_name[j] <-  long_name
    candidate_id$candidate_long_id[j] <-  candidate_long_id
  }
}
candidate_id <- candidate_id %>%
  filter(!is.na(candidate_long_id))

## Write
write.csv(candidate_id, "dta/polls_dta/party_association_clean.csv")

