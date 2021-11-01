rm(list = ls())
## Libraries
library(tidyverse)
## Load data
election_results <- read.csv("dta/polls_dta/election_results.csv", encoding = "UTF-8")

## Normalize
election_results <- election_results %>%
  group_by(year) %>%
  mutate(abstention = ifelse(candidate == "Abstention", percentage, 0),
         abstention = max(abstention),
         percentage = ifelse(candidate != "Abstention",
                             percentage/100 * (100 - abstention)/100, percentage/100)) %>%
  dplyr::select(-abstention) %>%
  mutate(candidate = ifelse(candidate == "Abstention", "_Abstention", candidate),
         candidate = stringi::stri_trans_general(candidate, "Latin-ASCII"),
         candidate = str_replace_all(candidate, "-", " "),
         long_name = ifelse(long_name == "Abstention", "_Abstention", long_name),
         long_name = stringi::stri_trans_general(long_name, "Latin-ASCII"),
         long_name = str_replace_all(long_name, "-", " "))

## Save
write_csv(election_results, "dta/polls_dta/election_results_clean.csv")
