rm(list = ls())
## Libraries
library(tidyverse)
## Load data
election_results_2017 <- read_csv("dta/polls_dta/election_results_2017.csv")
## Normalize
election_results_2017 <- election_results_2017 %>%
  mutate(abstention = ifelse(candidate == "Abstention", percentage, 0),
         abstention = max(abstention),
         percentage = ifelse(candidate != "Abstention",
                             percentage/100 * (100 - abstention)/100, percentage/100)) %>%
  dplyr::select(-abstention)

## Save
write_csv(election_results_2017, "dta/polls_dta/election_results_2017.csv")
