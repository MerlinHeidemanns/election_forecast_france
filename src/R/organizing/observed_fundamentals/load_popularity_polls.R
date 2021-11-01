###############################################################################
## Title: Download approval presidential approval polls
###############################################################################
## Libraries
library(RCurl)
###############################################################################
## Load data
url <- getURL("https://raw.githubusercontent.com/pollsposition/models/master/popularity/plot_data/raw_polls.csv")
df <- read.csv(text = x)
election_dates <- read_csv("dta/polls_dta/election_dates.csv")
###############################################################################
## Clean
#' column names
colnames(df) <- c("date", "president", "pollster", "N", "method", "p_approve",
                  "p_disapprove")
df <- df %>%
  #' term
  mutate(
    term_id = as.integer(str_match(president, "([\\d+]+)")[,2]),
    term_id = ifelse(is.na(term_id), 1, term_id)
  ) %>%
  #' pollster_id
  group_by(pollster) %>%
  mutate(
    pollster_id = cur_group_id()
  ) %>%
  #' method_id
  group_by(method) %>%
  mutate(
    method_id = cur_group_id()
  ) %>%
  ungroup() %>%
  #' candidate_id
  mutate(
    president_id = as.integer(factor(president, levels = c("chirac2", "sarkozy", "hollande", "macron")))
  ) %>%
  #' integer values
  mutate(
    y = as.integer(p_approve * N)
  ) %>%
  #' election_date
  left_join(
    election_dates %>%
      add_column(president = c("", "macron", "hollande", "sarkozy", "chirac2", "chirac1"))
  ) %>%
  #' weeks since election
  mutate(three_weeks =
    ceiling(
      as.numeric(
        difftime(date, date_second_round, units = "weeks")
      )/3
    )
  ) %>%
  #' month, year, year
  mutate(
    month = lubridate::month(date),
    years = lubridate::year(date) - lubridate::year(date_second_round) + 1
  )
###############################################################################
## Write
write_csv(df, file = "dta/fundamentals_dta/approval.csv")




