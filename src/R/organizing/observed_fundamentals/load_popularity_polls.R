###############################################################################
## Title: Download approval presidential approval polls
###############################################################################
## Libraries
library(RCurl)
###############################################################################
## Load data
url <- getURL("https://raw.githubusercontent.com/pollsposition/data/main/sondages/popularite.csv")
df <- read.csv(text = url)
election_dates <- read_csv("dta/polls_dta/election_dates.csv")
###############################################################################
## President bloc
bloc_vector <- c("Abstention",
                 "Gauche radicale et extreme gauche",
                 "Gauche",
                 "Centre",
                 "Droite",
                 "Droite radicale et extreme droite")
president_bloc <- data.frame(
  president = c("vge","mitterrand1" ,"mitterrand2" ,"chirac1" ,
                "chirac2" ,"sarkozy" ,"hollande" ,"macron"),
  bloc = c("Droite", "Gauche", "Gauche", "Droite", "Droite",
           "Droite", "Gauche", "Centre")
) %>%
  mutate(bloc_id = match(bloc, bloc_vector))
###############################################################################
## Clean
#' column names
colnames(df) <- c("date", "president", "pollster", "N", "method", "p_approve",
                  "p_disapprove")
president_vector <- df %>%
  distinct(president, .keep_all = TRUE) %>%
  arrange(date) %>%
  pull(president)
df <- df %>%
  #' change scale
  mutate(
    p_approve = p_approve/100,
    p_disapprove = p_disapprove/100
  ) %>%
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
    president_id = as.integer(factor(president, levels = president_vector))
  ) %>%
  #' integer values
  mutate(
    y = as.integer(p_approve * N)
  ) %>%
  #' election_date
  left_join(
    election_dates %>%
      mutate(date_next_first_round = lag(date_first_round)) %>%
      add_column(elected_president =
                   c("", rev(president_vector), "")),
      by = c("president" = "elected_president")
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
  ) %>%
  left_join(president_bloc)
###############################################################################
## Write
write_csv(df, file = "dta/fundamentals_dta/approval.csv")
###############################################################################