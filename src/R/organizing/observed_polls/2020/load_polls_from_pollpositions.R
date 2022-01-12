rm(list = ls())
###############################################################################
## Title: Load current polling data from PollPositions
###############################################################################
## Libraries
library(tidyverse)
library(RJSONIO)
library(RCurl)
###############################################################################

# grab the data
raw_data <- utils::URLencode("https://raw.githubusercontent.com/pollsposition/data/main/sondages/presidentielles_2022.json")
# Then covert from JSON into a list in R
data <- fromJSON(raw_data)

all_polls <- lapply(1:length(data[[1]]), function(jj){
  survey <- data[[1]][[jj]]
  institute <- survey$institut[1]
  date_debut <- survey$date_debut
  date_fin <- survey$date_fin
  methode <- survey$methode
  interroges <- survey$interroges
  lien <- survey$source

  polls <- data.frame()
  if ("premier_tour" %in% names(survey)){
    results_premier_tour <- survey$premier_tour
    premier_tour_questions <- lapply(1:length(results_premier_tour), function(ii){
      question <- results_premier_tour[[ii]]
      nspp <- question$nspp
      if (is.null(nspp)) nspp <- -1
      base <- question$base
      intentions_exprimees <- question$intentions_exprimees
      if (is.null(intentions_exprimees)) intentions_exprimees <- -1
      values <- question$intentions %>% as.numeric()
      names <- names(question$intentions)
      df <- data.frame(
        institute = institute,
        date_debut = date_debut,
        date_fin = date_fin,
        methode = methode,
        interroges = interroges,
        question_id = ii,
        nspp = nspp,
        base = base,
        intentions_exprimees = intentions_exprimees,
        candidates = names,
        percentages = values,
        lien = lien
      )

      if ("certitude" %in% names(question)){
        if ("detail" %in% names(question$certitude)){
          certitude <- data.frame(candidates = names(question$certitude$detail),
                                  certitude = as.numeric(question$certitude$detail))
          df <- df  %>%
            left_join(certitude)
        }
      }
      if (any(df$nspp == -1)){
        return()
      } else {
        return(df)
      }
    }) %>%
      do.call("bind_rows", .)

    polls <- bind_rows(
      polls,
      premier_tour_questions
    )
  }
  if ("second_tour" %in% names(survey)){
    results_second_tour <- survey$second_tour
    second_tour_questions <- lapply(1:length(results_second_tour), function(ii){
      question <- results_second_tour[[ii]]
      nspp <- question$nspp
      if (is.null(nspp)) nspp <- -1
      base <- question$base
      intentions_exprimees <- question$intentions_exprimees
      if (is.null(intentions_exprimees)) intentions_exprimees <- -1
      values <- question$intentions %>% as.numeric()
      names <- names(question$intentions)

      df <- data.frame(
        institute = institute,
        date_debut = date_debut,
        date_fin = date_fin,
        methode = methode,
        interroges = interroges,
        question_id = ii + 100,
        nspp = nspp,
        base = base,
        intentions_exprimees = intentions_exprimees,
        candidates = names,
        percentages = values,
        lien = lien
      )

      if ("certitude" %in% names(question)){
        if ("detail" %in% names(question$certitude)){
          certitude <- data.frame(candidates = names(question$certitude$detail),
                                  certitude = as.numeric(question$certitude$detail))
          df <- df  %>%
            left_join(certitude)
        }
      }


      if (any(df$nspp == -1)){
        return()
      } else {
        return(df)
      }
    }) %>%
      do.call("bind_rows", .)

    polls <- bind_rows(
      polls,
      second_tour_questions
    )
  }

  polls <- polls %>%
    mutate(survey_id = jj)

  return(polls)
}) %>%
  do.call("bind_rows", .)
#' Remove accented characters
all_polls <- all_polls %>%
  mutate(candidates = str_replace(iconv(candidates,to="ASCII//TRANSLIT"),
                                  "'",""),
         candidates = str_replace(candidates, "'", ""),
         candidates = str_replace(candidates, "-", " "))
#' Remove the extreme gauche poll
all_polls <- all_polls %>%
  group_by(survey_id, question_id, institute, date_fin) %>%
  mutate(remove = max(candidates %in%
                        c("Extr^eme Gauche",
                          "Sandrine Rousseau",
                          "Eric Ciotti",
                          "Philippe Juvin")) ) %>%
  filter(remove == 0)
institutes_remove <- all_polls %>%
  distinct(institute, survey_id, date_fin) %>%
  group_by(institute) %>%
  summarize(n = n()) %>%
  filter(n == 1) %>%
  pull(institute)
all_polls <- all_polls %>%
  filter(!institute %in% institutes_remove)

## Save
write.csv(all_polls,
          file = "dta/polls_dta/polls_2020/polls_position_polls_2022_raw.csv")
###############################################################################