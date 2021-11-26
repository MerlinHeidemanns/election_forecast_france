###############################################################################
## Title: Clean election results regional
###############################################################################
## Libraries
library(tidyverse)
###############################################################################
df_2015 <- read_csv("dta/election_results/election_results_regional/2015_region_t1_candidates.csv") %>%
  mutate(list = str_replace_all(list, "\\s", ""))
df_2015_list <- read_csv("dta/election_results/election_results_regional/2015_lists.csv") %>%
  mutate(list = str_replace_all(list, "\\s", ""))

df_2021 <- read_csv("dta/election_results/election_results_regional/2021_regional_1t.csv")

df_insee_codes <- read_csv("dta/france_classification/departements.csv") %>%
  mutate(
    departement = str_replace_all(departement, "\\-", " "),
    departement = stringi::stri_trans_general(departement, "Latin-ASCII")
  )
###############################################################################
## 2021
df_2021 %>%
  mutate(
    department = str_replace_all(department, "\\-", " "),
    department = stringi::stri_trans_general(department, "Latin-ASCII"),
    department = ifelse(department == "Pyrenees Atlantique","Pyrenees Atlantiques",department)
  )  %>%
  group_by(department, bloc) %>%
  summarize(percentage = sum(as.numeric(percentage_expressed), na.rm = TRUE))
## 2015
df_2015 <- df_2015 %>%
  left_join(
    df_2015_list
  ) %>%
  mutate(
    department = str_replace_all(department, "\\-", " "),
    department = stringi::stri_trans_general(department, "Latin-ASCII"),
    department = ifelse(department == "Pyrenees Atlantique","Pyrenees Atlantiques",department)
  ) %>%
  group_by(department, bloc) %>%
  summarize(percentage = sum(percentage)) %>%
  group_by(department) %>%
  mutate(percentage = percentage/sum(percentage))










