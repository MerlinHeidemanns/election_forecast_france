###############################################################################
## Title: Clean election results regional
###############################################################################
rm(list = ls())
###############################################################################
## Libraries
library(tidyverse)
###############################################################################
## 1986
df_1986 <- read_csv("dta/election_results/election_results_regional/1986_regional.csv")
list_1986 <- read_csv("dta/election_results/election_results_regional/1986_lists.csv")
## 1992
df_1992 <- read_csv("dta/election_results/election_results_regional/1992_regional.csv")
list_1992 <- read_csv("dta/election_results/election_results_regional/1992_lists.csv")
## 1998
df_1998 <- read_csv("dta/election_results/election_results_regional/1998_regional.csv")
df_1998_inscrits <- read_csv("dta/election_results/election_results_regional/1998_region_t1_inscrits.csv")

## 2004
df_2004 <- read_csv("dta/election_results/election_results_regional/2004_regional.csv")
list_2004 <- read_csv("dta/election_results/election_results_regional/2004_lists.csv")
## 2004
df_2010 <- read_csv("dta/election_results/election_results_regional/2010_regional.csv")
list_2010 <- read_csv("dta/election_results/election_results_regional/2010_lists.csv")
## 2015
df_2015 <- read_csv("dta/election_results/election_results_regional/2015_region_t1_candidates.csv") %>%
  mutate(list = str_replace_all(list, "\\s", ""))
df_2015_list <- read_csv("dta/election_results/election_results_regional/2015_lists.csv") %>%
  mutate(list = str_replace_all(list, "\\s", ""))
df_2015_inscrits <- read_csv("dta/election_results/election_results_regional/2015_region_t1_inscrits.csv")
## 2021
df_2021 <- read_csv("dta/election_results/election_results_regional/2021_regional_1t.csv")
## Codes
df_insee_codes <- read_csv("dta/france_classification/departements.csv") %>%
  mutate(
    departement = str_replace_all(departement, "\\-", " "),
    departement = stringi::stri_trans_general(departement, "Latin-ASCII")
  )
###############################################################################
## 2021
df_2021 <- df_2021 %>%
  mutate(
    department = str_replace_all(department, "\\-", " "),
    department = stringi::stri_trans_general(department, "Latin-ASCII"),
    department = ifelse(department == "Pyrenees Atlantique","Pyrenees Atlantiques",department)
  )  %>%
  group_by(department, bloc) %>%
  summarize(percentage = sum(as.numeric(percentage_inscriped), na.rm = TRUE)/100,
            votes = sum(votes)) %>%
  ungroup()
df_2021 <- df_2021 %>%
  bind_rows(
    df_2021 %>%
      group_by(department) %>%
      summarize(
       votes = (1 - sum(percentage)) * sum(votes),
       percentage = (1 - sum(percentage))) %>%
      mutate(bloc = "Abstention") %>%
      ungroup()
  ) %>%
  arrange(department) %>%
  mutate(year = 2021) %>%
  select(year, department, percentage,bloc, votes)
###############################################################################
## 2015
df_2015 <- df_2015 %>%
  left_join(
    df_2015_list
  ) %>%
  mutate(
    department = str_replace_all(department, "\\-", " "),
    department = stringi::stri_trans_general(department, "Latin-ASCII"),
    department = ifelse(department == "Pyrenees Atlantique","Pyrenees Atlantiques",department),
    votes = as.integer(str_replace(votes, "[\\s\\?]", ""))
  ) %>%
  mutate(department_cap = toupper(department)) %>%
  left_join(df_2015_inscrits, by = c("department_cap" = "department")) %>%
  select(-department_cap)
df_2015 <- df_2015 %>%
  bind_rows(
    df_2015 %>%
      mutate(percentage = votes/Inscrits) %>%
      group_by(department) %>%
      summarize(votes = mean(Inscrits * (1 - sum(percentage)))) %>%
      mutate(bloc = "Abstention")
  ) %>%
  group_by(department, bloc) %>%
  summarize(votes = sum(votes)) %>%
  group_by(department) %>%
  mutate(percentage = votes/sum(votes)) %>%
  mutate(year = 2015) %>%
  select(year, department, percentage,bloc, votes)
###############################################################################
## 2010
df_2010 <- lapply(1:13, function(x){
  df <- df_2010 %>%
    select(departement, circonscription, `Taux de participation`,
           ends_with(paste(" ",x, sep = "")) & !contains("X"))
  colnames(df) <- c("departement", "circonscription", "taux", "nuance", "prenom",
                    "nom", "liste", "vox")
  return(df)
}) %>%
  do.call("bind_rows", .) %>%
  left_join(list_2010) %>%
  filter(!is.na(bloc)) %>%
  mutate(departement = str_replace_all(departement, "\\-", " "),
         departement_lower = tolower(departement)) %>%
  select(-departement) %>%
  left_join(
    df_insee_codes %>%
      mutate(departement_lower = tolower(departement))
  ) %>%
  select(-departement_lower)
df_2010 <- df_2010 %>%
  bind_rows(
    df_2010 %>%
      group_by(departement, circonscription) %>%
      summarise(vox = sum((1 - taux/100) * vox)) %>%
      mutate(bloc = "Abstention")
  ) %>%
  group_by(departement, bloc) %>%
  summarize(votes = sum(as.integer(`vox`))) %>%
  group_by(departement) %>%
  mutate(percentage = votes/sum(votes)) %>%
  mutate(year = 2010) %>%
  rename(department = departement) %>%
  select(year, department, percentage,bloc, votes)
###############################################################################
## 2004
df_2004 <- lapply(1:19, function(x){
  df <- df_2004 %>%
    select(departement, Circonscription, `Taux de participation`,
           ends_with(paste(" ",x, sep = "")) & !contains("X"))
  colnames(df) <- c("departement", "circonscription", "taux", "nuance", "prenom",
                    "nom", "liste", "vox")
  return(df)
}) %>%
  do.call("bind_rows", .) %>%
  left_join(list_2004) %>%
  filter(!is.na(bloc)) %>%
  mutate(departement = str_replace_all(departement, "\\-", " "),
         departement_lower = tolower(departement)) %>%
  select(-departement) %>%
  left_join(
    df_insee_codes %>%
      mutate(departement_lower = tolower(departement))
  ) %>%
  select(-departement_lower)
df_2004 <- df_2004 %>%
  bind_rows(
    df_2004 %>%
      group_by(departement, circonscription) %>%
      summarise(vox = sum((1 - taux/100) * vox)) %>%
      mutate(bloc = "Abstention")
  ) %>%
  group_by(departement, bloc) %>%
  summarize(votes = sum(as.integer(`vox`))) %>%
  group_by(departement) %>%
  mutate(percentage = votes/sum(votes),
         year = 2004) %>%
  rename(department = departement) %>%
  select(year, department, percentage,bloc, votes)
###############################################################################
## 1998
df_1998 <- df_1998 %>%
  mutate(departement = str_replace_all(departement, "\\-", " "),
         departement_lower = tolower(departement)) %>%
  select(-departement) %>%
  left_join(
    df_insee_codes %>%
      mutate(departement_lower = tolower(departement))
  ) %>%
  select(-departement_lower) %>%
  group_by(departement, bloc) %>%
  summarize(votes = sum(as.integer(`Nombre de voix 1`))) %>%
  mutate(departement_cap = toupper(departement)) %>%
  left_join(df_1998_inscrits, by = c("departement_cap" = "departement")) %>%
  select(-departement_cap)
df_1998 <- df_1998 %>%
  bind_rows(df_1998 %>%
      mutate(percentage = votes/Inscrits) %>%
      group_by(departement) %>%
      summarize(votes = mean(Inscrits * (1 - sum(percentage)))) %>%
      mutate(bloc = "Abstention")
  ) %>%
  group_by(departement) %>%
  mutate(percentage = votes/sum(votes),
         year = 1998) %>%
  rename(department = departement) %>%
  select(year, department, percentage,bloc, votes)
###############################################################################
## 1992
df_1992 <- df_1992 %>%
  select(-`Code departement`, -Votants, -Exprimes) %>%
  pivot_longer(c(-departement, -Inscrits),
               names_to = "list",
               values_to = "votes") %>%
  left_join(list_1992) %>%
  mutate(departement_lower = tolower(departement)) %>%
  select(-departement) %>%
  left_join(
    df_insee_codes %>%
      mutate(departement_lower = tolower(departement))
  ) %>%
  select(-departement_lower)
df_1992 <- df_1992 %>%
  bind_rows(
    df_1992 %>%
      mutate(percentage = votes/Inscrits) %>%
      group_by(departement) %>%
      summarize(votes = mean(Inscrits * (1 - sum(percentage)))) %>%
      mutate(bloc = "Abstention")
  ) %>%
  group_by(departement, bloc) %>%
  summarize(votes = sum(votes)) %>%
  group_by(departement) %>%
  mutate(percentage = votes/sum(votes),
         year = 1992) %>%
  ungroup() %>%
  rename(department = departement) %>%
  select(year, department, percentage,bloc, votes)
###############################################################################
## 1986
df_1986 <- df_1986 %>%
  select(-`Code departement`, -Votants, -Exprimes) %>%
  pivot_longer(c(-departement, -Inscrits),
               names_to = "list",
               values_to = "votes") %>%
  left_join(list_1986) %>%
  mutate(departement_lower = tolower(departement)) %>%
  select(-departement) %>%
  left_join(
    df_insee_codes %>%
      mutate(departement_lower = tolower(departement))
  ) %>%
  select(-departement_lower)
df_1986 <- df_1986 %>%
  bind_rows(
    df_1986 %>%
      mutate(percentage = votes/Inscrits) %>%
      group_by(departement) %>%
      summarize(votes = mean(Inscrits * (1 - sum(percentage)))) %>%
      mutate(bloc = "Abstention")
  ) %>%
  group_by(departement, bloc) %>%
  summarize(votes = sum(votes)) %>%
  group_by(departement) %>%
  mutate(percentage = votes/sum(votes),
         year = 1986) %>%
  ungroup() %>%
  rename(department = departement) %>%
  select(year, department, percentage,bloc, votes)
###############################################################################
## add
df <- bind_rows(
  df_2021,
  df_2015,
  df_2010,
  df_2004,
  df_1998,
  df_1992,
  df_1986
) %>%
  mutate(election_type = "regional") %>%
  mutate(bloc = ifelse(bloc %in% c("Autonisme", "Regionalism"), "Autre",
                ifelse(bloc == "Ecologism", "Ecologisme", bloc)))
write_csv(df, "dta/election_results/election_results_regional.csv")
###############################################################################