###############################################################################
## Title: Load unemployment data from INSEE and clean
###############################################################################
## clean environment
rm(list = ls())
###############################################################################
## Load libraries
library(tidyverse)
###############################################################################
## Load dta
# https://www.insee.fr/fr/statistiques/series/102760732
df <- read_delim("dta/fundamentals_dta/unemployment/insee_2022_01_08_unemployment.csv",
               delim = ";")
df_insee_codes <- read_csv("dta/france_classification/departements.csv")
###############################################################################
## Last update
# last_updated <- df %>%
#   distinct(`Dernière mise à jour`) %>%
#   filter(!is.na(`Dernière mise à jour`)) %>%
#   pull(`Dernière mise à jour`)
###############################################################################
## Clean data
df <- df %>%
  # select(-`Dernière mise à jour`,
  #        -Période,
  #        -idBank) %>%
  filter(Libellé != "Codes") %>%
  pivot_longer(c(-Libellé),
               names_to = c("Year", "Quarter"),
               values_to = "rate",
               names_pattern = "([0-9]+)-T([1-4])") %>%
  mutate(departement = str_match(Libellé, "-\\s(.+)")[,2],
         departement = str_replace_all(departement, "\\-", " ")) %>%
  select(-Libellé) %>%
  left_join(
    df_insee_codes
  ) %>%
  filter(!is.na(insee_code))
## Save data
write_csv(df, file = "dta/fundamentals_dta/cleaned_input/departmental_unemployment.csv")
###############################################################################