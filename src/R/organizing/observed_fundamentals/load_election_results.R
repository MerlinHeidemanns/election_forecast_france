###############################################################################
## Title: Load election results
###############################################################################
## Clean environment
rm(list = ls())
###############################################################################
## Load packages
library(tidyverse)
library(rvest)
###############################################################################
## Load data
df_insee_codes <- read_csv("dta/france_classification/departements.csv")
#https://www.data.gouv.fr/fr/datasets/election-presidentielle-des-23-avril-et-7-mai-2017-resultats-definitifs-du-1er-tour-1/
df_2017 <- read_csv("dta/election_results/department_2017.csv")
###############################################################################
## Functions
#' Overview sites
return_overview_sites <- function(){
  years <- c(1965, 1969, 1974, 1981, 1988, 1995, 2002, 2007, 2012)
  links <- sapply(years, function(x){
    return(paste("http://www.politiquemania.com/presidentielles-",
                  x, "-departement.html", sep = ""))
  })
  return(
    data.frame(
      years = years,
      links = links
    )
  )
}
#' Return department pages
return_sites <- function(site){
  tables <- read_html(site) %>%
    html_node("body") %>%
    xml2::xml_find_all("//a[contains(@class, 'lien2')]") %>%
    html_attr('href')
  links <- sapply(tables, function(x){
      return(paste("http://www.politiquemania.com/", x, sep = ""))
    })
  links <- links[grepl("departement", links)]
    return(links)
}
#' Return results
return_results <- function(site){

  ## Extract tables
  tables <- read_html(site) %>%
    html_node("body") %>%
    xml2::xml_find_all("//table[contains(@class, 'resultats')]") %>%
    html_table()

  ## Splitting tables
  first_table_start <- match("Inscrits", tables[[1]]$X1)
  second_table_start <- match("Nom", tables[[1]]$X1)
  ## Departement
  location <- tables[[1]][1,1] %>% pull(X1)
  ## Totals
  totals_table <- tables[[1]][first_table_start:second_table_start - 1,] %>%
    filter(X1 != "")
  colnames(totals_table) <- totals_table[1, ]
  totals_table <- totals_table[2:nrow(totals_table),]
  totals_table <- totals_table %>%
    pivot_longer(everything(),
                names_to = "categorie",
                 values_to = "val") %>%
    mutate(kind = ifelse(grepl("\\%", val), "percentage", "votes")) %>%
    pivot_wider(
      id_cols = categorie,
      names_from = kind,
      values_from = val
    ) %>%
    mutate(percentage = str_remove_all(percentage, "\\%"),
           percentage = str_replace_all(percentage, "\\,", "."),
           percentage = as.numeric(percentage),
           votes = str_replace_all(votes, "\\s", ""),
           votes = as.integer(votes),
           departement = location)
  ## Candidates
  candidates_table  <- tables[[1]][second_table_start:nrow(tables[[1]]),]
  colnames(candidates_table) <- candidates_table[1,]
  candidates_table <- candidates_table[2:nrow(candidates_table), ]
  candidates_table <- candidates_table[, !duplicated(colnames(candidates_table))]
  candidates_table <- candidates_table %>%
    rename(
      candidate = Nom,
      votes = Voix,
      percentage = `%`
    ) %>%
    select(candidate, votes, percentage) %>%
    mutate(percentage = str_remove_all(percentage, "\\%"),
           percentage = str_replace_all(percentage, "\\,", "."),
           percentage = as.numeric(percentage),
           votes = str_replace_all(votes, "\\s", ""),
           votes = as.integer(votes),
           departement = location)

  return(
    list(
      candidates_table = candidates_table,
      totals_table = totals_table
    )
  )
}
###############################################################################
## Load data
overview_sites <- return_overview_sites()
candidate_tables <-  vector("list", 1e4)
total_tables <- vector("list", 1e4)
count <- 0
for (i in overview_sites$years){
  print(i)
  year <- i
  site <- overview_sites %>%
    filter(years == year) %>%
    pull(links)
  sites_departments <- return_sites(site)
  for (j in sites_departments){
    print(count)
    count <- count + 1
    tables <- return_results(j)
    candidate_tables[[count]] <- tables$candidates_table %>%
      mutate(year = i)
    total_tables[[count]] <- tables$totals_table %>%
      mutate(year = i)
  }
}
## Bind
total_tables_df <- do.call("bind_rows", total_tables)
candidate_tables_df <- do.call("bind_rows", candidate_tables)
## Add insee_codes
total_tables_df <- total_tables_df %>%
  mutate(
    departement = str_replace_all(departement, "\\-", " "),
    departement = ifelse(departement == "Côtes du Nord",
                         "Côtes d'Armor",
                         departement)
  ) %>%
  left_join(df_insee_codes) %>%
  filter(!is.na(insee_code)) %>%
  mutate(insee_code = as.character(insee_code))
candidate_tables_df <- candidate_tables_df %>%
  mutate(
    departement = str_replace_all(departement, "\\-", " "),
    departement = ifelse(departement == "Côtes du Nord",
                         "Côtes d'Armor",
                         departement)
  ) %>%
  left_join(df_insee_codes) %>%
  filter(!is.na(insee_code)) %>%
  mutate(insee_code = as.character(insee_code))
###############################################################################
## Add 2017
total_tables_df <- read_csv("dta/election_results/department_total_votes.csv") %>%
  filter(year != 2017)
df_2017 <- read_csv("dta/election_results/department_2017.csv")

department_vector <- total_tables_df %>%
  distinct(departement) %>%
  pull(departement)
df_2017_totals <- df_2017 %>%
  mutate(`Blancs ou nuls` = Blancs + Nuls,
         ) %>%
  select(!contains(".exp") & !contains(".ins") & !contains("_ins") & !contains("_vot"), -Blancs, -Nuls) %>%
  rename(insee_code = `CodeDépartement`,
         departement = `Département`) %>%
  pivot_longer(
    c(-insee_code, -departement),
    names_to = "categorie",
    values_to = "votes"
  ) %>%
  filter(categorie %in% c(total_tables_df$categorie %>% unique())) %>%
  mutate(group = ifelse(categorie %in%
                          c("Abstentions", "Votants"), 1,
                 ifelse(categorie %in%
                          c("Blancs ou nuls", "Exprimés"),2, 3)),
         insee_code = as.character(insee_code)) %>%
  group_by(group, departement) %>%
  mutate(percentage = 100 * votes/sum(votes),
         year = 2017,
         insee_code = as.numeric(insee_code),
         departement = str_replace_all(departement, "\\-", " ")) %>%
  ungroup() %>%
  select(-group) %>%
  mutate(departement = str_replace_all(departement, "\\-", " ")) %>%
  filter(departement %in% department_vector)
## Add
total_tables_df <- total_tables_df %>%
  bind_rows(df_2017_totals)
## Candidates
candidate_tables_df <- read_csv("dta/election_results/department_candidate_votes.csv") %>%
  filter(year != 2017) %>%
  mutate(insee_code = as.character(insee_code))
df_2017 <- read_csv("dta/election_results/department_2017.csv")

department_vector <- total_tables_df %>%
  distinct(departement) %>%
  pull(departement)
candidate_names <- data.frame(
  names_capitalized = c("LE PEN",
                        "MACRON",
                        "MÉLENCHON",
                        "FILLON",
                        "HAMON",
                        'DUPONT-AIGNAN',
                        "LASSALLE",
                        "POUTOU",
                        "ASSELINEAU",
                        "ARTHAUD",
                        "CHEMINADE"),
  candidate = c("Marine Le Pen",
                "Emmanuel Macron",
                "Jean-Luc Mélenchon",
                "Francois Fillon",
                "Benoit Hamon",
                'Nicolas Dupont-Aignan',
                "Ferdinand Lassalle",
                'Philippe Poutou',
                "Francois Asselineau",
                "Nathalie Arthaud",
                "Jacques Cheminade")
)
df_2017_candidates <- df_2017 %>%
  select(`Département`, `CodeDépartement`,
          `LE PEN`, `MACRON`,
         `MÉLENCHON`,
         `FILLON`,
         `HAMON`,
         `DUPONT-AIGNAN`,
         `LASSALLE`,
         `POUTOU`,
         `ASSELINEAU`,
         `ARTHAUD`,
         `CHEMINADE`,
         ) %>%
  rename(insee_code = `CodeDépartement`,
         departement = `Département`) %>%
  pivot_longer(
    c(-insee_code, -departement),
    names_to = "names_capitalized",
    values_to = "votes"
  ) %>%
  left_join(
    candidate_names
  ) %>%
  group_by(departement) %>%
  mutate(percentage = 100 * votes/sum(votes),
         year = 2017,
         insee_code = as.character(insee_code)) %>%
  ungroup() %>%
  select(-names_capitalized) %>%
  mutate(departement = str_replace_all(departement, "\\-", " ")) %>%
  filter(departement %in% department_vector)

candidate_tables_df <- bind_rows(
  candidate_tables_df, df_2017_candidates
)
## Save
write_csv(total_tables_df, "dta/election_results/department_total_votes.csv")
write_csv(candidate_tables_df, "dta/election_results/department_candidate_votes.csv")
###############################################################################