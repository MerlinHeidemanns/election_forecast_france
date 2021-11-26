###############################################################################
## Title: Load election results legislature
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
###############################################################################
## Functions
#' Overview sites
return_overview_sites <- function(){
  years <- c(1993, 1997, 2002, 2007, 2012)
  links <- sapply(years, function(x){
    return(paste("http://www.politiquemania.com/legislatives-",
                 x, "-circonscription.html", sep = ""))
  })
  return(
    data.frame(
      years = years,
      links = links
    )
  )
}
return_sites <- function(site){
  tables <- read_html(site) %>%
    html_node("body") %>%
    xml2::xml_find_all("//a[contains(@class, 'politiquemania')]") %>%
    html_attr('href')
  links <- sapply(tables, function(x){
    return(paste("http://www.politiquemania.com/", x, sep = ""))
  })
  links <- links[grepl("circonscription", links)]
  return(links)
}
return_circonscription <- function(site){
  tables <- read_html(site) %>%
    html_node("body") %>%
    xml2::xml_find_all("//a[contains(@class, 'politiquemania')]") %>%
    html_attr('href')
  links <- sapply(tables, function(x){
    return(paste("http://www.politiquemania.com/", x, sep = ""))
  })
  links <- links[grepl("circonscription", links)]
  return(links)
}
site <- return_circonscription(return_sites(return_overview_sites()$links[1])[1])[1]
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
  departement <- str_match(location, "l\\'(.+)")[, 2]
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
           circonscription = location,
           departement = departement)
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
           circonscription = location,
           departement = departement)

  ## Find parties
  candidates <- read_html(site) %>%
    html_node("body") %>%
    xml2::xml_find_all("//table[contains(@class, 'resultats')]")%>%
    `[[`(1) %>%
    xml2::xml_find_all("//td[contains(@class, 'resultats_presidentielle1a')]") %>%
    html_text()

  parties <- read_html(site) %>%
    html_node("body") %>%
    xml2::xml_find_all("//table[contains(@class, 'resultats')]")%>%
    `[[`(1) %>%
    xml2::xml_find_all("//td[contains(@class, 'resultats_presidentielle2a')]") %>%
    html_text() %>%
    c() %>%
    str_replace_all(., "\\\t|\\\n", "")
  labels <- read_html(site) %>%
    html_node("body") %>%
    xml2::xml_find_all("//table[contains(@class, 'resultats')]")%>%
    `[[`(1) %>%
    xml2::xml_find_all("//td[contains(@class, 'resultats_presidentielle2a')]") %>%
    xml2::xml_children() %>%
    html_attr("title")

  out <- rep(NA, length(parties))
  count = 0
  for (j in 1:length(parties)){
    if (parties[j] == ""){
      count = count + 1
      out[j] = labels[count]
    } else {
      out[j] = parties[j]
    }
  }

  candidates_parties <- data.frame(party = out, candidate = candidates)

  candidates_table <- candidates_table %>%
    left_join(candidates_parties) %>%
    distinct(candidate, votes, percentage, .keep_all = TRUE)

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
candidate_tables <- vector("list", 1e4)
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
    circonscriptions <- return_circonscription(j)
    for (r in circonscriptions){
      print(count)
      count <- count + 1
      tables <- return_results(r)
      candidate_tables[[count]] <- tables$candidates_table %>%
        mutate(year = i)
      total_tables[[count]] <- tables$totals_table %>%
        mutate(year = i)
    }
  }
}
## Bind
total_tables_df <- do.call("bind_rows", total_tables)
candidate_tables_df <- do.call("bind_rows", candidate_tables)
## Add insee_codes
total_tables_df <- total_tables_df %>%
  mutate(

    departement = str_match(circonscription,
            "(du\\s|de\\sla\\s|des\\s|de\\sl\\'|de\\s)(.+)")[,3],
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

    departement = str_match(circonscription,
                            "(du\\s|de\\sla\\s|des\\s|de\\sl\\'|de\\s)(.+)")[,3],
    departement = str_replace_all(departement, "\\-", " "),
    departement = ifelse(departement == "Côtes du Nord",
                         "Côtes d'Armor",
                         departement)
  ) %>%
  left_join(df_insee_codes) %>%
  filter(!is.na(insee_code)) %>%
  mutate(insee_code = as.character(insee_code))
## Save
write_csv(total_tables_df, "dta/election_results/legislature_department_total_votes.csv")
write_csv(candidate_tables_df, "dta/election_results/legislature_department_candidate_votes.csv")
###############################################################################