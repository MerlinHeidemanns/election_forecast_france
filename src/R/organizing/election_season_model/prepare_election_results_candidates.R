###############################################################################
## Title: Prepare election results by candidates at department level
###############################################################################
## Description
#' Load tidyverse
#' bloc vector for bloc factor
#' Load data
#' Clean candidate names for special characters and -
#' join bloc data
#' remove NAs
#' Summarize by bloc
#' Plot by department and bloc over time
#' Plot correlation matrix
#' turn bloc into factor
#' Save
###############################################################################
## Libraries
library(tidyverse)
###############################################################################
## Vectors
bloc_vector <- c("Abstention",
                 "Gauche radicale et extreme gauche",
                 "Gauche",
                 "Ecologisme",
                 "Centre",
                 "Droite",
                 "Droite radicale et extreme droite")
###############################################################################
## Load data
df <- read_csv("dta/election_results/department_candidate_votes.csv")
bloc_crosswalk <- read_csv("dta/france_classification/candidate_bloc_cross_walk.csv")
###############################################################################
## Candidates
df <- df %>%
  rename(long_name = candidate) %>%
  mutate(long_name = str_replace_all(long_name, "\\-", " "),
         long_name = stringi::stri_trans_general(long_name, "Latin-ASCII")
  ) %>%
  left_join(bloc_crosswalk,
            by = c("long_name" = "long_name",
                   "year" = "election_year")) %>%
  filter(!is.na(bloc)) %>%
  #' Aggregate by bloc
  group_by(year, bloc, departement) %>%
  summarize(percentage = sum(percentage),
            votes = sum(votes)) %>%
  ungroup()
#' Plot
ggplot(data = df) +
  geom_line(aes(x = year, y = percentage, group = departement)) +
  geom_smooth(data = df, aes(x = year, y = percentage)) +
  facet_wrap(bloc~.)
#' Correlations between vote shares
cor_df <- df %>%
  pivot_wider(
    id_cols = c(year, departement),
    names_from = bloc,
    values_from = percentage
  ) %>%
  select(-year, - departement) %>%
  cor(., use = "complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("bloc1") %>%
  pivot_longer(c(-bloc1),
               names_to = "bloc2",
               values_to = "value") %>%
  mutate(bloc1 = factor(bloc1, levels = bloc_vector),
         bloc2 = factor(bloc2, levels = bloc_vector)) %>%
  arrange(bloc1, bloc2)
ggplot(data = cor_df, aes(x=bloc1, y=bloc2, fill=value)) +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 10, hjust = 1),
        axis.title = element_blank())+
  coord_fixed()
#' Prepare for output
df <- df %>%
  mutate(bloc = factor(bloc, levels = bloc_vector),
         percentage = percentage/100)
#' Write
write_csv(df, file = "dta/election_results/department_candidate_votes_blocs_clean.csv")
###############################################################################