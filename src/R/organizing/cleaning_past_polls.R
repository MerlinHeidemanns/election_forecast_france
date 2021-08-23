## Clean
rm(list = ls())
## Libraries
library(tidyverse)
## Load
load_full_data <- FALSE
if (load_full_data) {
  df <- read_delim("dta/polls_wlezien_and_jennings",
                   delim = "\t")
  df <- df %>%
    filter(country == "France")
  write.csv(df, "dta/polls_wlezien_and_jennings_france.csv")
} else {
  df <- read_csv("dta/polls_wlezien_and_jennings_france.csv")
}
## Subset
df %>%
  filter(country == "France")