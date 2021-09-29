## Code to compile unemployment data
rm(list = ls())
## Libraries
library(tidyverse)
## Load raw data and clean
df <- read.csv("dta/fundamentals_dta/unemployment.csv") %>%
  mutate(quarter = str_match(year, "T([1-4])")[,2],
         year = str_match(year, "([\\d]+)\\-")[,2],
         date = as.Date(paste(year,
                              as.character((as.integer(quarter) * 3) - 2),
                              "01",
                              sep = "-"),
                        "%Y-%m-%d"),
         unemployment = as.numeric(unemployment),
         year = as.integer(year),
         quarter = as.integer(quarter)) %>%
  dplyr::select(date, unemployment, year, quarter)
## Save cleaned data
write.csv(df, "dta/fundamentals_dta/unemployment_clean.csv")