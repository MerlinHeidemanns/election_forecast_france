###############################################################################
## Title: Create an incumbency file
###############################################################################
## Libraries
library(tidyverse)
###############################################################################
## Create dataframe
df <- data.frame(
  election_year =
    c(1969, 1974, 1981, 1988, 1995, 2002, 2007, 2012, 2017, 2022),
  incumbent_bloc =
    c("Droite", "Droite", "Centre", "Gauche", "Gauche",
      "Droite", "Droite", "Droite", "Gauche", "Centre"),
  reelection = c(1, 0, 0, 1, 0, 1, 0, 1, 0, 1)
)
## Write
write_csv(df, file = "dta/fundamentals_dta/cleaned_input/incumbency.csv")
###############################################################################