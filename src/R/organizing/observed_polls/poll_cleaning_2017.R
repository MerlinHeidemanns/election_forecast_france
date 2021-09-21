# --- Libraries
library(tidyverse)
# --- Adjust
# * Support vector and files
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
path <- "dta/polls_dta/polls_2017"
files <- list.files(path)
# * Merge files
table <- lapply(1:length(files), function(j){
  table <- read_csv(paste(path, files[j], sep = "/"))
  table %>%
    mutate(poll_id = 1:n() + j * 1000) %>%
    pivot_longer(c(-poll_id, -PollingFirm, -FieldworkDate, -SampleSize),
                 names_to = "Candidate",
                 values_to = "Percentage") %>%
    mutate(Percentage = str_remove(Percentage, "[%]"),
           Percentage = ifelse(Percentage == "<0.5", "0", Percentage),
           Percentage = ifelse(Percentage == "–", NA, Percentage),
           Percentage = as.numeric(as.character(Percentage))) %>%
    filter(!is.na(Percentage)) %>%
    return()
}) %>%
  do.call("bind_rows", .)
# * Adjust dates
table <- table %>%
  mutate(year = as.integer(str_match(FieldworkDate, "[\\d]+$")[,1]),
         year = ifelse(year < 1000, year + 2000, year),
         start_month = match(str_match(FieldworkDate, "[A-z]+")[,1], months) ,
         end_month = match(str_match(FieldworkDate, "([A-z]+)[\\-\\s0-9]+$")[,2], months),
         start_day = str_match(FieldworkDate, "^[\\d]+")[,1],
         end_day = str_match(FieldworkDate, paste0("([\\d]+)[^\\d+]+",year, "$"))[,2]
  ) %>%
  mutate(start_day = as.Date(paste(year, start_month, start_day, sep = "_"),
                             "%Y_%m_%d"),
         end_day = as.Date(paste(year, end_month, end_day, sep = "_"),
                             "%Y_%m_%d"),
         Candidate = ifelse(Candidate == "Abs.", "ZZZ_Abstention", Candidate)) %>%
  dplyr::select(-FieldworkDate, -end_month, -start_month, -year)
# * Save
write.csv(table, "dta/polls_dta/2017_polls_compiled.csv")

