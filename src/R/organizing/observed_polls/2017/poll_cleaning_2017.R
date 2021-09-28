# --- Libraries
library(tidyverse)
# --- Adjust
# * Support vector and files
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
path <- "dta/polls_dta/polls_2017"
files <- list.files(path)
# * Merge files
table <- lapply(1:length(files), function(j){
  table <- read.csv(paste(path, files[j], sep = "/"), encoding = "UTF-8")
  table %>%
    mutate(poll_id = paste0(2017, "_", j, "", 1:n()),
           SampleSize = as.integer(str_replace_all(SampleSize, "[^0-9]", ""))) %>%
    rename(pollName = PollingFirm,
           dates = FieldworkDate,
           sampleSize = SampleSize) %>%
    pivot_longer(c(-poll_id, -pollName, -dates, -sampleSize),
                 names_to = "candidate",
                 values_to = "percentage") %>%
    mutate(percentage = str_remove(percentage, "[%]"),
           percentage = ifelse(percentage == "<0.5", "0", percentage),
           percentage = ifelse(percentage == "–", NA, percentage),
           percentage = as.numeric(as.character(percentage))) %>%
    filter(!is.na(percentage)) %>%
    return()
}) %>%
  do.call("bind_rows", .)
# * Adjust dates
table <- table %>%
  mutate(year = as.integer(str_match(dates, "[\\d]+$")[,1]),
         year = ifelse(year < 1000, year + 2000, year),
         start_month = match(str_match(dates, "[A-z]+")[,1], months) ,
         end_month = match(str_match(dates, "([A-z]+)[\\-\\s0-9]+$")[,2], months),
         start_day = str_match(dates, "^[\\d]+")[,1],
         end_day = str_match(dates, paste0("([\\d]+)[^\\d+]+",year, "$"))[,2]
  ) %>%
  mutate(start_day = as.Date(paste(year, start_month, start_day, sep = "_"),
                             "%Y_%m_%d"),
         end_day = as.Date(paste(year, end_month, end_day, sep = "_"),
                           "%Y_%m_%d"),
         candidate = ifelse(candidate == "Abs.", "_Abstention", candidate),
         candidate = str_replace(candidate, "\\.", " ")) %>%
  dplyr::select(-dates, -end_month, -start_month, -year) %>%
  mutate(candidate = stringi::stri_trans_general(candidate, "Latin-ASCII"))

# * Save
write.csv(table, "dta/polls_dta/2017_polls_compiled.csv")

