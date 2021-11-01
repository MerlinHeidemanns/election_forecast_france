################################################################################
## Title: Look at data
################################################################################
## Empty environment
rm(list = ls())
################################################################################
## Load libraries
library(tidyverse)
################################################################################
## Load data
df_current <-
  read_csv("dta/polls_dta/polls_2020/polls_position_polls_2022_raw.csv")
################################################################################
## Plot likely voters
plt_df <- df_current %>%
  select(date_fin, institute, interroges, intentions_exprimees) %>%
  distinct(date_fin, institute, .keep_all = TRUE) %>%
  mutate(share_reported = intentions_exprimees/interroges) %>%
  filter(share_reported > 0)
ggplot(plt_df, aes(x = date_fin, y = share_reported, color = institute)) +
  geom_point() +
  geom_smooth(method = "lm")

plt_df <- df_current %>%
  select(date_fin, institute, interroges, intentions_exprimees, nspp, base) %>%
  distinct(date_fin, institute, nspp, .keep_all = TRUE) %>%
  mutate(share_reported = intentions_exprimees/interroges,
         nspp = nspp / 100,
         share_reported_plus_nspp = round(share_reported + nspp, 2)) %>%
  filter(share_reported > 0) %>%
  pivot_longer(cols = c(share_reported, nspp, share_reported_plus_nspp),
               names_to = "type",
               values_to = "share") %>%
  group_by(institute, base, type) %>%
  summarize(
    q50 = quantile(share, 0.5),
    q25 = quantile(share, 0.25),
    q75 = quantile(share, 0.75),
    q10 = quantile(share, 0.10),
    q90 = quantile(share, 0.90)
  )

ggplot(plt_df, aes(x = interaction(institute, base), y = q50, color = type)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = q25, ymax = q75, color = type),
                position = position_dodge(width = 0.3))

################################################################################
## two way matchups
#' Macron vs Le Pen
plt_df <- df_current %>%
  mutate(z = ifelse(candidates %in% c("Emmanuel Macron", "Marine Le Pen"),
                    0, 1)) %>%
  group_by(question_id, survey_id, date_fin, institute) %>%
  mutate(z = sum(z)) %>%
  filter(z == 0) %>%
  filter(candidates == "Emmanuel Macron")
ggplot(plt_df, aes(x = date_fin, y = percentages, color = institute)) +
  geom_point() +
  geom_smooth()
################################################################################
## certitude
#' All candidates
ggplot(df_current, aes(x = date_fin, y = certitude)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(candidates ~ .)
#' certitude against polling number
plt_df <- df_current %>%
  group_by(question_id, survey_id, institute, date_fin) %>%
  filter(n() > 3) %>%
  ungroup() %>%
  select(candidates, date_fin, certitude, percentages) %>%
  pivot_longer(cols = c(certitude, percentages),
               names_to = "type",
               values_to = "val")
ggplot(plt_df, aes(x = date_fin, y = val, color = type)) +
  geom_point() +
  facet_wrap(candidates ~ .)
################################################################################
## Florian Philippot polling numbers
#'
plt_df <- df_current %>%
  filter(candidates == "Florian Philippot")
ggplot(plt_df, aes(x = date_fin, y = percentages)) +
  geom_point()


