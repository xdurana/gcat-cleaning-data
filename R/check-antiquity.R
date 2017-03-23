library(dplyr)
library(xlsx)
library(lubridate)
library(data.table)

directory <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'

participants <- fread(file.path(directory, 'Participants/data.csv')) %>%
  select(
    entity_id,
    Admin.Interview.endDate
  ) %>%
  mutate(
    temps_follow_up=as.numeric(interval(as.Date(Admin.Interview.endDate), Sys.Date()), "days")
  ) %>%
  dplyr::rename(
    inici=Admin.Interview.endDate
  )

participants %>%
  select(entity_id, inici, temps_follow_up) %>%
  arrange(desc(temps_follow_up)) %>%
  write.xlsx2('output/check/follow-up/temps_follow_up.xlsx', row.names = FALSE)

genotyped <- fread('output/check/genotyped/data.csv') %>%
  filter(as.numeric(plate) <= 60) %>%
  merge(participants) %>%
  select(entity_id, inici, temps_follow_up) %>%
  arrange(desc(temps_follow_up)) %>%
  write.xlsx2('output/check/follow-up/temps_follow_up_genotyped.xlsx', row.names = FALSE)
