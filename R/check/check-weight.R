library(tidyverse)

export_dir <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'

weight <- read_csv(file.path(export_dir, 'Peso/data.csv')) %>%
  select(
    entity_id,
    `1_Peso`,
    `2_Peso`
  ) %>%
  mutate(
    diff=abs(`1_Peso`-`2_Peso`)
  ) %>%
  mutate(
    weight=ifelse(diff > 1, NA, round((`1_Peso` + `2_Peso`)/2, digits=2))
  )

weight %>%
  select(
    entity_id,
    weight
  ) %>%
  write_csv('output/check/weight/data.csv')
  