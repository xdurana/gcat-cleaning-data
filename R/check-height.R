library(tidyverse)

export_dir <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'

height <- read_csv(file.path(export_dir, 'Altura/data.csv')) %>%
  select(
    entity_id,
    `1_ALTURA`,
    `2_ALTURA`
  ) %>%
  mutate(
    diff=abs(`1_ALTURA`-`2_ALTURA`)
  ) %>%
  mutate(
    height=ifelse(diff > 1, NA, round((`1_ALTURA` + `2_ALTURA`)/2, digits=2))
  )

height %>%
  select(
    entity_id,
    height
  ) %>%
  write_csv('output/check/height/data.csv')
