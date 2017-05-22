library(tidyverse)

whr <- function(waist, hip) {
  round(waist/hip, 2)
}

export_dir <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'

cintura_cadera <- read_csv(file.path(export_dir, 'CinturaCadera/data.csv')) %>%
  select(
    entity_id,
    `1_Cintura`,
    `2_Cintura`,
    `1_Cadera`,
    `2_Cadera`
  ) %>%
  mutate(
    diff_cintura=abs(`1_Cintura`-`2_Cintura`),
    diff_cadera=abs(`1_Cadera`-`2_Cadera`)
  ) %>%
  mutate(
    waist=ifelse(diff_cintura > 1, NA, round((`1_Cintura` + `2_Cintura`)/2, digits=2)),
    hip=ifelse(diff_cadera > 1, NA, round((`1_Cadera` + `2_Cadera`)/2, digits=2)),
    whr=whr(waist, hip)
  )

cintura_cadera <- cintura_cadera %>%
  select(
    entity_id,
    waist,
    hip,
    whr
  )
  
cintura_cadera %>% write_csv('output/check/whr/data.csv')