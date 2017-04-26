library(readr)
library(dplyr)

export_dir <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'

participants <- read_csv(file.path(export_dir, 'Participants/data.csv'))
gcat <- read_csv(file.path(export_dir, 'QUESTIONARI/data.csv'))

genotyped <- read_csv('output/check/genotyped/data.csv')
bmi <- read_csv('output/check/bmi/data.csv')
bp <- read_csv('output/check/bp/data.csv')
whr <- read_csv('output/check/whr/data.csv')

genotyped.data <- genotyped %>%
  select(
    entity_id,
    plate,
    sampleType
  ) %>%
  left_join(
    participants %>%
      select(
        entity_id,
        Admin.Interview.status,
        Admin.Participant.gender,
        Admin.Participant.age
      )
  ) %>%
  left_join(
    bmi %>%
      select(
        entity_id,
        BMI
      )
  ) %>%
  left_join(
    whr %>%
      select(
        entity_id,
        WHR
      )
  ) %>%
  left_join(
    bp
  )

ds <- genotyped.data %>%
  left_join(
    gcat %>%
      select(
        entity_id,
        SEXO,
        EDAD_ANOS,
        ETNIA_PARTICIPANTE,
        LATERALIDAD,
        ESTADO_CIVIL,
        CONVIVENCIA_PAREJA,
        ESTUDIOS,
        INGRESOS,
        SANIDAD,
        LABORAL_ESTADO
      )
  )
