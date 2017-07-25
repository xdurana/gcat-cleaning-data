library(tidyverse)

tension_arterial <- read_csv(file.path(export_dir, 'TensionArterial/data.csv')) %>%
  select(
    entity_id,
    `1_TAS`,
    `2_TAS`,
    `3_TAS`,
    `1_TAD`,
    `2_TAD`,
    `3_TAD`,
    `1_PULS`,
    `2_PULS`,
    `3_PULS`
  )

tension_arterial <- tension_arterial %>%
  mutate(
    systolic_blood_pressure = round((`1_TAS` + `2_TAS` + `3_TAS`)/3, digits=2),
    diastolic_blood_pressure = round((`1_TAD` + `2_TAD` + `3_TAD`)/3, digits=2),
    heart_rate = round((`1_PULS` + `2_PULS` + `3_PULS`)/3, digits=2)
  ) %>%
  select(
    entity_id,
    systolic_blood_pressure,
    diastolic_blood_pressure,
    heart_rate
  ) %>%
  mutate(
    systolic_blood_pressure_c = NA,
    diastolic_blood_pressure_c = NA,
    heart_rate_c = NA
  )

tension_arterial %>% write_csv('output/check/hr/data.csv')
