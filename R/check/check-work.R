library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    LABORAL_ESTADO,
    LABORAL_EMPLEO_ACTIVO_ANO,
    LABORAL_1_HORARIO,
    LABORAL_1_ACTIVIDAD_FISICA
  )

ds <- ds %>%
  mutate(
    working_status = ifelse(LABORAL_ESTADO == 0, NA, LABORAL_ESTADO),
    working_status_active = ifelse(LABORAL_ESTADO %in% c(1), 1, ifelse(LABORAL_ESTADO %in% c(2,3,4,5,6,7,8), 0, NA)),
    working_last_year = ifelse(LABORAL_EMPLEO_ACTIVO_ANO %in% c(1), 1, ifelse(LABORAL_EMPLEO_ACTIVO_ANO %in% c(2), 0, NA)),
    working_shift = ifelse(LABORAL_1_HORARIO %in% c(1,2,3), 1, ifelse(LABORAL_1_HORARIO %in% c(4), 0, NA))
  ) %>%
  select(
    entity_id,
    working_status,
    working_status_active,
    working_last_year,
    working_shift
  )

ds %>% write_csv('output/check/work/data.csv')