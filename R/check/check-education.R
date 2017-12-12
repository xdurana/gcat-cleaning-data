library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    ESTUDIOS,
    SANIDAD,
    INGRESOS
  ) %>%
  mutate(
    education = ifelse(ESTUDIOS == 0, NA, ESTUDIOS),
    education_college = ifelse(ESTUDIOS %in% c(7), 1, ifelse(ESTUDIOS %in% c(1,2,3,4,5,6), 0, NA)),
    income = ifelse(INGRESOS == 0, NA, INGRESOS),
    health_service_usage = ifelse(SANIDAD == 0, NA, SANIDAD)
  ) %>%
  select(
    entity_id,
    education,
    education_college,
    income,
    health_service_usage
  )

ds %>% write_csv('output/check/education/data.csv')
