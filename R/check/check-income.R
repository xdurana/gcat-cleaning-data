library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    INGRESOS
  ) %>%
  mutate(
    income = ifelse(ESTUDIOS == 0, NA, ESTUDIOS)
  ) %>%
  select(
    entity_id,
    income
  )

ds %>% write_csv('output/check/income/data.csv')
