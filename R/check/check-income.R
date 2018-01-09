library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    INGRESOS
  ) %>%
  mutate(
    income = ifelse(INGRESOS == 0, NA, INGRESOS)
  ) %>%
  select(
    entity_id,
    income
  )

ds %>% write_csv('output/check/income/data.csv')
