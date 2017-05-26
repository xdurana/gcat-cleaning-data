library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    LATERALIDAD
  ) %>%
  mutate(
    handedness=ifelse(LATERALIDAD == 0, NA, LATERALIDAD)
  ) %>%
  mutate(
    handedness_left=ifelse(handedness %in% c(1), 1, ifelse(handedness %in% c(2,3), 0, NA))
  ) %>%
  select(
    entity_id,
    handedness,
    handedness_left
  )

ds %>% write_csv('output/check/handedness/data.csv')
