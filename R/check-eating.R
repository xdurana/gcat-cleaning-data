library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    HABITO_CAFE,
    HABITO_TE
  )

ds <- ds %>%
  mutate(
    caffeine_use = ifelse(HABITO_CAFE %in% c(1), 1, ifelse(HABITO_CAFE %in% c(2), 0, NA)),
    theine_use = ifelse(HABITO_TE %in% c(1), 1, ifelse(HABITO_TE %in% c(2), 0, NA))
  ) %>%
  select(
    entity_id,
    caffeine_use,
    theine_use
  )

ds %>% write_csv('output/check/eating/data.csv')