library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    ESTADO_DE_SALUD
  ) %>%
  mutate(
    self_perceived_health = ifelse(ESTADO_DE_SALUD == 0, NA, ESTADO_DE_SALUD),
    self_perceived_health_binary = ifelse(ESTADO_DE_SALUD %in% c(1,2), 0, ifelse(ESTADO_DE_SALUD %in% c(3,4,5), 1, NA))
  ) %>%
  select(
    entity_id,
    self_perceived_health,
    self_perceived_health_binary
  )

ds %>% write_csv('output/check/self_perceived_health/data.csv')