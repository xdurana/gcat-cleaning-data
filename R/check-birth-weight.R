library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    ESTADO_PESO_NACIMIENTO
  ) %>%
  mutate(
    birth_weight = ifelse(ESTADO_PESO_NACIMIENTO == 0, NA, ESTADO_PESO_NACIMIENTO),
    birth_weight_binary = ifelse(ESTADO_PESO_NACIMIENTO %in% c(1,2,3), 0, ifelse(ESTADO_PESO_NACIMIENTO %in% c(4,5), 1, NA))
  ) %>%
  select(
    entity_id,
    birth_weight,
    birth_weight_binary
  )

ds %>% write_csv('output/check/birth_weight/data.csv')