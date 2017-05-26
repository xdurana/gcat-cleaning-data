library(tidyverse)

directory <- 'eating'

ds <- gcat %>%
  select(
    entity_id,
    HABITO_CAFE,
    HABITO_TE
  )

ds <- ds %>%
  mutate(
    coffee_intake = ifelse(HABITO_CAFE %in% c(1), 1, ifelse(HABITO_CAFE %in% c(2), 0, NA)),
    tea_intake = ifelse(HABITO_TE %in% c(1), 1, ifelse(HABITO_TE %in% c(2), 0, NA))
  ) %>%
  select(
    entity_id,
    coffee_intake,
    tea_intake
  )

ds %>% write_csv(sprintf('output/check/%s/data.csv', directory))