library(tidyverse)

directory <- 'alcohol'

ds <- gcat %>%
  select(
    entity_id,
    ALCOHOL_ACTUAL
  )

ds <- ds %>%
  mutate(
    alcohol_actual = ifelse(ALCOHOL_ACTUAL %in% c(1), 0, ifelse(ALCOHOL_ACTUAL %in% c(2,3,4,5,6,7,8), 1, NA))
  ) %>%
  select(
    entity_id,
    alcohol_actual
  )

ds %>% write_csv(sprintf('output/check/%s/data.csv', directory))