library(tidyverse)

ds <- participants %>%
  select(
    entity_id,
    Admin.Interview.status
  )

ds <- ds %>%
  rename(
    status=Admin.Interview.status
  ) %>%
  select(
    entity_id,
    status
  )

ds %>% write_csv('output/check/participants/data.csv')