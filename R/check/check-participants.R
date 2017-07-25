library(tidyverse)

ds <- participants %>%
  select(
    entity_id,
    Admin.Interview.status,
    Admin.Interview.startDate
  )

ds <- ds %>%
  dplyr::rename(
    status=Admin.Interview.status
  ) %>%
  mutate(
    year_of_recruitment = substring(Admin.Interview.startDate, 1, 4)
  ) %>%
  select(
    entity_id,
    status,
    year_of_recruitment
  )

ds %>% write_csv('output/check/participants/data.csv')
