library(tidyverse)

directory <- 'ethnic-group'

ds <- gcat %>%
  select(
    entity_id,
    ETNIA_PARTICIPANTE,
    ETNIA_PADRE,
    ETNIA_MADRE
  )

ds <- ds %>%
  mutate(
    ethnic_group = ETNIA_PARTICIPANTE,
    ethnic_group_father = ETNIA_PADRE,
    ethnic_group_mother = ETNIA_MADRE
  ) %>%
  select(
    entity_id,
    ethnic_group,
    ethnic_group_father,
    ethnic_group_mother
  )

ds %>% write_csv(sprintf('output/check/%s/data.csv', directory))