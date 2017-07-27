library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    CANCER1_TIPO,
    CANCER2_TIPO,
    CANCER3_TIPO
  ) %>%
  gather(
    key = N,
    value = CANCER,
    CANCER1_TIPO, CANCER2_TIPO, CANCER3_TIPO
  ) %>%
  filter(
    !is.na(CANCER)
  ) %>%
  select(
    -N
  ) %>%
  unique() %>%
  mutate(
    value = 1,
    CANCER = sprintf('cancer_%s', substring(CANCER, 1, 3))
  ) %>%
  spread(
    CANCER,
    value
  )

ds[is.na(ds)] <- 0

# TODO curar fenotips manuals

ds_otro <- gcat %>%
  select(
    entity_id,
    CANCER1_TIPO_OTRO,
    CANCER2_TIPO_OTRO,
    CANCER3_TIPO_OTRO
  ) %>%
  gather(
    key = N,
    value = CANCER,
    CANCER1_TIPO_OTRO, CANCER2_TIPO_OTRO, CANCER3_TIPO_OTRO
  ) %>%
  filter(
    !is.na(CANCER)
  ) %>%
  select(
    -N
  ) %>%
  unique()

ds %>% write_csv('output/check/cancer/data.csv')