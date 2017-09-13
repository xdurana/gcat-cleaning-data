library(tidyverse)

directory <- 'men-health'

ds <- gcat %>%
  select(
    entity_id,
    SALUD_HOMBRES_ESTADO_DE_SALUD_PERFIL
  ) %>%
  mutate(
    physical_autoperception_men = ifelse(SALUD_HOMBRES_ESTADO_DE_SALUD_PERFIL == 0, NA, SALUD_HOMBRES_ESTADO_DE_SALUD_PERFIL)
  ) %>%
  select(
    entity_id,
    physical_autoperception_men
  )

ds %>% write_csv(sprintf('output/check/%s/data.csv', directory))