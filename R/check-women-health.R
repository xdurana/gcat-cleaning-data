library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    SALUD_MUJER_MENSTRUACION_PRIMERA_EDAD,
    SALUD_MUJER_MENSTRUACION_ULTIMA_EDAD,
    SALUD_MUJER_MEDICACION_ANTICONCEPTIVO,
    SALUD_MUJER_MEDICACION_HORMONAS
  ) %>%
  mutate(
    age_at_menarche = SALUD_MUJER_MENSTRUACION_PRIMERA_EDAD,
    age_at_menopause = SALUD_MUJER_MENSTRUACION_ULTIMA_EDAD,
    oc_use = ifelse(SALUD_MUJER_MEDICACION_ANTICONCEPTIVO %in% c(1), 1, ifelse(SALUD_MUJER_MEDICACION_ANTICONCEPTIVO %in% c(2), 0, NA)),
    hrt_use = ifelse(SALUD_MUJER_MEDICACION_HORMONAS %in% c(1,2,3), 1, ifelse(SALUD_MUJER_MEDICACION_HORMONAS %in% c(4), 0, NA))
  ) %>%
  select(
    entity_id,
    age_at_menarche,
    age_at_menopause,
    oc_use,
    hrt_use
  )

ds %>% write_csv('output/check/women-health/data.csv')