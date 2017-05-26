library(tidyverse)
library(mice)
library(VIM)

directory <- 'women-health'

ds <- gcat %>%
  select(
    entity_id,
    SALUD_MUJER_MENSTRUACION_PRIMERA_EDAD,
    SALUD_MUJER_MENSTRUACION_ULTIMA_EDAD,
    SALUD_MUJER_MEDICACION_ANTICONCEPTIVO,
    SALUD_MUJER_MEDICACION_HORMONAS
  ) %>%
  mutate(
    age_at_menarche = ifelse(SALUD_MUJER_MENSTRUACION_PRIMERA_EDAD > 25, NA, SALUD_MUJER_MENSTRUACION_PRIMERA_EDAD),
    age_at_menopause = SALUD_MUJER_MENSTRUACION_ULTIMA_EDAD,
    reproductive_life_span = SALUD_MUJER_MENSTRUACION_ULTIMA_EDAD - SALUD_MUJER_MENSTRUACION_PRIMERA_EDAD,
    oc_use = ifelse(SALUD_MUJER_MEDICACION_ANTICONCEPTIVO %in% c(1), 1, ifelse(SALUD_MUJER_MEDICACION_ANTICONCEPTIVO %in% c(2), 0, NA)),
    hrt_use = ifelse(SALUD_MUJER_MEDICACION_HORMONAS %in% c(1,2,3), 1, ifelse(SALUD_MUJER_MEDICACION_HORMONAS %in% c(4), 0, NA))
  ) %>%
  # Filter incoherent ages
  mutate(
    age_at_menarche = ifelse(reproductive_life_span > 0, age_at_menarche, NA),
    age_at_menopause = ifelse(reproductive_life_span > 0, age_at_menopause, NA),
    reproductive_life_span = ifelse(reproductive_life_span > 0, reproductive_life_span, NA)
  ) %>%
  select(
    entity_id,
    age_at_menarche,
    age_at_menopause,
    reproductive_life_span,
    oc_use,
    hrt_use
  )

ds %>% write_csv(sprintf('output/check/%s/data.csv', directory))

missings_plot(ds, directory)
pair_plot(ds %>% select(age_at_menarche, age_at_menopause), directory, 'menarche')
histogram_plot(ds, directory, 'reproductive_life_span')
