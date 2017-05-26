library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    SALUD_MENTAL_FELIZ,
    SALUD_MENTAL_NERVIOSO,
    SALUD_MENTAL_BAJAMORAL,
    SALUD_MENTAL_CALMA,
    SALUD_MENTAL_TRISTE,
    SALUD_MENTAL_INCAPACIDAD
  )

ds <- ds %>%
  mutate(
    happiness = ifelse(SALUD_MENTAL_FELIZ %in% c(1,2,3), 1, ifelse(SALUD_MENTAL_FELIZ %in% c(4,5,6), 0, NA)),
    sadness = ifelse(SALUD_MENTAL_TRISTE %in% c(1,2,3), 1, ifelse(SALUD_MENTAL_TRISTE %in% c(4,5,6), 0, NA)),
    upset = ifelse(SALUD_MENTAL_BAJAMORAL %in% c(1,2,3), 1, ifelse(SALUD_MENTAL_BAJAMORAL %in% c(4,5,6), 0, NA)),
    anxiety = ifelse(SALUD_MENTAL_NERVIOSO %in% c(1,2,3), 1, ifelse(SALUD_MENTAL_NERVIOSO %in% c(4,5,6), 0, NA)),
    calmness = ifelse(SALUD_MENTAL_CALMA %in% c(1,2,3), 1, ifelse(SALUD_MENTAL_CALMA %in% c(4,5,6), 0, NA)),
    incapacity = ifelse(SALUD_MENTAL_INCAPACIDAD %in% c(1,2,3), 1, ifelse(SALUD_MENTAL_INCAPACIDAD %in% c(4,5,6), 0, NA))
  ) %>%
  select(
    entity_id,
    happiness,
    sadness,
    upset,
    anxiety,
    calmness,
    incapacity
  )

ds %>% write_csv('output/check/psychology/data.csv')