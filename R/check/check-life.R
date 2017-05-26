library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    ESTADO_CIVIL,
    CONVIVENCIA_PAREJA,
    CONVIVENCIA_PAREJA_DIVORCIO,
    INGRESOS,
    CONVIVENCIA_HOGAR
  )

ds <- ds %>%
  mutate(
    civil_status = ifelse(ESTADO_CIVIL == 0, NA, ESTADO_CIVIL),
    civil_status_ever_married = ifelse(ESTADO_CIVIL %in% c(1), 0, ifelse(ESTADO_CIVIL %in% c(2,3,4,5), 1, NA)),
    civil_status_ever_divorced = ifelse(CONVIVENCIA_PAREJA_DIVORCIO %in% c(1,2), 1, ifelse(CONVIVENCIA_PAREJA_DIVORCIO %in% c(3), 0, NA)),
    lives_in_couple = ifelse(CONVIVENCIA_PAREJA %in% c(1), 1, ifelse(CONVIVENCIA_PAREJA %in% c(2), 0, NA)),
    lives_alone = ifelse(CONVIVENCIA_HOGAR == 0, NA, ifelse(CONVIVENCIA_HOGAR >= 2, 0, 1))
  ) %>%
  select(
    entity_id,
    civil_status,
    civil_status_ever_married,
    civil_status_ever_divorced,
    lives_in_couple,
    lives_alone
  )

ds %>% write_csv('output/check/life/data.csv')
