library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    SEXO,
    CRIBADO_HOMBRES_HECES,
    CRIBADO_HOMBRES_PSA,
    CRIBADO_HOMBRES_NO,
    CRIBADO_MUJERES_CITOLOGIA,
    CRIBADO_MUJERES_HECES,
    CRIBADO_MUJERES_MAMOGRAFIA,
    CRIBADO_MUJERES_NO
  ) %>%
  mutate(
    screening_stool = ifelse(SEXO == 1, CRIBADO_HOMBRES_HECES, ifelse(SEXO == 2, CRIBADO_MUJERES_HECES, NA)),
    screening_psa = ifelse(SEXO == 1, CRIBADO_HOMBRES_PSA, FALSE),
    screening_cytology = ifelse(SEXO == 1, FALSE, ifelse(SEXO == 2, CRIBADO_MUJERES_CITOLOGIA, NA)),
    screening_mammography = ifelse(SEXO == 1, FALSE, ifelse(SEXO == 2, CRIBADO_MUJERES_MAMOGRAFIA, NA))
  ) %>%
  select(
    entity_id,
    screening_stool,
    screening_psa,
    screening_cytology,
    screening_mammography
  )

ds %>% write_csv('output/check/screening/data.csv')