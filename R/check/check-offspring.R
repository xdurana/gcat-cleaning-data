library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    FECHA_NACIMIENTO_ANO,
    SALUD_MUJER_HIJOS_1_EDAD,
    SALUD_HOMBRES_HIJOS,
    SALUD_MUJER_HIJOS,
    SALUD_MUJER_HIJOS_1_SEXO,
    SALUD_MUJER_HIJOS_2_SEXO,
    SALUD_MUJER_HIJOS_3_SEXO,
    SALUD_MUJER_HIJOS_4_SEXO,
    SALUD_MUJER_HIJOS_5_SEXO,
    SALUD_MUJER_HIJOS_6_SEXO,
    SALUD_MUJER_HIJOS_7_SEXO,
    SALUD_MUJER_HIJOS_8_SEXO,
    SALUD_MUJER_HIJOS_9_SEXO,
    SALUD_MUJER_HIJOS_10_SEXO
  ) %>%
  mutate(
    age_at_fist_birth = SALUD_MUJER_HIJOS_1_EDAD-FECHA_NACIMIENTO_ANO,
    offspring = ifelse(!is.na(SALUD_HOMBRES_HIJOS), SALUD_HOMBRES_HIJOS, ifelse(!is.na(SALUD_MUJER_HIJOS), SALUD_MUJER_HIJOS, 0))
  ) %>%
  mutate(
    offspring = ifelse(offspring == 11, 1, offspring)
  ) %>%
  mutate(
    offspring_binary = ifelse(offspring > 0, 1, 0)
  ) %>%
  mutate(
    offspring_male =
      ifelse(!is.na(SALUD_MUJER_HIJOS_1_SEXO) & SALUD_MUJER_HIJOS_1_SEXO == 1, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_2_SEXO) & SALUD_MUJER_HIJOS_2_SEXO == 1, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_3_SEXO) & SALUD_MUJER_HIJOS_3_SEXO == 1, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_4_SEXO) & SALUD_MUJER_HIJOS_4_SEXO == 1, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_5_SEXO) & SALUD_MUJER_HIJOS_5_SEXO == 1, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_6_SEXO) & SALUD_MUJER_HIJOS_6_SEXO == 1, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_7_SEXO) & SALUD_MUJER_HIJOS_7_SEXO == 1, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_8_SEXO) & SALUD_MUJER_HIJOS_8_SEXO == 1, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_9_SEXO) & SALUD_MUJER_HIJOS_9_SEXO == 1, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_10_SEXO) & SALUD_MUJER_HIJOS_10_SEXO == 1, 1, 0)
  ) %>%
  mutate(
    offspring_female =
      ifelse(!is.na(SALUD_MUJER_HIJOS_1_SEXO) & SALUD_MUJER_HIJOS_1_SEXO == 2, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_2_SEXO) & SALUD_MUJER_HIJOS_2_SEXO == 2, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_3_SEXO) & SALUD_MUJER_HIJOS_3_SEXO == 2, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_4_SEXO) & SALUD_MUJER_HIJOS_4_SEXO == 2, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_5_SEXO) & SALUD_MUJER_HIJOS_5_SEXO == 2, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_6_SEXO) & SALUD_MUJER_HIJOS_6_SEXO == 2, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_7_SEXO) & SALUD_MUJER_HIJOS_7_SEXO == 2, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_8_SEXO) & SALUD_MUJER_HIJOS_8_SEXO == 2, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_9_SEXO) & SALUD_MUJER_HIJOS_9_SEXO == 2, 1, 0) +
      ifelse(!is.na(SALUD_MUJER_HIJOS_10_SEXO) & SALUD_MUJER_HIJOS_10_SEXO == 2, 1, 0)
  ) %>%
  select(
    entity_id,
    offspring,
    offspring_binary,
    offspring_male,
    offspring_female,
    age_at_fist_birth
  )

ds %>% write_csv('output/check/offspring/data.csv')