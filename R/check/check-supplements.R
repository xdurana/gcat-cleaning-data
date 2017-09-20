library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    SUPLEMENTOS.MULTIVITAMINICOS,
    SUPLEMENTOS.VITAMINA_A,
    SUPLEMENTOS.VITAMINA_B6,
    SUPLEMENTOS.VITAMINA_B12,
    SUPLEMENTOS.COMPLEJO_VITAMINA_B,
    SUPLEMENTOS.VITAMINA_C,
    SUPLEMENTOS.VITAMINA_D,
    SUPLEMENTOS.VITAMINA_E,
    SUPLEMENTOS.CALCIO,
    SUPLEMENTOS.CALCIO_VITAMINA_D,
    SUPLEMENTOS.ACIDO_FOLICO,
    SUPLEMENTOS.OMEGA_3,
    SUPLEMENTOS.HIERRO,
    SUPLEMENTOS.SELENIO,
    SUPLEMENTOS.ZINC,
    SUPLEMENTOS.GLUCOSAMINA,
    SUPLEMENTOS.LINO,
    SUPLEMENTOS.SERENOA,
    SUPLEMENTOS.AJO
  ) %>%
  mutate(
    supplements_multivitaminics = ifelse(SUPLEMENTOS.MULTIVITAMINICOS == 0, FALSE, SUPLEMENTOS.MULTIVITAMINICOS),
    supplements_vitamin_a = ifelse(SUPLEMENTOS.VITAMINA_A == 0, FALSE, SUPLEMENTOS.VITAMINA_A),
supplements_vitamin_b6 = ifelse(SUPLEMENTOS.VITAMINA_B6 == 0, FALSE, SUPLEMENTOS.VITAMINA_B6),
    supplements_vitamin_b12 = ifelse(SUPLEMENTOS.VITAMINA_B12 == 0, FALSE, SUPLEMENTOS.VITAMINA_B12),
    supplements_complex_vitamin_b = ifelse(SUPLEMENTOS.COMPLEJO_VITAMINA_B == 0, FALSE, SUPLEMENTOS.COMPLEJO_VITAMINA_B),
    supplements_vitamin_c = ifelse(SUPLEMENTOS.VITAMINA_C == 0, FALSE, SUPLEMENTOS.VITAMINA_C),
    supplements_vitamin_d = ifelse(SUPLEMENTOS.VITAMINA_D == 0, FALSE, SUPLEMENTOS.VITAMINA_D),
    supplements_vitamin_e = ifelse(SUPLEMENTOS.VITAMINA_E == 0, FALSE, SUPLEMENTOS.VITAMINA_E),
    supplements_calcium = ifelse(SUPLEMENTOS.CALCIO == 0, FALSE, SUPLEMENTOS.CALCIO),
    supplements_calcium_vitamin_d = ifelse(SUPLEMENTOS.CALCIO_VITAMINA_D == 0, FALSE, SUPLEMENTOS.CALCIO_VITAMINA_D),
    supplements_folic_acid = ifelse(SUPLEMENTOS.ACIDO_FOLICO == 0, FALSE, SUPLEMENTOS.ACIDO_FOLICO),
    supplements_omega_3 = ifelse(SUPLEMENTOS.OMEGA_3 == 0, FALSE, SUPLEMENTOS.OMEGA_3),
    supplements_iron = ifelse(SUPLEMENTOS.HIERRO == 0, FALSE, SUPLEMENTOS.HIERRO),
    supplements_selenium = ifelse(SUPLEMENTOS.SELENIO == 0, FALSE, SUPLEMENTOS.SELENIO),
    supplements_zinc = ifelse(SUPLEMENTOS.ZINC == 0, FALSE, SUPLEMENTOS.ZINC),
    supplements_glucosamine = ifelse(SUPLEMENTOS.GLUCOSAMINA == 0, FALSE, SUPLEMENTOS.GLUCOSAMINA),
    supplements_flax = ifelse(SUPLEMENTOS.LINO == 0, FALSE, SUPLEMENTOS.LINO),
    supplements_serenoa = ifelse(SUPLEMENTOS.SERENOA == 0, FALSE, SUPLEMENTOS.SERENOA),
    supplements_garlic = ifelse(SUPLEMENTOS.AJO == 0, FALSE, SUPLEMENTOS.AJO)
  ) %>%
  select(
    entity_id,
    supplements_multivitaminics,
    supplements_vitamin_a,
    supplements_vitamin_b6,
    supplements_vitamin_b12,
    supplements_complex_vitamin_b,
    supplements_vitamin_c,
    supplements_vitamin_d,
    supplements_vitamin_e,
    supplements_calcium,
    supplements_calcium_vitamin_d,
    supplements_folic_acid,
    supplements_omega_3,
    supplements_iron,
    supplements_selenium,
    supplements_zinc,
    supplements_glucosamine,
    supplements_flax,
    supplements_serenoa,
    supplements_garlic
  )

ds %>% write_csv('output/check/supplements/data.csv')
