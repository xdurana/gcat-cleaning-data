library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    PREDIMED_ACEITE_GRASA,
    PREDIMED_ACEITE_CONSUMO,
    PREDIMED_VERDURAS,
    PREDIMED_FRUTA,
    PREDIMED_CARNES_ROJAS,
    PREDIMED_MANTEQUILLA,
    PREDIMED_BEBIDAS_CARBONATADAS,
    PREDIMED_VINO,
    PREDIMED_LEGUMBRES,
    PREDIMED_PESCADO,
    PREDIMED_REPOSTERIA,
    PREDIMED_FRUTOS_SECOS,
    PREDIMED_CARNE,
    PREDIMED_VEGETALES
  )

ds <- ds %>%
  mutate(
    predimed_agrasa = ifelse(PREDIMED_ACEITE_GRASA == 0, NA, PREDIMED_ACEITE_GRASA %% 2),
    predimed_acons = ifelse(PREDIMED_ACEITE_CONSUMO == 0, NA, PREDIMED_ACEITE_CONSUMO %% 2),
    predimed_verd = ifelse(PREDIMED_VERDURAS == 0, NA, PREDIMED_VERDURAS %% 2),
    predimed_frut = ifelse(PREDIMED_FRUTA == 0, NA, PREDIMED_FRUTA %% 2),
    predimed_croja = ifelse(PREDIMED_CARNES_ROJAS == 0, NA, (PREDIMED_CARNES_ROJAS + 1) %% 2),
    predimed_mant = ifelse(PREDIMED_MANTEQUILLA == 0, NA, (PREDIMED_MANTEQUILLA + 1) %% 2),
    predimed_bcar = ifelse(PREDIMED_BEBIDAS_CARBONATADAS == 0, NA, PREDIMED_BEBIDAS_CARBONATADAS %% 2),
    predimed_vino = ifelse(PREDIMED_VINO == 0, NA, PREDIMED_VINO %% 2),
    predimed_leg = ifelse(PREDIMED_LEGUMBRES == 0, NA, PREDIMED_LEGUMBRES %% 2),
    predimed_pesc = ifelse(PREDIMED_PESCADO == 0, NA, PREDIMED_PESCADO %% 2),
    predimed_repost = ifelse(PREDIMED_REPOSTERIA == 0, NA, (PREDIMED_REPOSTERIA + 1) %% 2),
    predimed_fsecos = ifelse(PREDIMED_FRUTOS_SECOS == 0, NA, PREDIMED_FRUTOS_SECOS %% 2),
    predimed_carne = ifelse(PREDIMED_CARNE == 0, NA, (PREDIMED_CARNE + 1) %% 2),
    predimed_veg = ifelse(PREDIMED_VEGETALES == 0, NA, PREDIMED_VEGETALES %% 2)
  ) %>%
  mutate(
    predimed_score = predimed_veg + predimed_carne + predimed_fsecos + predimed_repost + predimed_pesc + predimed_leg + predimed_vino + predimed_bcar + predimed_mant + predimed_croja + predimed_frut + predimed_verd + predimed_acons + predimed_agrasa
  ) %>%
  mutate(
    predimed = cut(
      predimed_score,
      breaks = c(0, 5, 9, Inf),
      labels = c("low", "normal", "high"),
      right = FALSE
    )
  ) %>%
  mutate(
    predimed_high_adherence = ifelse(is.na(predimed), NA, ifelse(predimed == "high", 1, 0))
  )

  ds %>%
    select(
      entity_id,
      predimed_score,
      predimed,
      predimed_high_adherence
    ) %>%
    write_csv('output/check/predimed/data.csv')
  
  ds %>%
    select(
      entity_id,
      predimed_veg,
      predimed_carne,
      predimed_fsecos,
      predimed_repost,
      predimed_pesc,
      predimed_leg,
      predimed_vino,
      predimed_bcar,
      predimed_mant,
      predimed_croja,
      predimed_frut,
      predimed_verd,
      predimed_acons,
      predimed_agrasa,
      predimed_score,
      predimed,
      predimed_high_adherence
    ) %>%
    write_csv('output/check/predimed_detail/data.csv')
  