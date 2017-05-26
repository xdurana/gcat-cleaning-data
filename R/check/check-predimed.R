library(tidyverse)

directory <- 'predimed'

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
    pred_agrasa = ifelse(PREDIMED_ACEITE_GRASA == 0, NA, PREDIMED_ACEITE_GRASA %% 2),
    pred_acons = ifelse(PREDIMED_ACEITE_CONSUMO == 0, NA, PREDIMED_ACEITE_CONSUMO %% 2),
    pred_verd = ifelse(PREDIMED_VERDURAS == 0, NA, PREDIMED_VERDURAS %% 2),
    pred_frut = ifelse(PREDIMED_FRUTA == 0, NA, PREDIMED_FRUTA %% 2),
    pred_croja = ifelse(PREDIMED_CARNES_ROJAS == 0, NA, (PREDIMED_CARNES_ROJAS + 1) %% 2),
    pred_mant = ifelse(PREDIMED_MANTEQUILLA == 0, NA, (PREDIMED_MANTEQUILLA + 1) %% 2),
    pred_bcar = ifelse(PREDIMED_BEBIDAS_CARBONATADAS == 0, NA, PREDIMED_BEBIDAS_CARBONATADAS %% 2),
    pred_vino = ifelse(PREDIMED_VINO == 0, NA, PREDIMED_VINO %% 2),
    pred_leg = ifelse(PREDIMED_LEGUMBRES == 0, NA, PREDIMED_LEGUMBRES %% 2),
    pred_pesc = ifelse(PREDIMED_PESCADO == 0, NA, PREDIMED_PESCADO %% 2),
    pred_repost = ifelse(PREDIMED_REPOSTERIA == 0, NA, (PREDIMED_REPOSTERIA + 1) %% 2),
    pred_fsecos = ifelse(PREDIMED_FRUTOS_SECOS == 0, NA, PREDIMED_FRUTOS_SECOS %% 2),
    pred_carne = ifelse(PREDIMED_CARNE == 0, NA, (PREDIMED_CARNE + 1) %% 2),
    pred_veg = ifelse(PREDIMED_VEGETALES == 0, NA, PREDIMED_VEGETALES %% 2)
  ) %>%
  mutate(
    predimed_score = pred_veg + pred_carne + pred_fsecos + pred_repost + pred_pesc + pred_leg + pred_vino + pred_bcar + pred_mant + pred_croja + pred_frut + pred_verd + pred_acons + pred_agrasa
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
  ) %>%
  select(
    entity_id,
    predimed_score,
    predimed,
    predimed_high_adherence
  )

hist(ds$predimed_score)

ds %>% write_csv(sprintf('output/check/%s/data.csv', directory))