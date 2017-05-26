library(tidyverse)

directory <- 'smoking'

ds <- gcat %>%
  select(
    entity_id,
    FUMAR_PIPA_DIA_CANTIDAD,
    FUMAR_PICADURA_DIA_CANTIDAD,
    FUMAR_CIGARROS_DIA_CANTIDAD,
    FUMAR_CIGARRILLOS_DIA_CANTIDAD,
    FUMAR_100,
    FUMAR_ACTUALIDAD,
    FUMAR_CONTINUADA_FIN_EDAD,
    FUMAR_CONTINUADA_INICIO_EDAD,
    EDAD_ANOS,
    FUMAR_INICIO_EDAD,
    EXFUMADOR_PIPA_DIA_CANTIDAD,
    EXFUMADOR_PICADURA_DIA_CANTIDAD,
    EXFUMADOR_CIGARROS_DIA_CANTIDAD,
    EXFUMADOR_CIGARRILLO_DIA_CANTIDAD,
    FUMAR_PRIMERO_DIA
  )

ds <- ds %>%
  mutate(
    smoking_packs = (
      as.numeric(ifelse(is.na(FUMAR_PIPA_DIA_CANTIDAD), 0, FUMAR_PIPA_DIA_CANTIDAD)) * 2.5 +
        as.numeric(ifelse(is.na(FUMAR_PICADURA_DIA_CANTIDAD), 0, FUMAR_PICADURA_DIA_CANTIDAD)) +
        as.numeric(ifelse(is.na(FUMAR_CIGARROS_DIA_CANTIDAD), 0, FUMAR_CIGARROS_DIA_CANTIDAD)) +
        as.numeric(ifelse(is.na(FUMAR_CIGARRILLOS_DIA_CANTIDAD), 0, FUMAR_CIGARRILLOS_DIA_CANTIDAD))
    )/20,
    smoking_packs_exsmoker = (
      as.numeric(ifelse(is.na(EXFUMADOR_PIPA_DIA_CANTIDAD), 0, EXFUMADOR_PIPA_DIA_CANTIDAD)) * 2.5 +
        as.numeric(ifelse(is.na(EXFUMADOR_PICADURA_DIA_CANTIDAD), 0, EXFUMADOR_PICADURA_DIA_CANTIDAD)) +
        as.numeric(ifelse(is.na(EXFUMADOR_CIGARROS_DIA_CANTIDAD), 0, EXFUMADOR_CIGARROS_DIA_CANTIDAD)) +
        as.numeric(ifelse(is.na(EXFUMADOR_CIGARRILLO_DIA_CANTIDAD), 0, EXFUMADOR_CIGARRILLO_DIA_CANTIDAD))
    )/20,
    smoking_habit =
      ifelse(
        is.na(FUMAR_100), NA,
        ifelse(
          FUMAR_100 == 2, 3,
          ifelse(
            is.na(FUMAR_ACTUALIDAD), NA,
            ifelse(FUMAR_ACTUALIDAD == 2, 2, 1)
          )
        )
      ),
    smoking_time = ifelse(
      is.na(FUMAR_100)
      | is.na(FUMAR_ACTUALIDAD)
      | is.na(FUMAR_CONTINUADA_FIN_EDAD)
      | is.na(FUMAR_CONTINUADA_INICIO_EDAD)
      | is.na(EDAD_ANOS),
      NA,
      ifelse(
        FUMAR_100 == 2,
        NA,
        ifelse(
          FUMAR_ACTUALIDAD == 2,
          FUMAR_CONTINUADA_FIN_EDAD - FUMAR_CONTINUADA_INICIO_EDAD,
          EDAD_ANOS - FUMAR_CONTINUADA_INICIO_EDAD
        )
      )
    ),
    smoking_cesation_time = EDAD_ANOS - FUMAR_CONTINUADA_FIN_EDAD,
    smoking_time_exsmoker = ifelse(
      is.na(FUMAR_100)
      | is.na(FUMAR_ACTUALIDAD)
      | is.na(FUMAR_CONTINUADA_FIN_EDAD)
      | is.na(FUMAR_CONTINUADA_INICIO_EDAD),
      NA,
      ifelse(
        FUMAR_100 == 2,
        NA,
        ifelse(
          FUMAR_ACTUALIDAD == 2,
          FUMAR_CONTINUADA_FIN_EDAD - FUMAR_CONTINUADA_INICIO_EDAD,
          NA
        )
      )
    ),
    smoking_time_smoker = ifelse(
      is.na(FUMAR_100)
      | is.na(FUMAR_ACTUALIDAD)
      | is.na(FUMAR_INICIO_EDAD)
      | is.na(EDAD_ANOS),
      NA,
      ifelse(
        FUMAR_100 == 2,
        NA,
        ifelse(
          FUMAR_ACTUALIDAD == 2,
          NA,
          EDAD_ANOS - FUMAR_INICIO_EDAD
        )
      )
    )
  ) %>%
  mutate(
    smoking_time = ifelse(smoking_time < 0, NA, smoking_time),
    smoking_time_exsmoker = ifelse(smoking_time_exsmoker < 0, NA, smoking_time_exsmoker),
    smoking_time_smoker = ifelse(smoking_time_smoker < 0, NA, smoking_time_smoker),
    smoking_cesation_time = ifelse(smoking_cesation_time < 0, NA, smoking_cesation_time)
  ) %>%
  mutate(
    smoking_packs_year_smoker = smoking_packs * smoking_time_smoker,
    smoking_packs_year_exsmoker = smoking_packs_exsmoker * smoking_time_exsmoker
  ) %>%
  mutate(
    smoking_packs_year =
      ifelse(
        is.na(smoking_packs_year_smoker),
        smoking_packs_year_exsmoker,
        ifelse(
          is.na(smoking_packs_year_exsmoker),
          smoking_packs_year_smoker,
          smoking_packs_year_exsmoker + smoking_packs_year_smoker
        )
      )
  ) %>%
  mutate(
    smoking_fagerstrom_score_first =
      ifelse(FUMAR_PRIMERO_DIA == 1, 3,
        ifelse(FUMAR_PRIMERO_DIA == 2, 2, 1
        )
      ),
    smoking_fagerstrom_score_quantity =
      ifelse(smoking_packs > 1.5, 3,
        ifelse(smoking_packs > 1, 2,
          ifelse(smoking_packs > 0.5, 1, 0
          )
        )
      )
  ) %>%
  mutate(
    smoking_fagerstrom_score = smoking_fagerstrom_score_first + smoking_fagerstrom_score_quantity
  ) %>%
  mutate(
    smoking_intensity =
      ifelse(smoking_habit == 1,
        ifelse(smoking_packs > 25/20, 6,
          ifelse(smoking_packs > 15/20, 5,
            ifelse(smoking_packs > 0, 4, 3
            )
          )
        ),
        ifelse(smoking_habit == 2,
          ifelse(smoking_cesation_time > 20, 10,
            ifelse(smoking_cesation_time > 10, 9,
              ifelse(smoking_cesation_time > 0, 8, 7
              )
            )
          ),
          ifelse(smoking_habit == 3, 1, NA)
        )
      )
  ) %>%
  mutate(
    smoking_status =
      ifelse(is.na(smoking_intensity), NA, ifelse(smoking_intensity == 1, 0, 1))
  ) %>%
  select(
    entity_id,
    smoking_packs,
    smoking_packs_exsmoker,
    smoking_habit,
    smoking_time,
    smoking_cesation_time,
    smoking_time_exsmoker,
    smoking_time_smoker,
    smoking_packs_year_smoker,
    smoking_packs_year_exsmoker,
    smoking_packs_year,
    smoking_fagerstrom_score,
    smoking_intensity,
    smoking_status
  )

ds %>% write_csv(sprintf('output/check/%s/data.csv', directory))
