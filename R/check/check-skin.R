library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    FOTOTIPO_PIEL,
    FOTOTIPO_QUEMADURAS_PRIMERA_EXPOSICION_SOLAR,
    FOTOTIPO_PROTECCION_SOLAR_BRONCEADO,
    FOTOTIPO_PECAS
  ) %>%
  mutate(
    skin_color = ifelse(FOTOTIPO_PIEL == 0, NA, FOTOTIPO_PIEL),
    skin_sensitivity_to_sun = ifelse(FOTOTIPO_QUEMADURAS_PRIMERA_EXPOSICION_SOLAR == 0, NA, FOTOTIPO_QUEMADURAS_PRIMERA_EXPOSICION_SOLAR),
    skin_sensitivity_to_sun_binary_1_5 = ifelse(FOTOTIPO_QUEMADURAS_PRIMERA_EXPOSICION_SOLAR %in% c(1), 1, ifelse(FOTOTIPO_QUEMADURAS_PRIMERA_EXPOSICION_SOLAR %in% c(5), 0, NA)),
    skin_sensitivity_to_sun_binary_12_5 = ifelse(FOTOTIPO_QUEMADURAS_PRIMERA_EXPOSICION_SOLAR %in% c(1,2), 1, ifelse(FOTOTIPO_QUEMADURAS_PRIMERA_EXPOSICION_SOLAR %in% c(5), 0, NA)),
    skin_sensitivity_to_sun_binary_12_45 = ifelse(FOTOTIPO_QUEMADURAS_PRIMERA_EXPOSICION_SOLAR %in% c(1,2), 1, ifelse(FOTOTIPO_QUEMADURAS_PRIMERA_EXPOSICION_SOLAR %in% c(4,5), 0, NA)),
    skin_tanning_without_protection = ifelse(FOTOTIPO_PROTECCION_SOLAR_BRONCEADO == 0, NA, FOTOTIPO_PROTECCION_SOLAR_BRONCEADO),
    freckling=ifelse(FOTOTIPO_PECAS == 0, NA, FOTOTIPO_PECAS),
    freckling_binary=ifelse(FOTOTIPO_PECAS %in% c(1,2,3,4), 1, ifelse(FOTOTIPO_PECAS %in% c(5), 0, NA))
  ) %>%
  select(
    entity_id,
    skin_color,
    skin_sensitivity_to_sun,
    skin_sensitivity_to_sun_binary_1_5,
    skin_sensitivity_to_sun_binary_12_5,
    skin_sensitivity_to_sun_binary_12_45,
    skin_tanning_without_protection,
    freckling,
    freckling_binary
  )

ds %>% write_csv('output/check/skin/data.csv')