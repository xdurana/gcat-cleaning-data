library(tidyverse)
library(plyr)

ds <- gcat %>%
  select(
    entity_id,
    FOTOTIPO_CABELLO,
    SALUD_HOMBRE_CALVICIE_20,
    SALUD_HOMBRE_CALVICIE_30,
    SALUD_HOMBRE_CALVICIE_40
  ) %>%
  mutate(
    hair_color = ifelse(FOTOTIPO_CABELLO == 0, NA, FOTOTIPO_CABELLO),
    hair_color_blond = ifelse(FOTOTIPO_CABELLO %in% c(4), 1, ifelse(FOTOTIPO_CABELLO %in% c(1,2,3,5), 0, NA)),
    hair_color_brown = ifelse(FOTOTIPO_CABELLO %in% c(2,3), 1, ifelse(FOTOTIPO_CABELLO %in% c(1,4,5), 0 , NA)),
    hair_color_red = ifelse(FOTOTIPO_CABELLO %in% c(5), 1, ifelse(FOTOTIPO_CABELLO %in% c(1,2,3,4), 0, NA)),
    hair_color_black = ifelse(FOTOTIPO_CABELLO %in% c(1), 1, ifelse(FOTOTIPO_CABELLO %in% c(2,3,4,5), 0, NA)),
    hair_color_light = ifelse(FOTOTIPO_CABELLO %in% c(4,5), 1, ifelse(FOTOTIPO_CABELLO %in% c(1,2,3), 0, NA)),
    hair_loss_20 = ifelse(SALUD_HOMBRE_CALVICIE_20 == 0, NA, SALUD_HOMBRE_CALVICIE_20),
    hair_loss_30 = ifelse(SALUD_HOMBRE_CALVICIE_30 == 0, NA, SALUD_HOMBRE_CALVICIE_30),
    hair_loss_40 = ifelse(SALUD_HOMBRE_CALVICIE_40 == 0, NA, SALUD_HOMBRE_CALVICIE_40),
    hair_loss_20_binary = ifelse(SALUD_HOMBRE_CALVICIE_20 %in% c(1,2), 0, ifelse(SALUD_HOMBRE_CALVICIE_20 %in% c(3,4), 1, NA)),
    hair_loss_30_binary = ifelse(SALUD_HOMBRE_CALVICIE_30 %in% c(1,2), 0, ifelse(SALUD_HOMBRE_CALVICIE_30 %in% c(3,4), 1, NA)),
    hair_loss_40_binary = ifelse(SALUD_HOMBRE_CALVICIE_40 %in% c(1,2), 0, ifelse(SALUD_HOMBRE_CALVICIE_40 %in% c(3,4), 1, NA))
  ) %>%
  select(
    entity_id,
    hair_color,
    hair_color_blond,
    hair_color_brown,
    hair_color_red,
    hair_color_black,
    hair_color_light,
    hair_loss_20,
    hair_loss_30,
    hair_loss_40,
    hair_loss_20_binary,
    hair_loss_30_binary,
    hair_loss_40_binary
  ) %>%
  mutate(
    hair_color_phototype_score = ifelse(is.na(hair_color), NA, as.integer(as.character(revalue(hair_color %>% as.factor, c("1" = "16", "2" = "8", "3" = "4", "4" = "2", "5" = "0")))))
  )


ds %>% write_csv('output/check/hair/data.csv')
