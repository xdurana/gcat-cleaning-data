library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    FOTOTIPO_OJOS
  ) %>%
  mutate(
    eye_color = ifelse(FOTOTIPO_OJOS == 0, NA, FOTOTIPO_OJOS),
    eye_color_dark = ifelse(FOTOTIPO_OJOS %in% c(1,2,3), 1, ifelse(FOTOTIPO_OJOS %in% c(4,5), 0, NA))
  ) %>%
  select(
    entity_id,
    eye_color,
    eye_color_dark
  ) %>%
  mutate(
    eye_color_phototype_score = ifelse(is.na(eye_color), NA, as.integer(as.character(revalue(eye_color %>% as.factor, c("1" = "12", "2" = "8", "3" = "4", "4" = "2", "5" = "0")))))
  )

ds %>% write_csv('output/check/eyes/data.csv')
