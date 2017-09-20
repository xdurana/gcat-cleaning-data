library(tidyverse)
library(plyr)

directory <- 'alcohol'

ds <- gcat %>%
  select(
    entity_id,
    SEXO,
    EXALCOHOL_30,
    EXALCOHOL_40,
    EXALCOHOL_50,
    EXALCOHOL_60,
    EXALCOHOL_FRECUENCIA,
    EXALCOHOL_NO,
    EXALCOHOL_NS_NC,
    ALCOHOL_ACTUAL,
    ALCOHOL_ACTUAL_COMIDA,
    ALCOHOL_ACTUAL_COMIDA_BLANCO_ROSADO,
    ALCOHOL_ACTUAL_COMIDA_TINTO,
    ALCOHOL_ACTUAL_COMIDA_CERVEZA,
    ALCOHOL_ACTUAL_COMIDA_CAVA,
    ALCOHOL_ACTUAL_COMIDA_VERMUT,
    ALCOHOL_ACTUAL_COMIDA_LICOR
  )

ds <- ds %>%
  mutate(
    alcohol_actual = ifelse(ALCOHOL_ACTUAL %in% c(1), 0, ifelse(ALCOHOL_ACTUAL %in% c(2,3,4,5,6,7,8), 1, NA)),
    alcohol_actual_white_rose_wine = as.numeric(ifelse(ALCOHOL_ACTUAL_COMIDA_BLANCO_ROSADO == 0, NA, revalue(as.character(ALCOHOL_ACTUAL_COMIDA_BLANCO_ROSADO), c("1" = "0", "2" = "2", "3" = "6", "4" = "14", "5" = "22", "6" = "30", "7" = "75", "8" = "120")))),
    alcohol_actual_red_wine = as.numeric(ifelse(ALCOHOL_ACTUAL_COMIDA_TINTO == 0, NA, revalue(as.character(ALCOHOL_ACTUAL_COMIDA_TINTO), c("1" = "0", "2" = "2", "3" = "6", "4" = "14", "5" = "22", "6" = "30", "7" = "75", "8" = "120")))),
    alcohol_actual_beer = as.numeric(ifelse(ALCOHOL_ACTUAL_COMIDA_CERVEZA == 0, NA, revalue(as.character(ALCOHOL_ACTUAL_COMIDA_CERVEZA), c("1" = "0", "2" = "2", "3" = "6", "4" = "14", "5" = "22", "6" = "30", "7" = "75", "8" = "120")))),
    alcohol_actual_cava = as.numeric(ifelse(ALCOHOL_ACTUAL_COMIDA_CAVA == 0, NA, revalue(as.character(ALCOHOL_ACTUAL_COMIDA_CAVA), c("1" = "0", "2" = "2", "3" = "6", "4" = "14", "5" = "22", "6" = "30", "7" = "75", "8" = "120")))),
    alcohol_actual_vermouth = as.numeric(ifelse(ALCOHOL_ACTUAL_COMIDA_VERMUT == 0, NA, revalue(as.character(ALCOHOL_ACTUAL_COMIDA_VERMUT), c("1" = "0", "2" = "2", "3" = "6", "4" = "14", "5" = "22", "6" = "30", "7" = "75", "8" = "120")))),
    alcohol_actual_spirits = as.numeric(ifelse(ALCOHOL_ACTUAL_COMIDA_LICOR == 0, NA, revalue(as.character(ALCOHOL_ACTUAL_COMIDA_LICOR), c("1" = "0", "2" = "4", "3" = "12", "4" = "28", "5" = "44", "6" = "60", "7" = "150", "8" = "240"))))
  ) %>%
  mutate(
    alcohol_ubes = alcohol_actual_white_rose_wine +
      alcohol_actual_beer +
      alcohol_actual_red_wine +
      alcohol_actual_cava +
      alcohol_actual_vermouth +
      alcohol_actual_spirits
  ) %>%
  mutate(
    alcohol = ifelse(SEXO == 1, ifelse(alcohol_ubes <= 28, 0, 1), ifelse(alcohol_ubes <= 14, 0, 1))
  ) %>%
  select(
    entity_id,
    alcohol_actual,
    alcohol_actual_white_rose_wine,
    alcohol_actual_red_wine,
    alcohol_actual_beer,
    alcohol_actual_cava,
    alcohol_actual_vermouth,
    alcohol_actual_spirits,
    alcohol_ubes,
    alcohol
  )

ds %>% write_csv(sprintf('output/check/%s/data.csv', directory))