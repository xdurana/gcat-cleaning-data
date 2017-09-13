library(tidyverse)

directory <- 'alcohol'

ds <- gcat %>%
  select(
    entity_id,
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
    alcohol_actual = ifelse(ALCOHOL_ACTUAL %in% c(1), 0, ifelse(ALCOHOL_ACTUAL %in% c(2,3,4,5,6,7,8), 1, NA))
  ) %>%
  select(
    entity_id,
    alcohol_actual
  )

ds %>% write_csv(sprintf('output/check/%s/data.csv', directory))