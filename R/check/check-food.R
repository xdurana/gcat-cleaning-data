library(tidyverse)

ds <- gcat %>%
  select(
    entity_id,
    ALIMENTOS_CARNE_BLANCA,
    ALIMENTOS_CARNE_ROJA,
    ALIMENTOS_PESCADOS,
    ALIMENTOS_PASTA,
    ALIMENTOS_PATATA,
    ALIMENTOS_PAN,
    ALIMENTOS_VERDURA,
    ALIMENTOS_LEGUMBRE,
    ALIMENTOS_FRUTAS,
    ALIMENTOS_EMBUTIDOS,
    ALIMENTOS_DULCES,
    ALIMENTOS_LACTEOS,
    ALIMENTOS_HUEVOS,
    HABITO_CAFE,
    HABITO_TE
  )

ds <- ds %>%
  mutate(
    food_white_meat = ifelse(ALIMENTOS_CARNE_BLANCA == 0, NA, ALIMENTOS_CARNE_BLANCA),
    food_red_meat = ifelse(ALIMENTOS_CARNE_ROJA == 0, NA, ALIMENTOS_CARNE_ROJA),
    food_fish = ifelse(ALIMENTOS_PESCADOS == 0, NA, ALIMENTOS_PESCADOS),
    food_pasta = ifelse(ALIMENTOS_PASTA == 0, NA, ALIMENTOS_PASTA),
    food_potatoes = ifelse(ALIMENTOS_PATATA == 0, NA, ALIMENTOS_PATATA),
    food_bread = ifelse(ALIMENTOS_PAN == 0, NA, ALIMENTOS_PAN),
    food_vegetables = ifelse(ALIMENTOS_VERDURA == 0, NA, ALIMENTOS_VERDURA),
    food_legumes = ifelse(ALIMENTOS_LEGUMBRE == 0, NA, ALIMENTOS_LEGUMBRE),
    food_fruit = ifelse(ALIMENTOS_FRUTAS == 0, NA, ALIMENTOS_FRUTAS),
    food_cold_meat = ifelse(ALIMENTOS_EMBUTIDOS == 0, NA, ALIMENTOS_EMBUTIDOS),
    food_sweets = ifelse(ALIMENTOS_DULCES == 0, NA, ALIMENTOS_DULCES),
    food_dairy = ifelse(ALIMENTOS_LACTEOS == 0, NA, ALIMENTOS_LACTEOS),
    food_eggs = ifelse(ALIMENTOS_HUEVOS == 0, NA, ALIMENTOS_HUEVOS),
    coffee_intake = ifelse(HABITO_CAFE %in% c(1), 1, ifelse(HABITO_CAFE %in% c(2), 0, NA)),
    tea_intake = ifelse(HABITO_TE %in% c(1), 1, ifelse(HABITO_TE %in% c(2), 0, NA))
  ) %>%
  select(
    entity_id,
    food_white_meat,
    food_red_meat,
    food_fish,
    food_pasta,
    food_potatoes,
    food_bread,
    food_vegetables,
    food_legumes,
    food_fruit,
    food_cold_meat,
    food_sweets,
    food_dairy,
    food_eggs,
    coffee_intake,
    tea_intake
  )

ds %>% write_csv('output/check/food/data.csv')
