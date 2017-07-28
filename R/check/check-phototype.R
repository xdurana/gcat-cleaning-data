library(tidyverse)

ds <- read_csv('output/check/skin/data.csv') %>%
  left_join(read_csv('output/check/ethnic-group/data.csv')) %>%
  left_join(read_csv('output/check/hair/data.csv')) %>%
  left_join(read_csv('output/check/eyes/data.csv')) %>%
  select(
    entity_id,
    skin_sensitivity_to_sun_phototype_score,
    skin_tanning_without_protection_phototype_score,
    skin_color_phototype_score,
    hair_color_phototype_score,
    eye_color_phototype_score,
    freckling_phototype_score,
    ethnic_group_phototype_score
  ) %>%
  mutate(
    phototype_score =
      skin_sensitivity_to_sun_phototype_score +
      skin_tanning_without_protection_phototype_score +
      skin_color_phototype_score +
      hair_color_phototype_score +
      eye_color_phototype_score +
      freckling_phototype_score +
      ethnic_group_phototype_score
  ) %>%
  select(
    entity_id,
    phototype_score
  )

ds %>% write_csv('output/check/phototype/data.csv')