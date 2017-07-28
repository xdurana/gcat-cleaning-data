library(tidyverse)

directory <- 'ethnic-group'

ds <- gcat %>%
  select(
    entity_id,
    ETNIA_PARTICIPANTE,
    ETNIA_PADRE,
    ETNIA_MADRE
  )

ds <- ds %>%
  mutate(
    ethnic_group = ETNIA_PARTICIPANTE,
    ethnic_group_father = ETNIA_PADRE,
    ethnic_group_mother = ETNIA_MADRE
  ) %>%
  select(
    entity_id,
    ethnic_group,
    ethnic_group_father,
    ethnic_group_mother
  ) %>%
  mutate(
    ethnic_group_phototype_score = ifelse(is.na(ethnic_group) | ethnic_group == 7, NA, as.integer(as.character(revalue(ethnic_group %>% as.factor, c("1" = "1", "2" = "12", "3" = "8", "4" = "4", "5" = "4", "6" = "8")))))
  )  

ds %>% write_csv(sprintf('output/check/%s/data.csv', directory))