library(tidyverse)
library(reshape2)

associations <- read_csv('output/check/icd9_atc/data.csv')
core <- read_csv('output/datasets/gcat-core/ids.csv')
odds <- read_csv('output/datasets/gcat-heritability/odds-ratio.csv') %>%
  mutate(
    diagnosis_code = as.character(diagnosis_code)
  )

diagnosis_or <- function(data, name) {
  
  associations_icd9 <- data %>%
    gather(
      key = "key", value = "value", -entity_id
    ) %>%
    filter(
      value > 0
    ) %>%
    mutate(
      diagnosis_code = substr(key, 1, 3)
    ) %>%
    select(
      -key
    ) %>%
    unique()
  
  associations_icd9_count <- associations_icd9 %>%
    group_by(diagnosis_code) %>%
    summarise(diagnosis_odds = sum(value)) %>%
    right_join(odds) %>%
    select(
      diagnosis,
      diagnosis_code,
      diagnosis_count,
      diagnosis_count_heritability,
      diagnosis_odds
    ) %>%
    unique() %>%
    arrange(desc(diagnosis_count)) %>%
    write_csv(sprintf('output/check/%s/or.csv', name))
  
  associations_icd9 <- associations_icd9 %>%
    spread(
      diagnosis_code,
      value
    )
  
  associations_icd9 <- data %>%
    select(entity_id) %>%
    left_join(associations_icd9)
  
  associations_icd9[is.na(associations_icd9)] <- 0
  
  associations_icd9 %>% write_csv(sprintf('output/check/%s/data.csv', name))
}

diagnosis_or(associations, 'icd9_or')
diagnosis_or(associations %>% filter(
  entity_id %in% core$entity_id
), 'icd9_or_heritability')