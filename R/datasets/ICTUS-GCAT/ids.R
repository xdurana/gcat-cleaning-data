library(tidyverse)
library(xlsx)

source('R/datasets/ICTUS-GCAT/data.R')

variables <- read_csv('output/datasets/gcat/variables.csv')

data <- read_csv('output/datasets/gcat/data.csv') %>%
  rownames_to_column() %>%
  mutate(
    id = sprintf("IC%.4d", as.numeric(rowname)),
    gender = ifelse(gender == 1, 'male', 'female'),
    is_genotyped = !is.na(core_sample_name)
  ) %>%
  filter(
    is_genotyped
  ) %>%
  select(
    id,
    entity_id,
    age,
    gender,
    alcohol_actual,
    bmi_who_obesity,
    icd9_code3_410,
    icd9_code3_411,
    icd9_code3_414,
    icd9_code3_420,
    icd9_code3_427,
    family_history_icd9_code3_410,
    family_history_icd9_code3_414,
    smoking_habit,
    is_genotyped
  ) %>%
  mutate(
    family_history_icd9_code3_410 = as.integer(family_history_icd9_code3_410),
    family_history_icd9_code3_414 = as.integer(family_history_icd9_code3_414)
  )

variables <- variables %>% describe_var(data)
variables %>% as.data.frame %>% write.xlsx2('output/datasets/ICTUS-GCAT/variables.xlsx', row.names = FALSE)

data.any <- data %>% apply(1, function(r) any(r %in% c("icd9_code3_410")))

data %>%
  select(
    icd9_code3_410,
    icd9_code3_411,
    icd9_code3_414,
    icd9_code3_420,
    icd9_code3_427,
    family_history_icd9_code3_410,
    family_history_icd9_code3_414
  ) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  mutate(any = rowSums(.)) %>%
  filter(any == 0) %>%
  nrow()
  

data %>%
  filter(
    any(
      icd9_code3_410,
      icd9_code3_411,
      icd9_code3_414,
      icd9_code3_420,
      icd9_code3_427,
      family_history_icd9_code3_410,
      family_history_icd9_code3_414
    )
  ) %>%
  nrow()


data %>%
  select(
    -entity_id
  ) %>%
  write_csv('output/datasets/ICTUS-GCAT/data.csv')

data %>%
  select(
    -entity_id
  ) %>%
  as.data.frame %>% write.xlsx2('output/datasets/ICTUS-GCAT/data.xlsx', row.names = FALSE)

data %>%
  select(
    id,
    entity_id,
    is_genotyped,
    gender,
    age
  ) %>%
  as.data.frame %>% write.xlsx2('output/datasets/ICTUS-GCAT/ids.xlsx', row.names = FALSE)
