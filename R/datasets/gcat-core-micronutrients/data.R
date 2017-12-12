library(tidyverse)
library(openxlsx)

core_data <- read_csv('output/datasets/gcat-core/ids.csv') %>%
  select(
    -gender,
    -core_sample_name,
    -age,
    -year_of_recruitment
  ) %>%
  left_join(read_csv('output/datasets/gcat-imputed/data.csv'))

variables <-
  read_csv('output/datasets/gcat-imputed/variables.csv') %>%
  select(
    -only,
    -nas,
    -cases
  ) %>%
  arrange(
    category,
    name
  ) %>%
  filter(
    subarea %in% c('Nutrition', 'Lifestyle and health behaviours')
  )

variables %>% write_csv('output/datasets/gcat-core-micronutrients/variables.csv', na = "")
variables %>% as.data.frame %>% write.xlsx('output/datasets/gcat-core-micronutrients/variables.xlsx')

core_data_variables <- core_data %>%
  select(one_of(c('id', 'entity_id', 'gender', variables$name))) %>%
  unique()

core_data_variables %>% write_csv('output/datasets/gcat-core-micronutrients/data.csv', na = "")
core_data_variables %>% as.data.frame %>% write.xlsx('output/datasets/gcat-core-micronutrients/data.xlsx')
