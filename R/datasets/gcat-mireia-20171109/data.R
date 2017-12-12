library(tidyverse)
library(openxlsx)

data <- read_csv('output/datasets/gcat-imputed/data.csv')

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
    subarea %in% c('Nutrition', 'Anthropometry', 'Sex/gender') |
    area %in% c('Socio-demographic and economic characteristics', 'Lifestyle and health behaviours')
  )

variables %>% write_csv('output/datasets/gcat-mireia-20171109/variables.csv', na = "")
variables %>% as.data.frame %>% write.xlsx('output/datasets/gcat-mireia-20171109/variables.xlsx')

data_variables <- data %>%
  select(one_of(c('entity_id', 'gender', variables$name))) %>%
  unique()

data_variables %>% write_csv('output/datasets/gcat-mireia-20171109/data.csv', na = "")
data_variables %>% as.data.frame %>% write.xlsx('output/datasets/gcat-mireia-20171109/data.xlsx')
