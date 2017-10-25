library(tidyverse)
library(openxlsx)

hf_data <- read_csv('output/datasets/gcat-core/ids.csv') %>%
  select(
    -gender,
    -core_sample_name,
    -age,
    -year_of_recruitment
  ) %>%
  left_join(read_csv('output/datasets/gcat-imputed/data.csv'))

variables <- read.xlsx('/home/labs/dnalab/share/lims/GCAT Cessio_Colleccions/PI-2017-03 HEALTH FORECAST/PI-2017-03_Healthforecast_DOC9-Material-Data Sheet_v1.xlsx', sheet = 4) %>%
  filter(
    PI_2017_3_HF %in% c('code', 'yes')
  )

variables <-
  read_csv('output/datasets/gcat-imputed/variables.csv') %>%
  filter(
    name %in% variables$name
  ) %>%
  select(
    -only,
    -nas,
    -cases
  ) %>%
  arrange(
    category,
    name
  )

variables %>% write_csv('output/datasets/gcat-core/variables.csv', na = "")

hf_data_variables <- hf_data %>%
  select(one_of(c('id', variables$name))) %>%
  unique()

hf_data_variables %>% write_csv('output/datasets/gcat-core/data.csv', na = "")
hf_data_variables %>% as.data.frame %>% write.xlsx('output/projects/gcat-core/data.xlsx')
