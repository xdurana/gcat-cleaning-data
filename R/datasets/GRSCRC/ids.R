library(tidyverse)
library(xlsx)

ids_all <- read_csv('output/datasets/gcat/data.csv')
variables <- read_csv('output/datasets/gcat/variables.csv')

data <- ids_all %>%
  rownames_to_column() %>%
  mutate(
    id = sprintf("GR%.4d", as.numeric(rowname)),
    gender = ifelse(gender == 1, 'male', 'female'),
    is_genotyped = !is.na(core_sample_name)
  ) %>%
  select(
    id,
    entity_id,
    is_genotyped,
    gender,
    year_of_recruitment,
    age
  )

data %>% write_csv('output/datasets/GRSCRC/ids.csv')
data %>% as.data.frame %>% write.xlsx2('output/datasets/GRSCRC/ids.xlsx', row.names = FALSE)
variables %>% as.data.frame %>% write.xlsx2('output/datasets/GRSCRC/variables.xlsx', row.names = FALSE)
