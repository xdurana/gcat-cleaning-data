library(tidyverse)
library(xlsx)

ids_all <- read_csv('output/datasets/heritability/data.csv')
participants <- read_csv('output/check/participants/data.csv')

hf_data <- ids_all %>%
  left_join(participants) %>%
  rownames_to_column() %>%
  mutate(
    id = sprintf("GR%.4d", as.numeric(rowname)),
    gender = ifelse(gender == 1, 'male', 'female')
  ) %>%
  select(
    id,
    entity_id,
    core_sample_name,
    gender,
    year_of_recruitment,
    age
  )

hf_data %>% write_csv('output/datasets/hf/ids.csv')
hf_data %>% as.data.frame %>% write.xlsx2('output/projects/hf/ids.xlsx', row.names = FALSE)