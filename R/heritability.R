library(tidyverse)

ids <- read_csv('inst/extdata/heritability/sample_name_GCAT_core.txt')
all <- read_csv('output/check/imputation/imputed.csv')
variables <- read_csv('output/check/imputation/variables.csv')

ids_all <-
  ids %>%
  left_join(all) %>%
  group_by(entity_id) %>%
  arrange(core_extraction_plate) %>%
  filter(row_number()==n()) %>%
  ungroup()

ids_all %>% write_csv('output/check/heritability/data.csv')
variables %>% write_csv('output/check/heritability/variables.csv')

which(is.na(ids_all$age))