library(tidyverse)
library(medicalrisk)

conditions <- read_csv('output/check/icd9/data.csv') %>%
  gather(condition, case, icd9_0119:icd9_V83) %>%
  filter(case > 0) %>%
  mutate(icd9 = as.factor(gsub('icd9_', 'D', condition))) %>%
  select(
    entity_id,
    icd9
  )

conditions <- conditions %>%
  merge(icd9cm_charlson_quan(levels(conditions$icd9)), by.x="icd9", by.y="row.names", all.x=TRUE)

conditions <- conditions %>%
  group_by(entity_id) %>%
  mutate_if(
    is.logical,
    funs(sum(.) > 0)
  ) %>%
  ungroup() %>%
  select(
    -icd9
  ) %>%
  unique()

charlson <-
  conditions %>%
  left_join(
    generate_charlson_index_df(conditions, idvar = 'entity_id')
  )
