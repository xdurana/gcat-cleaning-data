library(xlsx)
library(tidyverse)

directory <- 'hemoglobin'
directory_hemoglobina <- '/home/labs/dnalab/share/lims/GCATbiobank Laboratori/GCAT/HEMOGLOBINA RESULTS'

ds <- read.xlsx2(file.path(directory_hemoglobina, 'report.xlsx'), sheetIndex = 1)
ds <- ds %>%
  dplyr::rename(
    entity_id=Sample.Id
  ) %>%
  transform(
    entity_id=sprintf('=%s', entity_id)
  )

ds <- ds %>%
  mutate(
    eAG = gsub(',', '.', as.character(eAG))
  ) %>%
  mutate(
    eAG = gsub('<97 mg/dL', '94 mg/dL', as.character(eAG))
  ) %>%
  mutate(
    eAG = gsub('>298 mg/dL', '298 mg/dL', as.character(eAG))
  )

ds$eAG %>% table

ds <- ds %>%
  mutate(
    hemoglobin_HbA1c_result = as.numeric(substr(as.character(X.HbA1c.Result), 1, 3)),
    hemoglobin_eAG = as.numeric(gsub('mg/dL', '', eAG))
  ) %>%
  select(
    entity_id,
    hemoglobin_HbA1c_result,
    hemoglobin_eAG
  )

ds %>% write_csv(sprintf('output/check/%s/data.csv', directory))
