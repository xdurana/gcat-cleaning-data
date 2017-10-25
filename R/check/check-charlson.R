library(tidyverse)
library(medicalrisk)

icd9 <- read_csv('output/check/icd9_code3/data.csv')
icd9 %>%
  gather(condition, case, icd9_code3_011:icd9_code3_V83) %>%
  filter(case > 0) %>%
  mutate(condition = gsub('icd9_code3_', '', condition)) %>%
  select(-case)

icd9cm_charlson_quan(levels(cases$ICD9CM))