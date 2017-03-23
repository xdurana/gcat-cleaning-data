library(data.table)
library(dplyr)
library(plyr)
library(tidyr)

conditions <- fread('output/check/conditions/icd9.csv')
medication <- fread('output/check/medications/atc.csv')

gcat <- fread('/home/labs/dnalab/share/lims/R/gcat-cohort/output/export/GCAT/data.csv')

conditions_transaction <- conditions %>%
  transform(
    id=entity_id,
    value=sprintf("ICD_%s", Codi_3),
    count=1
  ) %>%
  select(
    id,
    value,
    count
  ) %>%
  unique()

medication_transaction <- medication %>%
  transform(
    value=substring(sprintf("ATC_%s", value), 1, 7),
    count = 1
  ) %>%
  select(
    id,
    value,
    count
  ) %>%
  unique()

#####

data_wide <- conditions_transaction %>%
  rbind(medication_transaction) %>%
  spread(value, count, fill = 0)

data <- data_wide

directory <- 'output/check/transactions'
write.table(data, file.path(directory, 'data.csv'), row.names = FALSE, col.names = TRUE, quote = FALSE, sep = ',')