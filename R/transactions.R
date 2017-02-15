library(dplyr)
library(plyr)
library(tidyr)

conditions <- read.csv('output/conditions/icd9.csv')
medication <- read.csv('output/medications/atc.csv')

gcat <- read.csv('output/data.csv', sep = ',', stringsAsFactors = TRUE)

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

directory <- '/home/labs/dnalab/xduran/Dropbox/gcatbiobank/frequent-itemset'
write.table(data, file.path(directory, 'data/conditions/data.csv'), row.names = FALSE, col.names = TRUE, quote = FALSE, sep = ',')

###

data <- data_wide %>%
  select(-ICD_250)

data_atc_a10 <- data_wide %>%
  select(-ICD_250, -ATC_A10)

values <- data_wide %>%
  select(
    id,
    ICD_250
  )

write.table(data_atc_a10, file.path(directory, 'data/conditions/data_a10.csv'), row.names = FALSE, col.names = TRUE, quote = FALSE, sep = ',')
write.table(values, file.path(directory, 'data/conditions/values.csv'), row.names = FALSE, col.names = TRUE, quote = FALSE, sep = ',')