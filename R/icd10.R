library(icdcoder)
library(tidyverse)
require(data.table)

icd9 <- read_csv('inst/extdata/conditions/summary-cim9-updated.csv') %>%
  filter(
    Freq > 1
  )

icd9$icd10 <- sapply((icd9)$Codi, function(codi) {
  (convICD(codi, 'icd9')$icd10 %>% head(1)) %>% as.character
})

icd9 %>%
  filter(
    is.na(Codi)
  ) %>%
  View()