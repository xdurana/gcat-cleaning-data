library(data.table)
library(dplyr)

padris <- fread('output/Seguiment PADRIS.csv') %>%
  rename(
    entity_id=id,
    gender=genere
  )

conditions <- padris %>%
  merge(fread('output/conditions/others_long.csv'))

medications <- padris %>%
  merge(fread('output/medications/data.csv'))

medications$CONDITION


library(icd)

table(conditions$condition)