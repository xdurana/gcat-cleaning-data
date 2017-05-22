library(tidyverse)

heritability <- read_csv(file.path('output/check/heritability', 'data.csv'))
variables <- read_csv(file.path('output/check/heritability', 'variables.csv'))

vs <- c('entity_id', (variables %>% filter(type %in% c('binary', 'numeric')) %>% select(name))$name)

ds <- heritability %>%
  select(one_of(vs))

ds <- ds %>% select(-starts_with("atc_"))

ds[is.na(ds)] <- 0

names <- ds %>% select(-entity_id) %>% colnames

mylogit <- glm(icd9_code3_401 ~ ., data = ds, family = "binomial")
summary(mylogit)