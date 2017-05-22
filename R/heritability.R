library(tidyverse)
library(rjson)
library(gtools)

datasets = c(
  'age',
  'atc',
  'birth_weight',
  'bmi',
  'core',
  'eating',
  'education',
  'eyes',
  'follow-up',
  'gender',
  'hair',
  'handedness',
  'hr',
  'icd9_code3',
  'life',
  'offspring',
  'psychology',
  'self_perceived_health',
  'skin',
  'sleep',
  'whr',
  'women-health',
  'work'
)

rd <- function(dataset) {
  read_csv(file.path('output/check', dataset, 'data.csv'))
}

va <- function(dataset) {
  read_csv(file.path('output/check', dataset, 'variables.csv'))
}

ds <- rd('participants')
for(i in 1:length(datasets)) {
  print(datasets[i])
  ds <- ds %>% left_join(rd(datasets[i]))
}

ds %>% filter(status == 'COMPLETED') %>% select(-status) %>% write_csv('output/check/heritability/data.csv')

variables <- do.call(bind_rows, lapply(datasets, function(ds) {
  va(ds)
})) %>%
  unique()

variables <- variables %>%
  mutate(
    nas = sapply(variables$name, function(variable) {
      sum(is.na(ds[variable]))
    }) 
  )

variables %>% write_csv('output/check/heritability/variables.csv')


cases <- function(variable, type) {
  ifelse(type %in% c('binary'), min(table(heritability[variable])), NA)
}

controls <- function(variable, type) {
  ifelse(type %in% c('binary'), max(table(heritability[variable])), NA)
}

distribution <- function(variable, type) {
  ifelse(type %in% c('categorical', 'binary'), toJSON(table(heritability[variable])), NA)
}