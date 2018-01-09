library(tidyverse)
library(rjson)
library(gtools)
library(xlsx)

datasets = c(
  'age',
  'alcohol',
  'atc',
  'birth_weight',
  'bmi',
  'cancer',
  'core',
  'education',
  'eyes',
  'ethnic-group',
  'family',
  'follow-up',
  'food',
  'gender',
  'hair',
  'handedness',
  'hemoglobin',
  'hr',
  'icd9_code3',
  'income',
  'intake',
  'life',
  'men-health',
  'offspring',
  'phototype',
  'participants',
  'predimed',
  'predimed_detail',
  'psychology',
  'screening',
  'self_perceived_health',
  'skin',
  'sleep',
  'smoking',
  'supplements',
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

ds %>% write_csv('output/datasets/gcat/data.csv') %>% unique()

variables <- do.call(bind_rows, lapply(datasets, function(ds) {
  va(ds)
})) %>%
  unique()

variables <-
  variables %>%
  filter(
    name %in% colnames(ds)
  )

variables <-
  variables %>% 
  mutate(
    nas = sapply(variables$name, function(variable) {
      sum(is.na(ds[variable]))
    }),
    cases = ifelse(type %in% c('binary'),
                   sapply(variables$name, function(variable) {
                     table(ds[variable])[2]
                   }),
                   NA
    )
  )

variables %>% write_csv('output/datasets/gcat/variables.csv')
variables %>% write.xlsx2('output/datasets/gcat/variables.xlsx')
