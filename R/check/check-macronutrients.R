library(tidyverse)
library(xlsx)

datasets <- c(
  'alcohol',
  'predimed_detail',
  'food'
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

ds %>% write_csv('output/check/macronutrients/data.csv') %>% unique()

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

variables %>% write_csv('output/check/macronutrients/variables.csv')
variables %>% write.xlsx2('output/check/macronutrients/variables.xlsx')
