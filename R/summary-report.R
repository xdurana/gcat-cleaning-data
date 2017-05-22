library(tidyverse)
library(ggplot2)
library(xtable)
library(knitr)
library(psych)

source('R/summary-report-recruitment.R')

variables <- read_csv(file.path('output/check/heritability', 'variables.csv'))
heritability <- read_csv(file.path('output/check/heritability', 'data.csv'))

## Continuous variables

continuous_variables <- variables %>% filter(type == 'numeric') %>% select(name) %>% collect %>% .[['name']]
continuous <- heritability %>% select(one_of(continuous_variables))

desc <- describe(continuous, skew = FALSE)
desc <- desc %>%
  transform(
    missings = nrow(heritability) - n
  ) %>%
  select(
    -vars,
    -n,
    -range
  )

## Binary variables

variables <- variables %>%
  mutate(
    missings = sapply(variables$name, function(variable) {
      sum(is.na(heritability[variable]))
    }),
    v0 = sapply(variables$name, function(variable) {
      table(heritability[variable])[1]
    }),
    v1 = sapply(variables$name, function(variable) {
      table(heritability[variable])[2]
    })
  ) %>%
  mutate(
    p0 = round(v0*100/nrow(participants), digits = 2),
    p1 = round(v1*100/nrow(participants), digits = 2)
  )

binary <- variables %>%
  filter(type == 'binary') %>%
  filter(!(category %in% c('Medication', 'Conditions'))) %>%
  select(
    name,
    description,
    missings,
    v0,
    v1,
    p0,
    p1
  )

## Categorical variables

categorical_variables <- variables %>% filter(type == 'categorical') %>% select(name) %>% collect %>% .[['name']]
categorical <- heritability %>% select(one_of(categorical_variables))

## Conditions

conditions <- read_csv('output/conditions/summary.csv')

## Generate report

options(xtable.comment = FALSE)
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
rmarkdown::render('inst/reports/summary-report.Rmd', 'pdf_document')
